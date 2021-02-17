{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module FromPDF where

import Control.Monad (when)
import qualified Data.Text as Text
import qualified Data.List as Lis
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Network.HTTP.Simple as HT
import Data.Aeson
import Network.HTTP.Client
import Data.Text.ICU.Convert as ICU
import qualified Data.ByteString.Lazy as BL
import Lib (iVecFromFile, iVecFromFileJP, oVecToFile, delimitAtWO2, delimitAtWO2By, vNub, takeFst, takeSnd, takeFstT, takeSndT, takeThdT, execShell, delimitAtW2, getHistogram, getDivideLine, vSortBy, fstT, sndT, thdT, takeFstT, takeSndT, takeThdT, fstQ, sndQ, thdQ, forQ, takeFstQ, takeSndQ, takeThdQ, takeForQ, indexingL)
import PopSExp (indexingSP, forgetSExp, injectSExpI, reconsSExp, showSP, mapNode, isBottomBy, takeSpecTags)
import ParserP (parse, pSExp)
import SExp
import System.Process
import Data.Algorithm.Diff
import Data.Time
import System.Timeout
import System.Environment
import System.Directory
import qualified Turtle
import qualified Control.Foldl as Fold
import Text.Show.Unicode
import Data.GI.Base
import qualified GI.Poppler as GPop
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.ForeignPtr (withForeignPtr)
import System.Info
import qualified Control.Exception as E
import Data.Text.Encoding.Error
import ForGisho (JTag (CJTag), jTag, jTag1, jTagIsNP, foldNPsTest)




-- for Stanford test
tesStan = do
  let
    engSens = "It is used here as a witness that the function is terminating."
  stanRess <- stanIOPoppy1 engSens
  let
    sexps = map parseStanford stanRess
  showSP $ sexps !! 0

parseStanford :: Turtle.Text -> SExp Tag String
parseStanford res = sexp
  where
    reshaped = reshapeSexp $ V.fromList $ map Text.unpack $ Text.split (== '\n')  res
    parsed = parse pSExp reshaped
    sexp =  modifyLastNPConj $ modifyPossession $ fst $ head $ parse pSExp reshaped

{-
data JTag = CJTag {
    jTag :: String
  , jTag1 :: String
  , jTagIsNP :: Bool
 }
  deriving (Show, Eq, Ord, Read)
-}

-- 下のfoldNPsJPRecursiveは筆者がチューニングした関数です。
-- PDFで結果を確認する際は、このfoldNPsJPRecursiveをfoldNPsTest sexp0と置き換えてください
-- 下のコメントアウトを切り替えればいいです。(stack installも忘れずに)。

-- *************************************************************************


foldNPsJPRecursive :: SExp JTag String -> SExp JTag String
-- foldNPsJPRecursive sexp0 = foldNPsTest sexp0
foldNPsJPRecursive sexp0 = deepNounized
  where
    sexpUnitNPs = recFoldPhraseJP (
        foldVPsJP
      . foldNPsJP
      . foldSahenVP
      . foldVPAux) sexp0
    sentensize = recFoldPhraseJP (
          foldSameTags0
        . foldNPVPSensWOJoshiByWall
        . foldSameTags0
        . foldNPVPSensByWall
        . foldSameTags0
        . foldAdvNPVPWOJoshiSensByWall
        . foldSameTags0
        . foldAdvNPVPSensByWall)
    deepNounizeBetJoshi = recFoldPhraseJP (
        foldNPsJP
      . foldSameTags0
      . foldAdjNP
      . foldSameTags0
      . foldVPNPByWall
      . recFoldPhraseJP (
              sentensize
            . foldVPTeVP
            . foldSameTags0
            . foldNPAdvByWall
            . foldSameTags0
            . foldAdjVPJP
            . foldSameTags0
            . foldAdvAdjJP
            . foldSameTags0
            . foldAdvVPJP
            . foldSameTags0
            . foldVPAdvByWall
            . foldSameTags0
            . foldAdvAdvize))
    deepNounized = deepNounizeBetJoshi sexpUnitNPs


testSentense' = "本書はPDFリーダーを自身の手で作ることを通してプログラミング言語Haskellを学ぶ本の第一弾です。"

te2 = do
  mecabedRes <- getMecabed testSentense'
  let
    sexp0 = toSExpsJP mecabedRes
    res2  = foldNPsJPRecursive sexp0
    sexp1 = foldNPsJP $ foldSameTags0 sexp0
  -- showSP $ forgetNPSubs res2
  showSP $ mapNode (\x -> (jTag x, jTag1 x)) id sexp1



te n = do
  pairss <- V.sequence =<< V.map getMecabed <$> iVecFromFileJP "hogehoge.txt"
  let
    pairs = pairss  V.! n
    sexp0 = toSExpsJP pairs
    res2  = foldNPsJPRecursive sexp0
  showSP res2
  -- showSP $ forgetSubs res2

rootJTag = CJTag {jTag = "root", jTag1 = "root", jTagIsNP = False}
nilJTag  = CJTag {jTag = "nil", jTag1 = "nil", jTagIsNP = False}

foldAdjNP :: SExp JTag String -> SExp JTag String
foldAdjNP sexp0 = sexp1
  where
    foldFunc = foldSuccTags (newJTag, 2, f)
      where
        newJTag = CJTag "名詞" "形容詞名詞" True
        f xs = isHeadVerb && isLastAux
          where
            isHeadVerb = "形容詞" == (jTag $ getTagJP $ (!! 0) xs)
            isLastAux = "名詞" == (jTag $ getTagJP $ last xs)
    sexp1 = foldFunc sexp0

foldVPTeVP :: SExp JTag String -> SExp JTag String
foldVPTeVP sexp0 = sexp1
  where
    foldFunc = foldSuccTags (newJTag, 3, f)
      where
        newJTag = CJTag "動詞" "動詞て動詞" False
        f xs = isHeadVerb && isMidTe && isLastAux
          where
            isHeadVerb = "動詞" == (jTag $ getTagJP $ (!! 0) xs)
            isMidTe = (Atom (CJTag "助詞" "接続助詞" False) "て" )== ((!! 1) xs)
            isLastAux = "動詞" == (jTag $ getTagJP $ last xs)
    sexp1 = foldFunc sexp0

foldVPAux :: SExp JTag String -> SExp JTag String
foldVPAux sexp0 = sexp1
  where
    foldFunc = foldSuccTags (newJTag, 2, f)
      where
        newJTag = CJTag "動詞" "助動詞動詞" False
        f xs = isHeadVerb && isLastAux
          where
            isHeadVerb = "動詞" == (jTag $ getTagJP $ (!! 0) xs)
            isLastAux = "助動詞" == (jTag $ getTagJP $ last xs)
    sexp1 = foldFunc sexp0

foldSahenVP :: SExp JTag String -> SExp JTag String
foldSahenVP sexp0 = sexp1
  where
    foldFunc = foldSuccTags (newJTag, 2, f)
      where
        newJTag = CJTag "動詞" "サ変動詞" False
        f xs = isHeadSahen && isLastVerb
          where
            isHeadSahen = "サ変接続" == (jTag1 $ getTagJP $ (!! 0) xs)
            isLastVerb = "動詞" == (jTag $ getTagJP $ last xs)
    sexp1 = foldFunc sexp0

foldAdjNPByWall :: SExp JTag String -> SExp JTag String
foldAdjNPByWall Nil = Nil
foldAdjNPByWall (Atom a b) = (Atom a b)
foldAdjNPByWall (Opr tg sexps)
  | length sexps < 4 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ tail stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    newJTag = CJTag "名詞" "形容詞名詞" True
    tagsCond sexps0 = isWall0 && isAdj1 && isNoun2 && isWall3
       where
         isWall0 = isKigou || isConj　|| isJoshi
           where
             isKigou = "記号" == (jTag $ getTagJP $ sexps0 !! 0)
             isConj = "接続詞" == (jTag $ getTagJP $ sexps0 !! 0)
             isJoshi = "助詞" == (jTag $ getTagJP $ head sexps0)
         isAdj1 = "形容詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isNoun2 = "名詞" == (jTag $ getTagJP $ (sexps0 !! 2))
         isWall3 = isKigou || isConj　|| isJoshi || isAux
           where
             isKigou = "記号" == (jTag $ getTagJP $ sexps0 !! 3)
             isConj = "接続詞" == (jTag $ getTagJP $ sexps0 !! 3)
             isJoshi = "助詞" == (jTag $ getTagJP $ sexps0 !! 3)
             isAux = "助動詞" == (jTag $ getTagJP $ sexps0 !! 3)
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 3 sexps, []) dropped
       where
         dropped = drop 3 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], foldedOprFirst)
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped -- popped
             where
               isFirstCheck = (length st == 3) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 4
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ init $ tail st, last st]
               foldedOprFirst = [Opr newJTag $ init st, last st]
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)

foldVPNPByWall :: SExp JTag String -> SExp JTag String
foldVPNPByWall Nil = Nil
foldVPNPByWall (Atom a b) = (Atom a b)
foldVPNPByWall (Opr tg sexps)
  | length sexps < 4 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ tail stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    -- newJTag = CJTag "名詞" "動詞名詞" True
    newJTag = CJTag "名詞" "動詞名詞" False
    tagsCond sexps0 = isWall0 && isVerb1 && isNoun2 && isWall3
       where
         isWall0 = isKigou || isConj　|| isJoshi
           where
             isKigou = "記号" == (jTag $ getTagJP $ sexps0 !! 0)
             isConj = "接続詞" == (jTag $ getTagJP $ sexps0 !! 0)
             isJoshi = "助詞" == (jTag $ getTagJP $ head sexps0)
         isVerb1 = "動詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isNoun2 = "名詞" == (jTag $ getTagJP $ (sexps0 !! 2))
         isWall3 = isKigou || isConj　|| isJoshi || isAux
           where
             isKigou = "記号" == (jTag $ getTagJP $ sexps0 !! 3)
             isConj = "接続詞" == (jTag $ getTagJP $ sexps0 !! 3)
             isJoshi = "助詞" == (jTag $ getTagJP $ sexps0 !! 3)
             isAux = "助動詞" == (jTag $ getTagJP $ sexps0 !! 3)
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 3 sexps, []) dropped
       where
         dropped = drop 3 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], foldedOprFirst)
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped
             where
               isFirstCheck = (length st == 3) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 4
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ init $ tail st, last st]
               foldedOprFirst = [Opr newJTag $ init st, last st]
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)

foldAdvNPVPSensByWall :: SExp JTag String -> SExp JTag String
foldAdvNPVPSensByWall Nil = Nil
foldAdvNPVPSensByWall (Atom a b) = (Atom a b)
foldAdvNPVPSensByWall (Opr tg sexps)
  | length sexps < 6 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ init $ tail stEnd, last stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    newJTag = CJTag "動詞" "文" False
    tagsCond sexps0 = isWall0 && isAdv1 && isNoun2 && isKakuJo3 && (isVerb4 || isAdj4) && isWall5
       where
         isWall0 = isKigou || isConj　|| isTo || isConj2
           where
             isKigou = "記号" == (jTag $ getTagJP $ head sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ head sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (head sexps0)
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ head sexps0)
         isAdv1 = "副詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isNoun2 = "名詞" == (jTag $ getTagJP $ (sexps0 !! 2))
         isKakuJo3 =  elem (sexps0 !! 3) $ kakuJo ++ kakariJo
           where
             kakuJo = map (\c -> Atom (CJTag "助詞" "格助詞" False) c) ["が", "は"]
             kakariJo = map (\c -> Atom (CJTag "助詞" "係助詞" False) c) ["が", "は"]
         isVerb4 = "動詞" == (jTag $ getTagJP $ (sexps0 !! 4))
         isAdj4 = "形容詞" == (jTag $ getTagJP $ (sexps0 !! 4))
         isWall5 = isKigou || isConj　|| (isTo || isToIu) || isConj2 || isNP
           where
             isKigou = "記号" == (jTag $ getTagJP $ last sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ last sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (last sexps0)
             isToIu = checkIsTok "という" $ forgetSubs $ last sexps0
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ last sexps0)
             isNP = "名詞" == (jTag $ getTagJP $ last sexps0)
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 5 sexps, []) dropped
       where
         dropped = drop 5 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], foldedOprFirst)
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped -- popped
             where
               isFirstCheck = (length st == 5) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 6
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ init $ tail st, last st]
               foldedOprFirst = [Opr newJTag $ init st, last st]
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)

foldAdvNPVPWOJoshiSensByWall :: SExp JTag String -> SExp JTag String
foldAdvNPVPWOJoshiSensByWall Nil = Nil
foldAdvNPVPWOJoshiSensByWall (Atom a b) = (Atom a b)
foldAdvNPVPWOJoshiSensByWall (Opr tg sexps)
  | length sexps < 5 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ init $ tail stEnd, last stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    newJTag = CJTag "動詞" "文" False
    tagsCond sexps0 = isWall0 && isAdv1 && isNoun2 && (isVerb3 || isAdj3) && isWall4
       where
         isWall0 = isKigou || isConj　|| isTo || isConj2
           where
             isKigou = "記号" == (jTag $ getTagJP $ head sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ head sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (head sexps0)
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ head sexps0)
         isAdv1 = "副詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isNoun2 = "名詞" == (jTag $ getTagJP $ (sexps0 !! 2))
         isVerb3 = "動詞" == (jTag $ getTagJP $ (sexps0 !! 3))
         isAdj3 = "形容詞" == (jTag $ getTagJP $ (sexps0 !! 3))
         isWall4 = isKigou || isConj　|| (isTo || isToIu) || isConj2 || isNP
           where
             isKigou = "記号" == (jTag $ getTagJP $ last sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ last sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (last sexps0)
             isToIu = checkIsTok "という" $ forgetSubs $ last sexps0
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ last sexps0)
             isNP = "名詞" == (jTag $ getTagJP $ last sexps0)
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 4 sexps, []) dropped
       where
         dropped = drop 4 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], foldedOprFirst)
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped -- popped
             where
               isFirstCheck = (length st == 4) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 5
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ init $ tail st, last st]
               foldedOprFirst = [Opr newJTag $ init st, last st]
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)

checkIsTok :: String -> SExp JTag String -> Bool
checkIsTok tok sexp =  case sexp of
               Nil -> False
               Opr _ _ -> False
               Atom _ tk -> tk == tok

foldNPVPSensWOJoshiByWall :: SExp JTag String -> SExp JTag String
foldNPVPSensWOJoshiByWall Nil = Nil
foldNPVPSensWOJoshiByWall (Atom a b) = (Atom a b)
foldNPVPSensWOJoshiByWall (Opr tg sexps)
  | length sexps < 4 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ init $ tail stEnd, last stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    newJTag = CJTag "動詞" "文" False
    tagsCond sexps0 = isWall0 && isNoun1 && (isVerb2 || isAdj2) && isWall3
       where
         isWall0 = isKigou || isConj　|| isTo || isConj2
           where
             isKigou = "記号" == (jTag $ getTagJP $ head sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ head sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (head sexps0)
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ head sexps0)
         isNoun1 = "名詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isVerb2 = "動詞" == (jTag $ getTagJP $ (sexps0 !! 2))
         isAdj2 = "形容詞" == (jTag $ getTagJP $ (sexps0 !! 2))
         isWall3 = isKigou || isConj　|| (isTo || isToIu) || isConj2 || isNP
           where
             isKigou = "記号" == (jTag $ getTagJP $ last sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ last sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (last sexps0)
             isToIu = checkIsTok "という" $ forgetSubs $ last sexps0
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ last sexps0)
             isNP = "名詞" == (jTag $ getTagJP $ last sexps0)
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 3 sexps, []) dropped
       where
         dropped = drop 3 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], foldedOprFirst)
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped -- popped
             where
               isFirstCheck = (length st == 3) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 4
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ init $ tail st, last st]
               foldedOprFirst = [Opr newJTag $ init st, last st]
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)

foldNPVPSensByWall :: SExp JTag String -> SExp JTag String
foldNPVPSensByWall Nil = Nil
foldNPVPSensByWall (Atom a b) = (Atom a b)
foldNPVPSensByWall (Opr tg sexps)
  | length sexps < 5 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ init $ tail stEnd, last stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    newJTag = CJTag "動詞" "文" False
    tagsCond sexps0 = isWall0 && isNoun1 && isKakuJo2 && (isVerb3 || isAdj3) && isWall4
       where
         isWall0 = isKigou || isConj　|| isTo || isConj2
           where
             isKigou = "記号" == (jTag $ getTagJP $ head sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ head sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (head sexps0)
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ head sexps0)
         isNoun1 = "名詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isKakuJo2 =  elem (sexps0 !! 2) $ kakuJo ++ kakariJo
           where
             kakuJo = map (\c -> Atom (CJTag "助詞" "格助詞" False) c) ["が", "は"]
             kakariJo = map (\c -> Atom (CJTag "助詞" "係助詞" False) c) ["が", "は"]
         isVerb3 = "動詞" == (jTag $ getTagJP $ (sexps0 !! 3))
         isAdj3 = "形容詞" == (jTag $ getTagJP $ (sexps0 !! 3))
         isWall4 = isKigou || isConj　|| (isTo || isToIu) || isConj2 || isNP
           where
             isKigou = "記号" == (jTag $ getTagJP $ last sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ last sexps0)
             isTo = Atom (CJTag "助詞" "格助詞" False) "と" == (last sexps0)
             isToIu = checkIsTok "という" $ forgetSubs $ last sexps0
             isConj2 = "接続助詞" == (jTag1 $ getTagJP $ last sexps0)
             isNP = "名詞" == (jTag $ getTagJP $ last sexps0)
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 4 sexps, []) dropped
       where
         dropped = drop 4 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], foldedOprFirst)
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped -- popped
             where
               isFirstCheck = (length st == 4) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 5
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ init $ tail st, last st]
               foldedOprFirst = [Opr newJTag $ init st, last st]
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)


foldNPAdvByWall :: SExp JTag String -> SExp JTag String
foldNPAdvByWall Nil = Nil
foldNPAdvByWall (Atom a b) = (Atom a b)
foldNPAdvByWall (Opr tg sexps)
  | length sexps < 3 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ tail stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    newJTag = CJTag "副詞" "名詞副詞" False
    tagsCond sexps0 = isWall0 && (isNoun1 || isAdj1) && isKakuJo2
       where
         isWall0 = isKigou || isAdv || isConj || isHa || isGa
           where
             isKigou = "記号" == (jTag $ getTagJP $ head sexps0)
             isAdv = "副詞" == (jTag $ getTagJP $ head sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ head sexps0)
             isHa = Atom (CJTag "助詞" "係助詞" False) "は" == head sexps0
             isGa = Atom (CJTag "助詞" "格助詞" False) "が" == head sexps0
         isNoun1 = "名詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isAdj1 = "形容詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         -- isKakuJo2 =  (elem lastlast $ kakuJo ++ [kakuJo2, kakuJo3, kakuJo4, fukuJo]) || isComplex || checkIsTok "までに" (forgetSubs lastlast) || foldl (\y -> \x -> y || checkIsTok x forgotten) False complices
         isKakuJo2 =  (elem lastlast $ kakuJo ++ [kakuJo2, kakuJo3, kakuJo4, kakuJo5, kaJo, fukuJo]) || foldl (\y -> \x -> y || checkIsTok x forgotten) False complices
           where
             lastlast = sexps0 !! 2
             forgotten = forgetSubs lastlast
             kakuJo = map (\c -> Atom (CJTag "助詞" "格助詞" False) c) ["で", "に", "を", "では"]
             kakuJo2 = Atom (CJTag "助詞" "副詞化" False) "に"
             kakuJo3 = Atom (CJTag "助詞" "副助詞" False) "までに"
             kakuJo4 = Atom (CJTag "助詞" "係助詞" False) "も"
             kakuJo5 = Atom (CJTag "助詞" "係助詞" False) "は"
             kaJo = Atom (CJTag "助詞" "副助詞／並立助詞／終助詞" False) "か"
             fukuJo = Atom (CJTag "助詞" "副助詞" False) "でも"
             complices = ["では", "までに", "について", "については", "として"]
             isComplex = case forgetSubs lastlast of
               Nil -> False
               Opr _ _ -> False
               Atom _ tk -> tk == "では"
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 2 sexps, []) dropped
       where
         dropped = drop 2 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], [foldedOprFirst])
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped -- popped
             where
               isFirstCheck = (length st == 2) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 3
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ tail st]
               foldedOprFirst = Opr newJTag st
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)

cshowFG :: [SExp JTag String] -> IO ()
cshowFG = cshowIL . (map forgetSubs)

foldVPAdvByWall :: SExp JTag String -> SExp JTag String
foldVPAdvByWall Nil = Nil
foldVPAdvByWall (Atom a b) = (Atom a b)
foldVPAdvByWall (Opr tg sexps)
  | length sexps < 3 = (Opr tg sexps)
  | isNewTagOKEnd = Opr tg (plEnd ++ [head stEnd, Opr newJTag $ tail stEnd])
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    newJTag = CJTag "副詞" "動詞副詞" False
    tagsCond sexps0 = isWall0 && isNoun1 && isKakuJo2
       where
         isWall0 = isKigou || isAdv || isConj
           where
             isKigou = "記号" == (jTag $ getTagJP $ head sexps0)
             isAdv = "副詞" == (jTag $ getTagJP $ head sexps0)
             isConj = "接続詞" == (jTag $ getTagJP $ head sexps0)
         isNoun1 = "動詞" == (jTag $ getTagJP $ (sexps0 !! 1))
         isKakuJo2 =  elem (sexps0 !! 2) $ kakuJo
           where
             kakuJo = map (\c -> Atom (CJTag "助詞" "接続助詞" False) c) ["て", "ば"]
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take 2 sexps, []) dropped
       where
         dropped = drop 2 sexps
         f y@(st, pl) x
           | isFirstCheckOK = ([x], [foldedOprFirst])
           | isFirstCheck = poppedFirst
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], pl ++ foldedOpr) -- <- stack terminated
           | otherwise = popped -- popped
             where
               isFirstCheck = (length st == 2) && pl == []
               isFirstCheckOK = isFirstCheck && (tagsCond (dummy : st))
                 where
                   dummy = Atom (CJTag "記号" "読点" False) "、"
               isNotYet = length st < 3
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = [head st, Opr newJTag $ tail st]
               foldedOprFirst = Opr newJTag st
               popped = (snocL (tail st) x, snocL pl $ head st)
               poppedFirst = (snocL st x, pl)

recFoldPhraseJP :: Eq t => (t -> t) -> t -> t
recFoldPhraseJP phraseFunc sexp0
  | foldedOnce == sexp0 = sexp0
  | otherwise = recFoldPhraseJP phraseFunc foldedOnce
   where
     foldedOnce = phraseFunc sexp0

foldAdvAdvize :: SExp JTag String -> SExp JTag String
foldAdvAdvize sexp0 = sexp1
  where
    foldVPNP = foldSuccTags (newJTag, 2, f)
      where
        newJTag = CJTag "副詞" "副詞副詞" False
        f xs = isHeadAdv && isLastAdvize
          where
            isHeadAdv = "副詞" == (jTag $ getTagJP $ head xs)
            isLastAdvize = "副詞化" == (jTag1 $ getTagJP $ last xs)
    sexp1 = foldVPNP sexp0

foldAdjVPJP :: SExp JTag String -> SExp JTag String
foldAdjVPJP sexp0 = sexp1
  where
    foldVPNP = foldSuccTags (newJTag, 2, f)
      where
        newJTag = CJTag "動詞" "形容詞動詞" False
        f xs = isHeadNP && isLastVP
          where
            isHeadNP = "形容詞" == (jTag $ getTagJP $ head xs)
            isLastVP = "動詞" == (jTag $ getTagJP $ last xs)
    sexp1 = foldVPNP sexp0

foldAdvAdjJP :: SExp JTag String -> SExp JTag String
foldAdvAdjJP sexp0 = sexp1
  where
    foldVPNP = foldSuccTags (newJTag, 2, f)
      where
        newJTag = CJTag "形容詞" "副詞形容詞" False
        f xs = isHeadNP && isLastVP
          where
            isHeadNP = "副詞" == (jTag $ getTagJP $ head xs)
            isLastVP = "形容詞" == (jTag $ getTagJP $ last xs)
    sexp1 = foldVPNP sexp0

foldAdvVPJP :: SExp JTag String -> SExp JTag String
foldAdvVPJP sexp0 = sexp1
  where
    foldVPNP = foldSuccTags (newJTag, 2, f)
      where
        newJTag = CJTag "動詞" "副詞動詞" False
        f xs = isHeadNP && isLastVP
          where
            isHeadNP = "副詞" == (jTag $ getTagJP $ head xs)
            isLastVP = "動詞" == (jTag $ getTagJP $ last xs)
    sexp1 = foldVPNP sexp0

foldVPsJP :: SExp JTag String -> SExp JTag String
foldVPsJP sexp0 = sexp1
  where
    foldJoDoushi tag tag1 sexp = foldSuccTags (newJTag, 2, f) sexp
      where
        newJTag = CJTag tag tag1 False
        f xs = isHeadVerb && isLastJoDoushi
          where
            isHeadVerb = (CJTag tag tag1 False) == (getTagJP $ head xs)
            isLastJoDoushi = (CJTag "助動詞" "*" False) == (getTagJP $ last xs)
    foldNonAuto = foldSuccTags (newJTag, 3, f)
      where
        newJTag = CJTag "動詞" "自立" False
        f xs = isHeadAuto && isLastNonAuto
          where
            isHeadAuto = (CJTag "動詞" "自立" False) == (getTagJP $ head xs)
            isMidConnectJoshi = (Atom (CJTag "動詞" "自立" False) "て") == xs !! 1
            isLastNonAuto = (CJTag  "動詞" "非自立" False) == (getTagJP $ last xs)
    sexp1 = (foldNonAuto . (foldJoDoushi "形容詞" "自立") . (foldJoDoushi "動詞" "自立") . (foldJoDoushi "動詞" "非自立")) sexp0
    -- sexp1 = (foldJoDoushi "動詞" "非自立") sexp0

foldNPsJP :: SExp JTag String -> SExp JTag String
foldNPsJP sexp0 = sexp4
  where
    newJTag = CJTag {jTag = "名詞", jTag1 = "名詞名詞", jTagIsNP = True}
    sexp1 = foldPrefix sexp0
    sexp2 = foldSuffix sexp1
    sexp3 = foldSameTags0 sexp2
    sexp31 = foldSuccTags (newJTag, 2, f) sexp3
      where
        f xs = isHeadRentai && isLastNoun
          where
            isHeadRentai = (jTag $ getTagJP $ (head xs)) == "連体詞"
            isLastNoun = (jTag $ getTagJP $ (last xs)) == "名詞" || (jTag $ getTagJP $ (last xs)) == "代名詞"
    sexp4 = foldSuccTags (newJTag, 3, f) sexp31
      where
        f xs = isHeadNoun && isMidJoshiAndNo && isLastNoun
          where
            isHeadNoun = (jTag $ getTagJP $ (head xs)) == "名詞" || (jTag $ getTagJP $ (head xs)) == "代名詞"
            isMidJoshiAndNo = (Atom (CJTag "助詞" "連体化" False) "の") == (forgetSubs $ xs !! 1) || (Atom (CJTag "助動詞" "*" False) "な") == (forgetSubs $ xs !! 1) || (Atom (CJTag "助詞" "格助詞" False) "での") == (forgetSubs $ xs !! 1)
            isLastNoun = (jTag $ getTagJP $ (last xs)) == "名詞" || (jTag $ getTagJP $ (last xs)) == "代名詞"

-- isSuffix
-- isSuffixStacked

foldSuccTags :: (JTag, Int, [(SExp JTag String)] -> Bool) -> SExp JTag String -> SExp JTag String
foldSuccTags _ Nil = Nil
foldSuccTags _ (Atom a b) = (Atom a b)
foldSuccTags (newTag, lenOfCond, tagsCond) (Opr tg sexps)
  | length sexps < lenOfCond = Nil
  | isNewTagOKEnd = Opr tg (snocL plEnd $ Opr newTag stEnd)
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    isNewTagOKEnd = tagsCond stEnd
    res@(stEnd, plEnd)
      | otherwise = foldl f (take lenOfCond sexps, []) $ drop lenOfCond sexps
       where
         f y@(st, pl) x
           | isNotYet = (stackedAdded, pl) -- <- stack continue
           | isNewTagOK = ([x], snocL pl foldedOpr) -- <- stack terminated
           | otherwise = (snocL (tail st) x, snocL pl $ head st) -- popped
             where
               isNotYet = length st < lenOfCond
               stackedAdded = snocL st x
               isNewTagOK = tagsCond st
               foldedOpr = Opr newTag st

forgetSubs :: SExp JTag String -> SExp JTag String
forgetSubs Nil = Nil
forgetSubs (Atom a b) = (Atom a b)
forgetSubs (Opr tg sexps)
  | tg == rootJTag = (Opr tg $ map forgetSubs sexps)
  | otherwise = (Atom tg $ concatMap getSubTokens sexps)

forgetNPSubs :: SExp JTag String -> SExp JTag String
forgetNPSubs Nil = Nil
forgetNPSubs (Atom a b) = (Atom a b)
forgetNPSubs (Opr tg sexps)
  | tg == rootJTag = (Opr tg $ map forgetNPSubs sexps)
  | jTagIsNP tg == True = (Atom tg $ concatMap getSubTokens sexps)
  | otherwise = (Opr tg $ map forgetNPSubs sexps)

getSubTokens :: SExp JTag String -> String
getSubTokens Nil = ""
getSubTokens (Atom a b) = b
getSubTokens (Opr tg sexps) = concat (map getSubTokens sexps)

toSExpsJP :: V.Vector MData -> SExp JTag String
toSExpsJP mData = Opr rootJTag atoms
  where
    atoms = V.toList $ V.map (\x -> Atom (cNewTag x) (mToken x)) mData
       where
           cNewTag x = CJTag {jTag = mTag x, jTag1 = mTag1 x, jTagIsNP = isNP}
             where
               isNP
                | mTag x == "名詞" = True
                | otherwise = False

foldSameTags :: SExp JTag String -> SExp JTag String
foldSameTags Nil = Nil
foldSameTags (Atom a b) = (Atom a b)
foldSameTags (Opr tg sexps)
  | sexps == [] = Nil
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    res@(stEnd, plEnd)
      | otherwise = foldl f ([head sexps], []) $ tail sexps
       where
         f y@(st, pl) x
           | isNewTagSame = (snocL st x, pl) -- <- stack continue
           | isSameStacked = ([x], snocL pl foldedOpr) -- stack terminated with Stacked Sames
           | otherwise = ([x], snocL pl $ head st) -- stack terminated without Stacked Sames
             where
               isSameStacked = 1 < length st
               prevTag = getTagJP $ head st
               isNewTagSame = getTagJP x == prevTag
               foldedOpr = Opr (getTagJP $ head st) st

foldSameTags0 :: SExp JTag String -> SExp JTag String
foldSameTags0 Nil = Nil
foldSameTags0 (Atom a b) = (Atom a b)
foldSameTags0 (Opr tg sexps)
  | sexps == [] = Nil
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    res@(stEnd, plEnd)
      | otherwise = foldl f ([head sexps], []) $ tail sexps
       where
         f y@(st, pl) x
           | isNewTagSame = (snocL st x, pl) -- <- stack continue
           | isSameStacked = ([x], snocL pl foldedOpr) -- stack terminated with Stacked Sames
           | otherwise = ([x], snocL pl $ head st) -- stack terminated without Stacked Sames
             where
               isSameStacked = 1 < length st
               prevTag = getTagJP $ head st
               isNewTagSame = (jTag $ getTagJP x) == (jTag prevTag)
               foldedOpr = Opr (getTagJP $ head st) st

foldSameTags1 :: SExp JTag String -> SExp JTag String
foldSameTags1 Nil = Nil
foldSameTags1 (Atom a b) = (Atom a b)
foldSameTags1 (Opr tg sexps)
  | sexps == [] = Nil
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    res@(stEnd, plEnd)
      | otherwise = foldl f ([head sexps], []) $ tail sexps
       where
         f y@(st, pl) x
           | isNewTagSame = (snocL st x, pl) -- <- stack continue
           | isSameStacked = ([x], snocL pl foldedOpr) -- stack terminated with Stacked Sames
           | otherwise = ([x], snocL pl $ head st) -- stack terminated without Stacked Sames
             where
               isSameStacked = 1 < length st
               prevTag = getTagJP $ head st
               isNewTagSame = (jTag1 $ getTagJP x) == (jTag1 prevTag)
               foldedOpr = Opr (getTagJP $ head st) st

foldSuffix :: SExp JTag String -> SExp JTag String
foldSuffix Nil = Nil
foldSuffix (Atom a b) = (Atom a b)
foldSuffix (Opr tg sexps)
  | sexps == [] = Nil
  | otherwise = Opr tg (plEnd ++ stEnd)
  where
    res@(stEnd, plEnd)
      | otherwise = foldl f ([head sexps], []) $ tail sexps
       where
         f y@(st, pl) x
           | isNewTagSuffix = (snocL st x, pl) -- <- stack continue
           | isSuffixStacked = ([x], snocL pl foldedOpr) -- stack terminated with Suffix
           | otherwise = ([x], snocL pl $ head st) -- stack terminated without Suffix
             where
               isSuffixStacked = 1 < length st
               purged = Opr (getTagJP x) $ snocL st x
               isNewTagSuffix = ((jTag $ getTagJP x) == "接尾辞") ||  ((jTag1 $ getTagJP x) == "接尾")
               foldedOpr = Opr (getTagJP $ head st) st

foldPrefix :: SExp JTag String -> SExp JTag String
foldPrefix Nil = Nil
foldPrefix (Atom a b) = (Atom a b)
foldPrefix (Opr tg sexps) = Opr tg (plEnd ++ stEnd)
  where
    res@(stEnd, plEnd) = foldl f ([], []) sexps
       where
         f y@(st, pl) x
           | isNewTagPrefix = (snocL st x, pl) -- <- new Stack Start
           | isNoStacked = ([], snocL pl x) -- determined prefixHead tag, and stacking terminated
           | otherwise = ([], snocL pl purged) -- determined prefixHead tag, and stacking terminated
             where
               isNoStacked = st == []
               purged = Opr (getTagJP x) $ snocL st x
               isNewTagPrefix = ((jTag $ getTagJP x) == "接頭詞") || ((jTag1 $ getTagJP x) == "接頭")
               -- isNewTagPrefix = ((jTag $ getTagJP x) == "接頭辞") || ((jTag1 $ getTagJP x) == "接頭")

getTagJP :: SExp JTag String -> JTag
getTagJP Nil = nilJTag
getTagJP (Atom tag _) = tag
getTagJP (Opr tag _) = tag


foldlDebug :: (a1 -> a2 -> a1) -> a1 -> [a2] -> Int -> (a1, a2)
foldlDebug f yInit xs n = (scanned !! n, xs !! n)
   where
     scanned = scanl f yInit xs

type PageTrees = [[SExp (Posi, Tag) String]]
data LayoutMode = OneTotal | TwoCols

data MData = CMData {
    mToken :: String
  , mTag :: String
  , mTag1 :: String
  , mAttr1 :: String
  , mAttr2 :: String
  , mGr1 :: String
  , mGr2 :: String
  , mVoc1 :: String
  , mVoc2 :: String
  , mVoc3 :: String
 }
  deriving (Show, Eq, Ord, Read)



foldTokSq :: SExp Tag (String, [Sq Double]) -> ([String], [Sq Double])
foldTokSq = foldSExp f0 f1 f2 ([], [])
  where
    f0 tg res = res
    f1 (resTok, resSq) (resTok1, resSq1) = (resTok ++ resTok1, resSq ++ resSq1)
    f2 tag tokSq@(str, sqs) = ([str], sqs)


synSqs :: [Sq Double] -> [Sq Double]
synSqs [] = []
synSqs sqs'@(sq:sqs) = stss ++ [residue]
  where
    tp = sqTop sq
    lef = sqLeft sq
    bot = sqBot sq
    foldPoints@(tpp, leff, bott, fronn, stss) = foldl f (tp, lef, bot, sq, []) sqs
      where
        f y@(tp1, lef1, bot1, fronSq, stacked) x
          | sqLeft x < sqLeft fronSq = (sqTop x, sqLeft x, sqBot x, x, stacked ++ [CSq {sqTop = tp1, sqLeft = lef1, sqBot = bot1, sqRight = sqRight fronSq}])
          | otherwise = (tp1, lef1, bot1, x, stacked)
    residue = CSq {sqTop = tpp, sqLeft = leff, sqBot = bott, sqRight = sqRight fronn}

{-
pathPrefix = "file://"
pathInfix1 = "/home/polymony/poppyS/"
pathInfix2 = "pdfs/"
pdfs <- listDirectory (pathInfix1 ++ pathInfix2)
pdfPath = Text.pack $ pathPrefix ++ pathInfix1 ++ pathInfix2 ++ (head pdfs)
nPage = fromIntegral 1
layoutsPrim = [CSq{sqTop = 0.0, sqLeft = 0.0, sqBot = 1.0, sqRight = 1.0}]
layoutMode = TwoCols
docPathSuffix = "./pdfs/CTFP.pdf"
docPathSuffix = "./pdfs/The_CUDA_Handbook.pdf"
docPathSuffix = "./" ++ pathInfix2 ++ (head pdfs)
nPage = 10
doc <- GPop.documentNewFromFile pdfPath Nothing
page <- GPop.documentGetPage doc nPage
nPages <- GPop.documentGetNPages doc

res <- mapM (\n -> extractSExpsJP =<< GPop.documentGetPage doc n) [0 .. nPages - 1]
res2 = V.concatMap id $ V.fromList res





iter <- GPop.indexIterNew doc
curNum = 0
next = 0
dest <- GPop.newZeroDest
title = ""
last_tmp <- GPop.indexIterNew doc
action <- GPop.indexIterGetAction(iter)
hoge = GPop.getActionType action
hogegeType <- hoge
isGOTODEST = hogegeYype == GPop.ActionTypeGotoDest
actionGotoType <- GPop.getActionGotoDestType =<< GPop.getActionGotoDest action
destDestNameMaybe <- GPop.getActionGotoDestDest =<< GPop.getActionGotoDest action
aa <- case destDestNameMaybe of
   Nothing -> ""
   Just typ -> if typ == GPop.getActionGotoDestDestNamed
       then do
          nam <- GPop.namedDest
          destName <- GPop. doc
          nDest <- GPop.documentFindDest doc

isDESTNAMED = not $ destDestName == Nothing
destN <- GPop.documentFindDest doc destDestName
currNum <- GPop.getDestPageNum destN
currNumOtherwise <- GPop.getDestPageNum =<< GPop.getActionGotoDest action
GPop.actionFree action
last_tmp <- GPop.indexIterCopy iter
isNext <- GPop.indexIterNext iter
isDesc = isNext == False || num < curNum




page <- GPop.documentGetPage doc nPage
nPages <- GPop.documentGetNPages doc

ff n = getSExpsIOSCheck =<< GPop.documentGetPage doc n
ff2 n = getSExpsIOSCheck2 =<< GPop.documentGetPage doc n
mapM ff2 [7 .. 9]
mapM ff [0 .. nPages - 1]


aa@(isExistsText, rects) <- GPop.pageGetTextLayout page
chars <- mapM (GPop.pageGetTextForArea page) rects
rectsSwapped <- mapM (gpopRectToPopPRect page) rects
sqs = map (pRectToSq hei) rectsSwapped
charRects = zip chars rects
charSqs = zip chars sqs

sexp = sexps !! 8
sexpG = mapNode snd fst sexp
-}

walkIndex iter = do
  child@(GPop.IndexIter chld) <- GPop.indexIterGetChild iter
  isNull <- withForeignPtr (managedForeignPtr chld) (\p -> return $ p == nullPtr)
  when (not isNull) $ walkIndex child
  next <- GPop.indexIterNext iter
  case next of
    True -> return ()
    False -> return ()

getSExpsIOSCheck page = do
  n <- GPop.pageGetIndex page
  (wid, hei) <- GPop.pageGetSize page
  chInfos <- getChInfosS page
  let
    blocks = getBlockS chInfos hei wid
  let
    forBlocksCheck = V.map (V.map (V.map chIChar)) blocks
  putStrLn $ show forBlocksCheck
  putStrLn $ show n

{-
doc <- GPop.documentNewFromFile (Text.pack "file:///home/polymony/poppyS/gisho.pdf") Nothing
page <- GPop.documentGetPage doc 143
-}

extractSExpsJP
  :: GPop.Page -> IO (V.Vector (String, String, String))
extractSExpsJP page = do
  (wid, hei) <- GPop.pageGetSize page
  chInfos <- getChInfos page
  blocksPrim <- (\x -> getBlock x hei wid) <$> getChInfos page -- stable, but not covers some PDFs which has character layout bugs.
  -- blocksPrimS <- (\x -> getBlockS x hei wid) <$> getChInfosS page -- probably Unstable.
  let
    blocks = V.map (V.map (V.map g)) blocksPrim
      where
        g info = info {chIChar = gg $ chIChar info}
          where
            gg str = V.foldl' ggg str replaceTable
              where
                replaceTable = V.fromList [("\8208", "-"), ("\8217", "'")]
                ggg str2 (nd, ns) = Text.replace nd ns str2
    forBlocksCheck = V.map (V.map (V.map chIChar)) blocks
    charSqVConcated = V.map (V.concatMap (V.map f)) blocks
      where
        f inf
          | chText == "" = error "a square's chText is Void"
          | otherwise = (head chText, V.singleton $ chISq inf)
          where
            chText = Text.unpack $ chIChar inf
    stackedSens = blockLinesConcated
      where
        blockLineChars = V.map (V.map (V.map (head . Text.unpack . chIChar))) blocks -- :: V.Vector (V.Vector (V.Vector Char)) -- first is Block second is lines in Block last is Line in Lines
        blockLinesConcated = V.map concatLines blockLineChars
          where
            concatLines bLines
              | bLines == V.empty = ""
              | otherwise = folddd
             where
                bLinesStr = V.map (fff . V.toList) bLines
                 where
                   fff strstr
                    | strstr == "" = ""
                    | last strstr == '\n' = init strstr
                    | otherwise = strstr
                folddd = V.foldl' g (V.head bLinesStr) $ V.tail bLinesStr
                g y x
                  | x == "" = y
                  | (not isIsolated) && isLastBar = (init y) ++ x
                  | isLastSpace = y ++ x
                  | otherwise = y ++ " " ++ x
                  where
                    isIsolated = 1 < length y && ((last $ init y) == ' ')
                    isLastBar = 0 < length y && (last y == '-' || last y == '\8208')
                    isLastSpace = 0 < length y && last y == ' '
        blockLinesConcatedJP = V.map concatLines blockLineChars
          where
            concatLines bLines
              | bLines == V.empty = ""
              | otherwise = folddd
             where
                bLinesStr = V.map (fff . V.toList) bLines
                 where
                   fff strstr
                    | strstr == "" = ""
                    | last strstr == '\n' = init strstr
                    | otherwise = strstr
                folddd = V.foldl' g (V.head bLinesStr) $ V.tail bLinesStr
                g y x
                  | x == "" = y
                  | (not isIsolated) && isLastBar = (init y) ++ x
                  | isLastSpace = y ++ x
                  | otherwise = y ++ "" ++ x
                  where
                    isIsolated = 1 < length y && ((last $ init y) == ' ')
                    isLastBar = 0 < length y && (last y == '-' || last y == '\8208')
                    isLastSpace = 0 < length y && last y == ' '
    retrSexpS (sens, charSqsPrim) = do
      stanRess <- stanIOPoppy1 sens
      let
        charSqs = V.filter (\x -> (not $ fst x == '\n')) charSqsPrim
        sexps = stanAssign2Poppy1 charSqs stanRess
      return sexps
    retrSexpSJP (sens, charSqsPrim) = do
      mecabRes <- getMecabed sens
      let
        resLimited = V.map (\x -> (mToken x, mTag x, mTag1 x)) mecabRes
        tokenOnly = V.map mToken mecabRes
        folded = V.foldl' f "" tokenOnly
          where
            f y x = y ++ " " ++ x
      return resLimited
    zipped = V.zip stackedSens charSqVConcated
  bb <- V.mapM retrSexpSJP zipped
  return $ V.concatMap id bb

{-
doc <- GPop.documentNewFromFile (Text.pack "file:///home/polymony/poppyS/bug0.pdf") Nothing
page <- GPop.documentGetPage doc 0
isJapanese = True
-}
getSExpsIOS
  :: GPop.Page -> Bool -> IO [SExp (Posi, Tag) ([Char], [Sq Double])]
getSExpsIOS page isJapanese = do
  hoge <- GPop.pageGetText page
  (wid, hei) <- GPop.pageGetSize page
  chInfos <- getChInfos page
  blocksPrim <- (\x -> getBlock x hei wid) <$> getChInfos page -- stable, but not covers some PDFs which has character layout bugs.
  -- blocksPrimS <- (\x -> getBlockS x hei wid) <$> getChInfosS page -- probably Unstable.
  let
    blocks = V.map (V.map (V.map g)) blocksPrim
      where
        g info = info {chIChar = gg $ chIChar info}
          where
            gg str = V.foldl' ggg str replaceTable
              where
                replaceTable = V.fromList [("\8208", "-"), ("\8217", "'")]
                ggg str2 (nd, ns) = Text.replace nd ns str2
    forBlocksCheck = V.map (V.map (V.map chIChar)) blocks
    charSqVConcated = V.map (V.concatMap (V.map f)) blocks
      where
        f inf
          | chText == "" = error "a square's chText is Void"
          | otherwise = (head chText, V.singleton $ chISq inf)
          where
            chText = Text.unpack $ chIChar inf
    stackedSens = blockLinesConcated
      where
        blockLineChars = V.map (V.map (V.map (head . Text.unpack . chIChar))) blocks -- :: V.Vector (V.Vector (V.Vector Char)) -- first is Block second is lines in Block last is Line in Lines
        blockLinesConcated = V.map concatLines blockLineChars
          where
            concatLines bLines
              | bLines == V.empty = ""
              | otherwise = folddd
             where
                bLinesStr = V.map (fff . V.toList) bLines
                 where
                   fff strstr
                    | strstr == "" = ""
                    | last strstr == '\n' = init strstr
                    | otherwise = strstr
                folddd = V.foldl' g (V.head bLinesStr) $ V.tail bLinesStr
                g y x
                  | x == "" = y
                  | (not isIsolated) && isLastBar = (init y) ++ x
                  | isLastSpace = y ++ x
                  | otherwise = y ++ " " ++ x
                  where
                    isIsolated = 1 < length y && ((last $ init y) == ' ')
                    isLastBar = 0 < length y && (last y == '-' || last y == '\8208')
                    isLastSpace = 0 < length y && last y == ' '
        blockLinesConcatedJP = V.map concatLines blockLineChars
          where
            concatLines bLines
              | bLines == V.empty = ""
              | otherwise = folddd
             where
                bLinesStr = V.map (fff . V.toList) bLines
                 where
                   fff strstr
                    | strstr == "" = ""
                    | last strstr == '\n' = init strstr
                    | otherwise = strstr
                folddd = V.foldl' g (V.head bLinesStr) $ V.tail bLinesStr
                g y x
                  | x == "" = y
                  | (not isIsolated) && isLastBar = (init y) ++ x
                  | isLastSpace = y ++ x
                  | otherwise = y ++ "" ++ x
                  where
                    isIsolated = 1 < length y && ((last $ init y) == ' ')
                    isLastBar = 0 < length y && (last y == '-' || last y == '\8208')
                    isLastSpace = 0 < length y && last y == ' '
    retrSexpS (sens, charSqsPrim) = do
      stanRess <- stanIOPoppy1 sens
      let
        charSqs = V.filter (\x -> (not $ fst x == '\n')) charSqsPrim
        sexps = stanAssign2Poppy1 charSqs stanRess
      return sexps
    retrSexpSJP (sens, charSqsPrim) = do
      mecabRes <- getMecabed sens
      let
        sexp0 = toSExpsJP mecabRes
        -- sexp = foldNPsJP sexp0
        sexp = foldNPsJPRecursive sexp0
      -- showSP sexp
      let
        indexed0 = indexingSP sexp
        forgotten = map (\x@((x1,x2),x3) -> (x1,x2,x3)) $ forgetSExp indexed0
        indexed = [res]
          where
          res = map ggg resPrim
            where
              ggg (nums, tag, tok)
                -- | (length nums == 1) && (jTag tag == "名詞") = (nums, NP, tok)
               -- | (jTag tag == "名詞") = (nums, NP, tok)
               | (jTagIsNP tag == True) = (nums, NP, tok)
               | otherwise = (nums, TAGVOID, tok)
          resPrim = gg forgotten
          gg = map g
          g (a, b, c) = (a, b, ff <$> c)
          ff str = case found of
            Nothing -> str
            Just (_, e) -> e
            where
              found = Lis.find (\str2 -> (fst str2) == str) replaceStanTable
        charSqs = V.filter (\x -> (not $ fst x == '\n')) charSqsPrim
        sexps = stanAssign2Poppy1JP charSqs indexed
      return sexps
    zipped = V.zip stackedSens charSqVConcated
    isJPBifurk zipped
     | isJapanese = V.mapM retrSexpSJP zipped
     | otherwise  = V.mapM retrSexpS zipped
  bb <- isJPBifurk zipped
  -- bb <- V.mapM retrSexpSJP zipped
  return $ concatMap id bb

data ChInfo = CChInfo {
    chIChar :: Text.Text
  , chISq :: Sq Double
  , chIFontName :: [Maybe Text.Text]
  , chIFontSize :: [Double]
  }
    deriving (Eq, Ord, Show)

-- prp
getChInfos :: GPop.Page -> IO (V.Vector ChInfo)
getChInfos page = do
  txttxt <- GPop.pageGetText page
  if txttxt == ""
    then return V.empty
    else do
      (wid, hei) <- GPop.pageGetSize page
      aa@(isExistsText, rects) <- GPop.pageGetTextLayout page
      chars <- mapM (GPop.pageGetTextForArea page) rects
      rectsSwapped <- mapM (gpopRectToPopPRect page) rects
      attrss <- mapM (GPop.pageGetTextAttributesForArea page) rects
      let
        getFontNameSafe attr = do
          let
            f e = do
              let
                err = show (e :: UnicodeException)
              putStrLn ("warning: cannot read a font name: " ++ err)
              return (Just "")
          hoge <- GPop.getTextAttributesFontName attr `E.catch` f
          return hoge
        getFontNames attrsPrim = do
          -- fontsPrim <- mapM GPop.getTextAttributesFontName attrsPrim
          fontsPrim <- mapM getFontNameSafe attrsPrim
          return fontsPrim
        getFontSizes attrsPrim = do
          fontsPrim <- mapM GPop.getTextAttributesFontSize attrsPrim
          return fontsPrim
        freeAttrs attrs = do
          mapM GPop.textAttributesFree attrs
      fontNames <- mapM getFontNames attrss
      fontSizes <- mapM getFontSizes attrss
      -- mapM freeAttrs attrss
      let
        -- sqs = map (pRectToSq hei) rectsSwapped
        sqs = map pRectToSqNoSwap rectsSwapped
        charRects = zip chars rects
        charSqs = zip chars sqs
        fonts = zip fontNames fontSizes
        chInfos = V.fromList $ map f $ zip charSqs fonts
          where
            f ((ch, sq), (fName, fSize)) = CChInfo {
              chIChar = ch
            , chISq = sq
            , chIFontName = fName
            , chIFontSize = fSize}
      return chInfos

--prp
getChInfosS :: GPop.Page -> IO (V.Vector ChInfo)
getChInfosS page = do
  txttxt <- GPop.pageGetText page
  if txttxt == ""
    then return V.empty
    else do
      (wid, hei) <- GPop.pageGetSize page
      aa@(isExistsText, rectsPrim) <- GPop.pageGetTextLayout page
      let
        charsPrim = map (\c -> Text.pack $ [c]) $ Text.unpack txttxt
        zipzip = zip charsPrim rectsPrim
        chars = takeFstL zipzip
        rects = takeSndL zipzip
      -- chars <- mapM (GPop.pageGetTextForArea page) rects
      rectsSwapped <- mapM (gpopRectToPopPRect page) rects
      attrss <- mapM (GPop.pageGetTextAttributesForArea page) rects
      let
        getFontNames attrsPrim = do
          fontsPrim <- mapM GPop.getTextAttributesFontName attrsPrim
          return fontsPrim
        getFontSizes attrsPrim = do
          fontsPrim <- mapM GPop.getTextAttributesFontSize attrsPrim
          return fontsPrim
        freeAttrs attrs = do
          mapM GPop.textAttributesFree attrs
      fontNames <- mapM getFontNames attrss
      fontSizes <- mapM getFontSizes attrss
      -- mapM freeAttrs attrss
      let
        sqs = map pRectToSqNoSwap rectsSwapped
        charRects = zip chars rects
        charSqs = zip chars sqs
        fonts = zip fontNames fontSizes
        chInfos = V.fromList $ map f $ zip charSqs fonts
          where
            f ((ch, sq), (fName, fSize)) = CChInfo {
              chIChar = ch
            , chISq = sq
            , chIFontName = fName
            , chIFontSize = fSize}
      return chInfos

getBlock
  :: V.Vector ChInfo
     -> Double -> p -> V.Vector (V.Vector (V.Vector ChInfo))
getBlock chInfos hei wid
  | chInfos == V.empty = V.empty
  | otherwise = folded
  where
    marks = V.map (Text.pack . (\x -> [x])) $ V.fromList ['.', ';', head ":", '!', '?', '。']
    folded
      | otherwise = V.filter (\x -> not $ x == V.empty) $ V.snoc pooledBlocksF (V.snoc pooledLinesF stackedCharsF)
      where
        folddd@(stackedCharsF, pooledLinesF, pooledBlocksF)
          | otherwise = V.foldl' g (V.singleton $ V.head chInfos, V.empty, V.empty) $ V.tail chInfos

        -- TermByMark
        --    terminate previous line and current line, and pool them as a new block.
        -- TermByOtherHeight
        -- TermOtherFont
        --    terminate previous line and current line, and pool them as a new block.
        xs = V.tail chInfos
        aaaa = V.scanl' g (V.singleton $ V.head chInfos, V.empty, V.empty) xs
        forCheck = V.map (V.map chIChar) $ takeFstT aaaa
        g y@(stackedChars, pooledLines, pooledBlocks) x
          | isTerminateBlockPunct = (V.singleton xMutated,    V.empty,         nextPooledBlocksL)
          | isCurrVoid          =   (nextStackedChars, pooledLines,     pooledBlocks)
          | isTerminateOtherFont =  (V.singleton xMutated,    V.empty,         nextPooledBlocksL)
          | isReturned    =         (V.singleton xMutated,    nextPooledLines, pooledBlocks)
          | otherwise          =    (nextStackedChars, pooledLines,     pooledBlocks)
          where
            currCol = sqRight $ chISq x
            currRow = sqBot $ chISq x
            prevCol = sqRight $ chISq $ V.last stackedChars
            prevRow = sqBot $ chISq $ V.last stackedChars
            currFontName
              | fontNames == [] = Nothing
              | otherwise = head fontNames
              where
                fontNames = chIFontName x
            currFontSize
              | fontSizes == [] = 0.0
              | otherwise = head fontSizes
              where
                fontSizes = chIFontSize x
            prevFontName = snd $ Lis.maximumBy g $ getHistogram $ concat $ V.toList $ V.map chIFontName $ stackedChars
              where
                g x y = compare (fst x) (fst y)
            prevFontSize = snd $ Lis.maximumBy g $ getHistogram $ concat $ V.toList $ V.map chIFontSize $ stackedChars
              where
                g x y = compare (fst x) (fst y)

            isPrevVoid = (chIChar $ V.last stackedChars) == (Text.pack "")
            isCurrVoid = (chIChar x) == (Text.pack "")
            isReturned = 0.0001 < (abs (currRow - prevRow) / hei) -- maybe FontSize should refered.
            isTerminateBlockPunct = isPrevLineHasTwoMoreChars && isReturned && isPunct && isNotIso && isPrevExists
               where
                  isPrevExists = 0 < V.length pooledLines
                  isPrevLineHasTwoMoreChars = 3 < (V.length stackedChars)
                  -- isReturnedNextCol = 0.001 < (prevRow - currRow) / hei -- maybe FontSize should refered.
                  isPunct = V.elem (chIChar $ V.last $ V.init stackedChars) marks
                  isNotIso = not $ (chIChar $ V.last $ V.init $ V.init stackedChars) == (Text.pack "")
            isTerminateOtherFont = isReturned && ((abs $ currFontSize - prevFontSize) > 2.0) -- || (not $ currFontName == prevFontName)
               where
                  isPrevCharExists = 0 < V.length stackedChars

            nextPooledBlocksL = (V.snoc pooledBlocks nextPooledLines)
            nextPooledLines = V.snoc pooledLines $ (V.snoc stackedCharsInit mutatedLast)
               where
                stackedCharsInit = V.init stackedChars
                stackedCharsLast = V.last stackedChars
                mutatedLast = stackedCharsLast {chIChar = Text.pack "\n"}
            nextStackedChars = (V.snoc stackedChars xMutated)
            xMutated
              | isCurrVoid = (V.last stackedChars) {chISq = currCharSq, chIChar = Text.pack " "}
              | otherwise = x
                 where
                   currCharSq = chISq x

            --isInitialCharStacked = not $ stackedChars == V.empty

        -- ho = V.map (V.map (V.toList . takeFst)) folded


getBlockS
  :: V.Vector ChInfo
     -> p1 -> p2 -> V.Vector (V.Vector (V.Vector ChInfo))
getBlockS chInfos hei wid
  | chInfos == V.empty = V.empty
  | otherwise = folded
  where
    marks = V.map (Text.pack . (\x -> [x])) $ V.fromList ['.', ';', head ":", '!', '?']
    folded
      | otherwise = V.filter (\x -> not $ x == V.empty) $ V.snoc pooledBlocksF (V.snoc pooledLinesF stackedCharsF)
      where
        folddd@(stackedCharsF, pooledLinesF, pooledBlocksF)
          | otherwise = V.foldl' g (V.singleton $ V.head chInfos, V.empty, V.empty) $ V.tail chInfos

        -- TermByMark
        --    terminate previous line and current line, and pool them as a new block.
        -- TermByOtherHeight
        -- TermOtherFont
        --    terminate previous line and current line, and pool them as a new block.
        xs = V.tail chInfos
        aaaa = V.scanl' g (V.singleton $ V.head chInfos, V.empty, V.empty) xs
        forCheck = V.map (V.map chIChar) $ takeFstT aaaa
        g y@(stackedChars, pooledLines, pooledBlocks) x
          | isTerminateBlockPunct = (V.singleton x,    V.empty,         nextPooledBlocksL)
--          | isCurrVoid          =   (nextStackedChars, pooledLines,     pooledBlocks)
          | isTerminateOtherFont =  (V.singleton x,    V.empty,         nextPooledBlocksL)
          | isReturned    =         (V.singleton x,    nextPooledLines, pooledBlocks)
          | otherwise          =    (nextStackedChars, pooledLines,     pooledBlocks)
          where
            isStackedCharsVoid = stackedChars == V.empty
            currFontName
              | fontNames == [] = Nothing
              | otherwise = head fontNames
              where
                fontNames = chIFontName x
            currFontSize
              | fontSizes == [] = 0.0
              | otherwise = head fontSizes
              where
                fontSizes = chIFontSize x
            prevFontName = snd $ Lis.maximumBy g $ getHistogram $ concat $ V.toList $ V.map chIFontName $ stackedChars
              where
                g x y = compare (fst x) (fst y)
            prevFontSize = snd $ Lis.maximumBy g $ getHistogram $ concat $ V.toList $ V.map chIFontSize $ stackedChars
              where
                g x y = compare (fst x) (fst y)

            isCurrVoid = (chIChar x) == (Text.pack " ")
            isReturned = (chIChar $ V.last stackedChars) == Text.pack "\n" -- maybe FontSize should refered.
            isTerminateBlockPunct = isPrevLineHasTwoMoreChars && isReturned && isPunct && isNotIso && isPrevExists
               where
                  isPrevExists = 0 < V.length pooledLines
                  isPrevLineHasTwoMoreChars = 3 < (V.length stackedChars)
                  isPunct = V.elem (chIChar $ V.last $ V.init stackedChars) marks
                  isNotIso = not $ (chIChar $ V.last $ V.init $ V.init stackedChars) == (Text.pack "")
            isTerminateOtherFont = isReturned && ((abs $ currFontSize - prevFontSize) > 2.0)  -- || (not $ currFontName == prevFontName)
               where
                  isPrevCharExists = 0 < V.length stackedChars

            nextPooledBlocksL = (V.snoc pooledBlocks nextPooledLines)
            nextPooledLines = V.snoc pooledLines stackedChars
            nextStackedChars = (V.snoc stackedChars x)
        -- ho = V.map (V.map (V.toList . takeFst)) folded


replaceStanTable = [("-LRB-", "("), ("-RRB-", ")")]

stanAssign2Poppy1JP
  :: (Eq a1, Eq a2) =>
     V.Vector (Char, V.Vector a2)
     -> [[(Posi, a1, Maybe [Char])]] -> [SExp (Posi, a1) ([Char], [a2])]
stanAssign2Poppy1JP charSqs indexed = map reconsSExp resSExpForg
  where
    lengths = map length indexed
    indexedFlattened = concat indexed
    tokens = V.map f $ V.filter (\val -> not $ val == Nothing) $ takeThdT $ V.fromList indexedFlattened
       where
         f x = case x of
           Just x -> x
           Nothing -> ""
    interposedSpace
      | tokens == V.empty = V.empty
      | otherwise = V.fromList $  V.foldl' f (V.head tokens) $ V.tail tokens
      where
        f y x = y ++ " " ++ x
    chSqsMute = charSqs
    assigned = assignByDiff interposedSpace chSqsMute
    assignedFlt = V.map (\x@(c, sqss) -> (c, V.concatMap id sqss)) assigned
    forgottenIndexed = indexing indexedFlattened
    onlyToken = map (\x@(i, (is, tg, tk)) -> (i, g tk)) $ filter (\x@(i, (is, tg, tk)) -> f tk) forgottenIndexed
       where
         f tk = case tk of
           Nothing -> False
           Just _ -> True
         g tk = case tk of
           Nothing -> ""
           Just a -> a
    justsIndices = takeFstL onlyToken
    onlyTokenInterposed
     | onlyToken == [] = []
     | otherwise = foldl f [head onlyToken] $ tail onlyToken
        where
          f y x = y ++ [(-1, " ")] ++ [x]
    onlyTokenFlt = concatMap (\x@(i,str) -> map (\y -> (i, y)) str) onlyTokenInterposed
    index = takeFstL onlyTokenFlt
    chars = V.fromList $ takeSndL onlyTokenFlt
    assedSqs = map h2 $ Lis.groupBy (\x -> \y -> (fst x) == (fst y)) $ zip index $ V.toList $  V.map (\x@(c, sqss) -> (c, V.concatMap id sqss)) $ assignByDiff chars assignedFlt
       where
         h2 lis = (i, (tokens, sqs))
           where
             i = fst $ head lis
             tokens = takeFstL $ takeSndL lis
             sqs = concatMap V.toList $ takeSndL $ takeSndL lis
    assedSqs2 = map h3 $ Lis.groupBy (\x -> \y -> (fst x) == (fst y)) $ filter (\x@(i,_) -> not $ i == -1) $ zip index $ V.toList $ takeSnd $ assignByDiff chars assignedFlt
       where
         h3 lis = (i, sqs)
           where
             i = fst $ head lis
             sqs = concatMap (V.toList . (V.concatMap id)) $ takeSndL lis
    resForgotten2 = map g3 forgottenIndexed
      where
        g3 (i, (is, tg, mbStr))
         | isJust = (is, tg, Just (tk, tokSq))
         | otherwise = (is, tg, Nothing)
           where
             tk = case mbStr of
               Just tkk -> tkk
               Nothing -> ""
             isJust = not $ mbStr == Nothing
             tokSq = snd $ head $ filter (\x -> (fst x) == i) assedSqs2
    separated@(rawRes, resSExpForg) = foldl f (resForgotten2, []) lengths
      where
        f y@(raw, stacked) n = (drop n raw, stacked ++ [take n raw])

--kesuna
stanAssign2Poppy1
  :: Eq a =>
     V.Vector (Char, V.Vector a)
     -> [Turtle.Text] -> [SExp (Posi, Tag) (String, [a])]
stanAssign2Poppy1 charSqs stanRess = map reconsSExp resSExpForg
  where
    indexed = map (gg . forgetIndexed) stanRess
      where
        gg = map g
        g (a, b, c) = (a, b, ff <$> c)
        ff str = case found of
          Nothing -> str
          Just (_, e) -> e
          where
            found = Lis.find (\str2 -> (fst str2) == str) replaceStanTable
    lengths = map length indexed
    indexedFlattened = concat indexed
    tokens = V.map f $ V.filter (\val -> not $ val == Nothing) $ takeThdT $ V.fromList indexedFlattened
       where
         f x = case x of
           Just x -> x
           Nothing -> ""
    interposedSpace
      | tokens == V.empty = V.empty
      | otherwise = V.fromList $  V.foldl' f (V.head tokens) $ V.tail tokens
      where
        f y x = y ++ " " ++ x
    chSqsMute = charSqs
    assigned = assignByDiff interposedSpace chSqsMute
    assignedFlt = V.map (\x@(c, sqss) -> (c, V.concatMap id sqss)) assigned
    forgottenIndexed = indexing indexedFlattened
    onlyToken = map (\x@(i, (is, tg, tk)) -> (i, g tk)) $ filter (\x@(i, (is, tg, tk)) -> f tk) forgottenIndexed
       where
         f tk = case tk of
           Nothing -> False
           Just _ -> True
         g tk = case tk of
           Nothing -> ""
           Just a -> a
    justsIndices = takeFstL onlyToken
    onlyTokenInterposed
     | onlyToken == [] = []
     | otherwise = foldl f [head onlyToken] $ tail onlyToken
        where
          f y x = y ++ [(-1, " ")] ++ [x]
    onlyTokenFlt = concatMap (\x@(i,str) -> map (\y -> (i, y)) str) onlyTokenInterposed
    index = takeFstL onlyTokenFlt
    chars = V.fromList $ takeSndL onlyTokenFlt
    assedSqs = map h2 $ Lis.groupBy (\x -> \y -> (fst x) == (fst y)) $ zip index $ V.toList $  V.map (\x@(c, sqss) -> (c, V.concatMap id sqss)) $ assignByDiff chars assignedFlt
       where
         h2 lis = (i, (tokens, sqs))
           where
             i = fst $ head lis
             tokens = takeFstL $ takeSndL lis
             sqs = concatMap V.toList $ takeSndL $ takeSndL lis
    assedSqs2 = map h3 $ Lis.groupBy (\x -> \y -> (fst x) == (fst y)) $ filter (\x@(i,_) -> not $ i == -1) $ zip index $ V.toList $ takeSnd $ assignByDiff chars assignedFlt
       where
         h3 lis = (i, sqs)
           where
             i = fst $ head lis
             sqs = concatMap (V.toList . (V.concatMap id)) $ takeSndL lis
    resForgotten2 = map g3 forgottenIndexed
      where
        g3 (i, (is, tg, mbStr))
         | isJust = (is, tg, Just (tk, tokSq))
         | otherwise = (is, tg, Nothing)
           where
             tk = case mbStr of
               Just tkk -> tkk
               Nothing -> ""
             isJust = not $ mbStr == Nothing
             tokSq = snd $ head $ filter (\x -> (fst x) == i) assedSqs2
    separated@(rawRes, resSExpForg) = foldl f (resForgotten2, []) lengths
      where
        f y@(raw, stacked) n = (drop n raw, stacked ++ [take n raw])



type Square = ((Int, Int), (Int, Int))
type SquareD = ((Double, Double), (Double, Double))



isIncludedPopplerRect :: PopPRectangle -> PopPRectangle -> Bool
isIncludedPopplerRect ns@(PopPRectangle a b c d) nd@(PopPRectangle e f g h)
  = (a <= e) && (b <= f) && (g <= c) && (h <= d)

popPPageFindText :: GPop.Page -> Turtle.Text -> IO [PopPRectangle]
popPPageFindText page token = do
  -- rects <- GPop.pageFindText page token
  rects <- GPop.pageFindTextWithOptions page token [GPop.FindFlagsCaseSensitive]
  rectsSwapped <- mapM (gpopRectToPopPRect page) rects
  return rectsSwapped

getCharPossSqRect
  :: [Char]
     -> GPop.Page
     -> V.Vector (Char, V.Vector PopPRectangle)
     -> IO (V.Vector (Char, V.Vector PopPRectangle))
getCharPossSqRect [] page stacked = return stacked
getCharPossSqRect (c:cs) page stacked = do
  charPoss <- popPPageFindText page $ Text.pack [c]
  aaaa <- getCharPossSqRect cs page (V.snoc stacked (c, V.fromList charPoss))
  return $ aaaa

extractChars :: String -> String
extractChars str = res
  where
    res = Lis.sort $ Lis.nub str

delimitAtV :: Eq a => a -> V.Vector a -> V.Vector (V.Vector a)
delimitAtV d vs = V.snoc resPl resSt
  where
    res@(resSt, resPl) = V.foldl' f (V.empty, V.empty) vs
      where
        f y@(stacked, pooled) x
          | x == d = (V.empty, V.snoc (V.snoc pooled stacked) $ V.singleton x)
          | otherwise = (V.snoc stacked x, pooled)

data PopPRectangle = PopPRectangle Double Double Double Double
  deriving (Show, Eq, Ord, Read)
   -- xSmaller ySmaller xBigger yBigger
   -- from Normal mathematical Axis (lower left is (0, 0))

pRectToSq :: Double -> PopPRectangle -> Sq Double
pRectToSq hei (PopPRectangle smallX smallY bigX bigY) = CSq {sqTop =  hei - bigY, sqLeft = smallX, sqBot = hei - smallY, sqRight = bigX}

pRectToSqNoSwap :: PopPRectangle -> Sq Double
pRectToSqNoSwap (PopPRectangle smallX smallY bigX bigY) = CSq {sqTop =  smallY, sqLeft = smallX, sqBot = bigY, sqRight = bigX}

-- docPathSuffix = "./pdfs/CTFP.pdf"
replaceStr :: Turtle.Text -> Turtle.Text -> String -> String
replaceStr nd ns str = Text.unpack $ Text.replace nd ns $ Text.pack str




stanIOPoppy1 :: [Char] -> IO [Turtle.Text]
stanIOPoppy1 str = do
  conv <- open "UTF8" Nothing
  let
    text = fromUnicode conv $ Text.pack str
  req <- HT.setRequestMethod "POST" <$> HT.parseRequest command
  req2 <-buildRequest command $ RequestBodyBS text
  jsonString <- responseBody <$> HT.httpLbs req2
  let
    bulkResult = ICU.toUnicode conv $ BL.toStrict jsonString
    splitted = Text.split (== '\n') bulkResult
    delimited = delimitAtWO2By (\a -> Text.take (Text.length initWord) a == initWord) id splitted
      where
        initWord = "      \"index\":"
    render tarText = Text.replace "\\n" "\n" $ f $ Text.drop 3 $ Text.dropWhile (\c -> not $ c == ':') tarText
      where
        f bulk = Text.take (Text.length bulk - 2) bulk
    stanRess = map (render . head) $ tail delimited
  return  stanRess


sqsTakeTop sqs = maximum $ map sqTop sqs
sqsTakeLeft sqs = maximum $ map sqLeft sqs
sqsTakeBot sqs = maximum $ map sqBot sqs
sqsTakeRight sqs = maximum $ map sqRight sqs

average :: Fractional a => Num a => [a] -> a
average ts = (sum ts) / (fromIntegral $ length ts)


fetch :: [a] -> [Int] -> [a]
fetch xs ns = map (\n -> xs !! n) ns

fetchMap :: (a -> b) -> [a] -> [Int] -> [b]
fetchMap f xs ns = map (\n -> f $ xs !! n) ns


data TokSq a b = CTokSq { tsId :: Int
                        , tsRawVal :: a
                        , tsRawSqs :: [Sq b]
                        , tsValTable :: [(a, [Int])]
                        , tsIncOrphs :: [(a, Int)]}
                          deriving (Show, Eq, Ord)

data Pos a = CPos {posX :: a, posY :: a}
  deriving (Show, Eq, Ord)

data Sq a = CSq {
   sqTop :: a
 , sqLeft :: a
 , sqBot :: a
 , sqRight :: a}
  deriving (Show, Eq, Ord)

sqToRect :: Sq Double -> IO (GPop.Rectangle)
sqToRect sq = do
  rect0 <- GPop.rectangleNew
  let
    x1 = sqLeft sq
    x2 = sqRight sq
    y1 = sqTop sq
    y2 = sqBot sq
  set rect0 [#x1 := x1,  #y1 := y2, #x2 := x2,  #y2 := y1]
  return rect0

swapRectAns page rect = do
  rect0 <- GPop.rectangleNew
  (col, row) <- GPop.pageGetSize page
  x1 <- GPop.getRectangleX1 rect
  x2 <- GPop.getRectangleX2 rect
  y1 <- GPop.getRectangleY1 rect
  y2 <- GPop.getRectangleY2 rect
  set rect0 [#x1 := x1,  #y1 := row - y2, #x2 := x2,  #y2 := row - y1]
  return rect0

gpopRectToPopPRect :: GPop.Page -> GPop.Rectangle -> IO PopPRectangle
gpopRectToPopPRect page rect = do
  -- rect <- swapRectAns page rect0
  x1Prim <- GPop.getRectangleX1 rect
  x2Prim <- GPop.getRectangleX2 rect
  y1Prim <- GPop.getRectangleY1 rect
  y2Prim <- GPop.getRectangleY2 rect
  let
    (x1, x2)
     | x1Prim < x2Prim = (x1Prim, x2Prim)
     | otherwise = (x2Prim, x1Prim)
    (y1, y2)
     | y1Prim < y2Prim = (y1Prim, y2Prim)
     | otherwise = (y2Prim, y1Prim)
  -- return ((x1, y1), (x2, y2))
  return $ PopPRectangle x1 y1 x2 y2

showRect :: GPop.Rectangle -> IO (Sq Double)
showRect rect = do
  x1Prim <- GPop.getRectangleX1 rect
  x2Prim <- GPop.getRectangleX2 rect
  y1Prim <- GPop.getRectangleY1 rect
  y2Prim <- GPop.getRectangleY2 rect
  let
    (x1, x2)
     | x1Prim < x2Prim = (x1Prim, x2Prim)
     | otherwise =  (x2Prim, x1Prim)
    (y1, y2)
     | y1Prim < y2Prim = (y1Prim, y2Prim)
     | otherwise =  (y2Prim, y1Prim)
  -- return ((x1, y1), (x2, y2))
  return $ CSq {sqTop = y1, sqLeft = x1, sqBot = y2, sqRight = x2}

cshowIL :: Show b => [b] -> IO ()
cshowIL lis = mapM_ uprint $ zip [0 .. (length lis - 1)] lis

cshowL :: (Foldable t, Show a) => t a -> IO ()
cshowL lis = mapM_ uprint  lis

cshowI :: Show b => V.Vector b -> IO ()
cshowI lis = cshowIL $ V.toList lis

cshow :: Show a => V.Vector a -> IO ()
cshow lis = cshowL $ V.toList lis

swapEtFlatten :: [(a, [b])] -> [(b, a)]
swapEtFlatten lis = concatMap (\x@(xa, xbs) -> map (\xb -> (xb, xa)) xbs) lis

isSqIncludePoint :: Ord a => Num a => Sq a -> Pos a -> Bool
isSqIncludePoint sq pos = isInnerX && isInnerY
  where
    isInnerX = sqLeft sq <= (posX pos) && (posX pos) <= sqRight sq
    isInnerY = sqTop sq <= posY pos && posY pos <= sqBot sq

isSqIncludeSq :: Ord a => Num a => Sq a -> Sq a -> Bool
isSqIncludeSq sqs sqd = isInnerX && isInnerY
  where
    isInnerX = sqLeft sqs <= sqLeft sqd && sqRight sqd <= sqRight sqs
    isInnerY = sqTop sqs <= sqTop sqd && sqBot sqd <= sqBot sqs

takeFstL :: [(a, b)] -> [a]
takeFstL = map fst

takeSndL :: [(a, b)] -> [b]
takeSndL = map snd

command = "http://localhost:9000/?annotators=parse&outputFormat=json&timeout=50000"
text = "The quick brown fox jumped over the lazy dog."

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  nakedRequest <- parseRequest url
  return (nakedRequest { method = "POST", requestBody = body })


forgetIndexed :: Turtle.Text -> [([Int], Tag, Maybe String)]
forgetIndexed res
  | parsed == [] = error errorMesse
  | otherwise = forgotten
  where
    reshaped = reshapeSexp $ V.fromList $ map Text.unpack $ Text.split (== '\n')  res
    parsed = parse pSExp reshaped
    errorMesse = "error: cannot parse pSExp reshaped\n" ++ "reshaped = " ++ (show reshaped)
    -- sexp =  fst $ head $ parse pSExp reshaped
    sexp =  modifyLastNPConj $ modifyPossession $ fst $ head $ parse pSExp reshaped
    -- sexp =  modifyPossession $ fst $ head $ parse pSExp reshaped
    indexed = indexingSP sexp
    forgotten = map (\x@((x1,x2),x3) -> (x1,x2,x3)) $ forgetSExp indexed

isFirst :: Diff a -> Bool
isFirst a = case a of
  First _ -> True
  _ -> False
isSecond :: Diff a -> Bool
isSecond a = case a of
  Second _ -> True
  _ -> False
isBoth :: Diff a -> Bool
isBoth a = case a of
  Both _ _ -> True
  _ -> False

assignByDiff :: Eq a => Eq b => V.Vector b -> V.Vector (b, V.Vector a) -> V.Vector (b, V.Vector (V.Vector a))
assignByDiff strs tupsPrim
  | strs == V.empty = V.empty
  | tupsPrim == V.empty = V.empty
  | otherwise = inpNubbed
  where
    tupsStrs = takeFst tupsPrim
    tupsVals = takeSnd tupsPrim
    diff = vGetDiffBy (==) tupsStrs strs
    assigned = fst $ V.foldl' f (V.empty, tupsPrim) diff
      where
        f y@(stacked, consumed) x =
          case x of
            First a -> (V.snoc stacked (x, Just (snd $ V.head consumed)), V.tail consumed)
            Second b -> (V.snoc stacked (x, Nothing), consumed)
            Both a b -> (V.snoc stacked (x, Just (snd $ V.head consumed)), V.tail consumed)
    interposed = fst $ V.foldl' g (V.empty, V.singleton $ snd $ V.head assigned) assigned
      where
        g y@(stacked, keepVal) x@(dif, val) =
          case dif of
            First a -> (stacked, V.snoc keepVal val)
            Second b -> (V.snoc stacked (b, keepVal), keepVal)
            Both a b -> (V.snoc stacked (b, V.singleton val), V.singleton val)
    inpNubbed = V.map (\x@(x1, x2) ->  (x1, h3 $ h $ vNub x2)) interposed
      where
        h vs = V.filter (\z -> not $ z == V.empty) $ V.map h2 vs
        h2 mb =
          case mb of
            Just a -> a
            Nothing -> V.empty
        h3 vs
          | vs == (V.singleton V.empty) = V.empty
          | otherwise = vs

vGetDiffBy :: (t -> t -> Bool) -> V.Vector t -> V.Vector t -> V.Vector (Diff t)
vGetDiffBy f vs1 vs2 = V.fromList $ getDiffBy f (V.toList vs1) (V.toList vs2)

{-
  execShell "sudo docker run -p 9000:9000 nlpbox/corenlp"
-}

getRectSW page lin opts = do
  resPrim@(txt,rectsPrim) <- (\x -> (lin, x)) <$> GPop.pageFindTextWithOptions page lin opts
  rects <- sequence $ map (swapRectAns page) rectsPrim
  sqs <- mapM showRect $ rects
  return (txt, sqs)

indexing :: [b] -> [(Int, b)]
indexing xs = zip [0 .. (length xs - 1)] xs

takeLeft :: ((a, b1), b2) -> a
takeLeft x = fst $ fst x

takeTop :: ((a, b1), b2) -> b1
takeTop x = snd $ fst x

takeBot :: (a1, (a2, b)) -> a2
takeBot x = fst $ snd x

takeRight :: (a1, (a2, b)) -> b
takeRight x = snd $ snd x

flattenSnd :: Foldable t => t (a, [b]) -> [(a, b)]
flattenSnd xs = concatMap (\x -> map (\y -> (fst x, y)) $ snd x) xs

isIncludePoint :: Ord a => (a, a) -> Sq a -> Bool
isIncludePoint poi@(r, c) sq = sqLeft sq <= c && c <= sqRight sq && sqTop sq <= r && r <= sqBot sq

fromTokensToSens :: V.Vector String -> String
fromTokensToSens arr
  | arr == V.empty = ""
  | otherwise = V.foldl' f (V.head arr) (V.tail arr)
    where
      f y x = y ++ " " ++ x

fromTokensToSensJP :: V.Vector String -> String
fromTokensToSensJP arr
  | arr == V.empty = ""
  | otherwise = V.foldl' f (V.head arr) (V.tail arr)
    where
      f y x = y ++ x

getMData :: String -> MData
getMData str = res
  where
    tok = takeWhile (\c -> not $ c == '\t') str
    tags
      | dropped == [] = []
      | otherwise = delimitAtWO2 ',' id $ tail dropped
      where
        dropped = dropWhile (\c -> not $ c == '\t') str
    tagsInterp = tags ++ (replicate 9 "")
    res = CMData {
        mToken = tok
      , mTag = tagsInterp !! 0
      , mTag1 = tagsInterp !! 1
      , mAttr1 = tagsInterp !! 2
      , mAttr2 = tagsInterp !! 3
      , mGr1 =  tagsInterp !! 4
      , mGr2 = tagsInterp !! 5
      , mVoc1 = tagsInterp !! 6
      , mVoc2 = tagsInterp !! 7
      , mVoc3 = tagsInterp !! 8
      }

get2 :: (Turtle.MonadIO m) => Text.Text -> m [Turtle.Line]
get2 cmd = do
  Turtle.fold (Turtle.inshell cmd Turtle.empty) Fold.list

tesseractPartlyIOJP :: String -> IO (V.Vector (V.Vector (String, V.Vector Square)))
tesseractPartlyIOJP pathWithFileName = undefined
{-
  hoge =  do
      img@(rawImg, (rowPrim, colPrim)) <- iCharColor pathWithFileName
      let
        woExt = P.takeWhile (\c -> not $ c == '.') $ P.reverse $ P.takeWhile (\c -> not $ c == '/') $ P.reverse pathWithFileName
        docName = P.takeWhile (\c -> not $ c == '_') woExt
        nPrim = read $ P.reverse $ P.takeWhile (\c -> not $ c == '_') $ P.reverse woExt :: Int
        n = nPrim - 1
      tokSqsRatios <- popplerTokenizePartlyIOJP docName n
      let
        tokSqs tokSqsRatio = V.map (\x@(str, sqs) -> (str, V.map f sqs)) tokSqsRatio
          where
            row = fromIntegral rowPrim
            col = fromIntegral colPrim
            f ((r1, c1), (r2, c2)) = ((floor $ r1 * row, floor $ c1 * col), (floor $ r2 * row, floor $ c2 * col))
      return $ V.map tokSqs tokSqsRatios
-}

tessStanIOJP :: String -> Bool -> IO (V.Vector (V.Vector (String, (String, String))), V.Vector (V.Vector String), (V.Vector (V.Vector (String, V.Vector Square))))
tessStanIOJP pathWithFileName isLinux = do
  presNubPrim <- iVecFromFile "tessPresDiv.txt"
  postNubPrim <- iVecFromFile "tessPostDiv.txt"
  tessResPrim2Prim <- tesseractPartlyIOJP pathWithFileName
  let
    tessResPrim2 = V.concatMap f tessResPrim2Prim
      where
        f vs = res
          where
            sepIndices = V.findIndices (\x@(str, _) -> str == " ") vs
            added
              | vs == V.empty = V.empty
              | otherwise = vNub $ V.snoc (V.cons (-1) sepIndices) (V.length vs)
            spans
              | added == V.empty = V.empty
              | otherwise = V.zip (V.init added) (V.tail added)
            res = V.map g spans
              where
                g (i, j) = V.map (\k -> vs V.! k) $ V.fromList [(i + 1) .. (j - 1)]
    presNub = V.map (\x -> read x) presNubPrim :: V.Vector (String, String)
    postNub = V.map (\x -> read x) postNubPrim :: V.Vector (String, String)
    tessResPPrim = V.filter (\x@(c, sqs) -> not $ c == "\n") $ V.concatMap id tessResPrim2
    f tessResPrimTemp = do
      let
        sens = Text.unpack $ Text.replace "\n" "" $ Text.pack $ fromTokensToSensJP $ takeFst tessResPrimTemp :: String
      (rawST2Prim, stParsed2Prim) <- getNPStructureJP sens isLinux
      return (rawST2Prim, stParsed2Prim)
  resres <- V.mapM f tessResPrim2
  let
    rawST2Prim = V.filter (\x -> not $ mToken x == "EOS") $ V.concatMap id $ takeFst resres
    stParsed2Prim = V.map (\x -> V.filter (\y -> not $ mToken y == "EOS") x) $ V.concatMap id $ takeSnd resres
    rawST2 = V.map (\y -> V.map (\x -> (mToken x, (mTag x, mTag1 x))) y) stParsed2Prim -- forDummy
    stParsed = V.map (\x -> V.map mToken x) stParsed2Prim

    tessRes = replaceEtDivideHead presNub $ replaceEtDivideLast postNub tessResPPrim
    stData = assSqsToPhraseNeoJP stParsed tessRes :: V.Vector (V.Vector (String, V.Vector Square))
    stDataSyned = V.map (\xs -> V.singleton (f $ takeFst xs, V.concatMap id $ takeSnd xs)) stData
      where
        f ys = concat $ V.toList ys
  return (rawST2, stParsed, stDataSyned)

replaceEtDivideHead :: Eq a => V.Vector (String, String) -> V.Vector (String, a) -> V.Vector (String, a)
replaceEtDivideHead replaceTable vs
  | res == vs = vs
  | otherwise = replaceEtDivideHead replaceTable res
  where
    res = V.concatMap f vs
    f x@(str, sqs)
     | not $ founds == V.empty = V.fromList [(fSnd, sqs), (drop lenOfFoundsHFst str, sqs)]
     | otherwise = V.singleton (str, sqs)
        where
          founds = V.filter g replaceTable
            where
              g tabl@(hStr, replans)
               | lenOfStr < lenOfHStr = False
               | lenOfStr == lenOfHStr = False
               | hStr == taken = True
               | otherwise = False
               where
                 lenOfStr = length str
                 lenOfHStr = length hStr
                 taken = take lenOfHStr str
          foundsHead@(fFst, fSnd) = V.head founds :: (String, String)
          lenOfFoundsHFst = length fFst

replaceEtDivideLast :: Eq a => V.Vector (String, String) -> V.Vector (String, a) -> V.Vector (String, a)
replaceEtDivideLast replaceTable vs
  | res == vs = vs
  | otherwise = replaceEtDivideLast replaceTable res
  where
    res = V.concatMap f vs
    f x@(str, sqs)
     | not $ founds == V.empty = V.fromList [(reverse $ drop lenOfFoundsHFst $ reverse str, sqs), (fSnd, sqs)]
     | otherwise = V.singleton (str, sqs)
        where
          founds = V.filter g replaceTable
            where
              g tabl@(lStr, replans)
               | lenOfStr < lenOfHStr = False
               | lenOfStr == lenOfHStr = False
               | lStr == taken = True
               | otherwise = False
               where
                 lenOfStr = length str
                 lenOfHStr = length lStr
                 taken = reverse $ take lenOfHStr $ reverse str
          foundsHead@(fFst, fSnd) = V.head founds :: (String, String)
          lenOfFoundsHFst = length fFst

assSqsToPhraseNeoJP :: V.Vector (V.Vector String)
  -> V.Vector (String, V.Vector Square)
  -> V.Vector (V.Vector (String, V.Vector Square))
assSqsToPhraseNeoJP stParsed tessRes = result3
  where
    eachLength = V.map V.length stParsed :: V.Vector Int -- For Strings
    flattened = V.concatMap id stParsed :: V.Vector String
    eachLength2 = V.map length flattened :: V.Vector Int -- For Chars
    flattened2 = V.map (\x -> [x]) $ V.fromList $ concatMap id flattened :: V.Vector String
    tessResFlattened = V.concatMap f tessRes :: V.Vector (String, V.Vector Square)
      where
        f :: (String, V.Vector Square) -> V.Vector (String, V.Vector Square)
        f x@(str, sqs) = V.fromList $ map (\c -> ([c], sqs)) str
    strs = flattened2
    --tupsPrim = tessRes
    tupsPrim = tessResFlattened
    ass3Syned = V.map (\x@(x1, x2) -> (x1, V.concatMap id x2)) assBySlide3
      where
         assBySlide3 = assignByDiff strs tupsPrim
    reformed@(pl, rs) = V.foldl' f (V.empty, ass3Syned) eachLength2
      where
        f y@(pooled, residue) lenOfJag = (pooled V.++ (V.singleton $ V.take lenOfJag residue), V.drop lenOfJag residue)
    result = pl V.++ (V.singleton rs)
    reformed2@(pl2, rs2) = V.foldl' f (V.empty, result) eachLength
      where
        f y@(pooled, residue) lenOfJag = (pooled V.++ (V.singleton $ V.take lenOfJag residue), V.drop lenOfJag residue)
    result2 = pl2 V.++ (V.singleton rs2)
    result3 = V.map (\xs -> V.map (\ys -> (concatMap id $ takeFst ys, V.concatMap id $ takeSnd ys) ) xs) result2

-- this corresponds to stanIOJP(from poppy1)
getNPStructureJP :: String -> Bool -> IO (V.Vector MData, (V.Vector (V.Vector MData)))
getNPStructureJP sens isLinux = do
  pairs <- getMecabed sens
  let
    parsed = parseNPsJP1 pairs
    parsed2 = parseForPatent parsed
  return (pairs, parsed2)

parseForPatent :: V.Vector (V.Vector MData) -> V.Vector (V.Vector MData)
parseForPatent parsed = anotherParsed
  where
    tokenAbstracted = V.map (\x -> (f x, x)) parsed
       where
         f mds = concat $ V.toList $ V.map mToken mds
    anotherParsed = V.snoc pl (V.concatMap id st)
       where
         folded@(st,pl) = V.foldl' g (V.empty, V.empty) tokenAbstracted
         g :: (V.Vector (V.Vector MData), V.Vector (V.Vector MData)) -> (String, V.Vector MData) -> (V.Vector (V.Vector MData), V.Vector (V.Vector MData))
         g y@(stacked, pooled) x@(tok, dat) = res
           where
             res
              | V.length stacked == 2 = (V.empty, V.snoc pooled (V.concatMap id $ V.snoc stacked (snd x)))
              | V.length stacked == 1 && tok == "の" = (V.snoc stacked (snd x), pooled)
              | stacked == V.empty && (not $ onlyNumberApplicant == "") && isApAllNums = (V.snoc stacked (snd x), pooled)
              | otherwise = (V.empty, V.snoc pooled $ snd x)
               where
                onlyNumberApplicant :: String
                onlyNumberApplicant
                 | take 3 (Text.unpack $ Text.replace "\"" "" $ Text.pack tok) == "前記第" = drop 3 tok
                 | take 1 (Text.unpack $ Text.replace "\"" "" $ Text.pack tok) == "第" = drop 1 tok
                 | otherwise = ""
                isApAllNums = V.and $ V.fromList $ map (\c -> elem [c] nums) onlyNumberApplicant
                  where
                    nums = (map show [0 .. 9]) ++ ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "百", "１", "２", "３", "４", "５", "６", "７", "８", "９", "０"]

getMecabed :: String -> IO (V.Vector MData)
getMecabed sens = do
  let
    ngWords = ["~", "`", "#", "$", "&", "*", "(", ")", "\\", "|", "[", "]", "{", "}", ";", "'", "\"", "<", ">", "/", "?", "!"]
    replaced = Text.unpack $ foldl (\y -> \x -> Text.replace x "" y) (Text.pack sens) ngWords
    -- cmd = Turtle.fromString $ "echo " ++ "\""  ++ sens ++ "\"" ++ " | mecab"
    cmd = Turtle.fromString $ "echo " ++ "\""  ++ replaced ++ "\"" ++ " | mecab"
    chcp = Turtle.fromString "chcp 65001" -- for windows encoding
  -- putStrLn os
  if os == "mingw32"
    then do
     _ <- get2 chcp
     return ()
    else return ()
  res2 <- fmap (Text.unpack . Turtle.lineToText) <$> get2 cmd
  return $ V.fromList $ map getMData res2

getGinzad sens = do
  let
    cmd = Turtle.fromString $ "echo " ++ "\""  ++ sens ++ "\"" ++ " | ginza"
  res2 <- fmap (Text.unpack . Turtle.lineToText) <$> get2 cmd
  return res2

getGinzameSh :: String -> IO [(String, String)]
getGinzameSh sens = do
  let
    cmd = Turtle.fromString $ "echo " ++ "\""  ++ sens ++ "\"" ++ " | ginzame"
  res2 <- fmap (Text.unpack . Turtle.lineToText) <$> get2 cmd
  return $ map (\x -> (mToken x, mTag x)) $ map getMData res2


toRawSExpList :: V.Vector MData -> [([Int], String, Maybe String)]
toRawSExpList mData = V.toList $ V.map (\md -> ([], mTag md, Just (mToken md))) mData


parseNPsJP1 :: V.Vector MData -> V.Vector (V.Vector MData)
parseNPsJP1 pairs
 | pairs == V.empty = V.empty
 | otherwise = V.snoc pooled stacked
    where
      resPrim@(stacked, pooled) = V.foldl' f (V.singleton $ V.head pairs, V.empty) (V.tail pairs)
         where
           nouns = V.fromList ["名詞", "形容詞", "接頭詞", "連体詞"]
           postNounsNouns = V.fromList ["接尾辞"]
           f y@(stacked, pooled) x
             | V.elem (mTag x) nouns && (not $ stacked == V.empty) && (V.elem lTag nouns) = (V.snoc stacked x, pooled)
             | otherwise = (V.singleton x, V.snoc pooled stacked)
                 where
                   lTag = mTag $ V.last stacked

mDataRoot = CMData {mToken = "Root"
                  , mTag = "ROOT"
                  , mTag1 = "ROOT"
                  , mAttr1 = "ROOT"
                  , mAttr2 = "ROOT"
                  , mGr1 = "ROOT"
                  , mGr2 = "ROOT"
                  , mVoc1 = "ROOT"
                  , mVoc2 = "ROOT"
                  , mVoc3 = "ROOT"}

makeNode tok tag = CMData {mToken = tok
                  , mTag = tag
                  , mTag1 = ""
                  , mAttr1 = ""
                  , mAttr2 = ""
                  , mGr1 = ""
                  , mGr2 = ""
                  , mVoc1 = ""
                  , mVoc2 = ""
                  , mVoc3 = ""}

testestes22 = do
  res2 <- getGinzad "こちらも、トグル中にはGlobalConfigにその結果は常に返されているので、そちらのみ見ておけばいいことになるから。"
  res3 <- getGinzad "こちらも、それにはGlobalConfigにその結果は常に返されているので、そちらのみ見ておけばいいことになるから。"
  let
    trueRes = tail $ init res2
    resSepped = map (delimitAtWO2 '\t' id) trueRes
    trueRes2 = tail $ init res3
    resSepped2 = map (delimitAtWO2 '\t' id) trueRes2
    resSome = map (\x -> (x !! 0, x !! 6, x !! 1, x !! 3, x !! 5)) resSepped
    aa = map (\cs -> tail $ dropWhile (\c -> not $ c == '\t') cs) $ init $ tail res2
    bb = map (\cs -> (takeWhile (\c -> not $ c == '\t') cs, cs)) aa
    folded1 = deficit
      where
        deficit
          | length bb < 2 = []
          | otherwise = capsuls
             where
               toks = map fst bb
               indices =  Lis.sort $ map (\n -> take (length toks - 2) $ Lis.sortBy g $ (n, "これ") : (map (\m2 -> (m2, fst $ bb !! m2)) $ filter (\m -> not $ elem m [n, n + 1]) [0 .. (length toks - 1)])) $ [0 .. (length toks - 2)]
                 where
                   g x y = compare (fst x) (fst y)
               capsuls = map (concat . takeSndL) indices
  resGinzad <- mapM getGinzad folded1
  let
    deficit = resGinzad
    mds3 = res2
    deficit3 = deficit
    res = map (\xs -> getDiff xs mds3 ) deficit3
    f n = do
        let
          res0 = V.toList $ V.imap (\i -> \xs -> zip xs (take i mds3 ++ drop (i + 1) mds3) ) $ V.fromList deficit3
          sen = res0 !! n
          g pair = do
            uprint $ fst pair
            uprint $ snd pair
            putStrLn ""
        mapM g sen
  putStrLn "hogehogehoge"

data Ginza = CGinza { ginId :: Int
                    , ginTok :: String
                    , ginTokFine :: String
                    , ginPOSEng :: String
                    , ginPOSJP :: String
                    , ginETC :: String
                    , ginDep :: Int
                    , ginPOSEng1 :: String
                    , ginETC1 :: String
                    , ginExtra :: String
                    }
                       deriving (Show, Eq, Ord, Read)

createNPGinza i i1 = resG
  where
    resG = CGinza { ginId = i
                  , ginTok = "それ"
                  , ginTokFine = "其れ"
                  , ginPOSEng = "PRON"
                  , ginPOSJP = "代名詞"
                  , ginETC = "_"
                  , ginDep = i1
                  , ginPOSEng1 = "subst"
                  , ginETC1 = "_"
                  , ginExtra = "_"
                  }

toGinza rawStr = resG
  where
    res = delimitAtWO2 '\t' id rawStr
    resG = CGinza { ginId = (-1) + read (res !! 0) :: Int
                  , ginTok = res !! 1
                  , ginTokFine = res !! 2
                  , ginPOSEng = res !! 3
                  , ginPOSJP = res !! 4
                  , ginETC = res !! 5
                  , ginDep = (-1) + read (res !! 6) :: Int
                  , ginPOSEng1 = res !! 7
                  , ginETC1 = res !! 8
                  , ginExtra = res !! 9
                  }

testes3 = do
  let
    str = "上のでどの箇所がシュリンクしうるかがわかっているので、実際にそれを置き換える"
  resres@(shIDs, ginzas) <- shrinkGinzaIO str
  let
    furked
      | shIDs == [] = []
      | length shIDs == 1 = [shIDs]
      | otherwise = [pl0 ++ [st0] | pl0 <- pl, st0 <- st ++ [k]]
      where
        res@(k, st, pl) = foldl g (head shIDs, [], []) $ tail shIDs
        g (j, stacked, pooled) i
         | j + 1 == i = (i, stacked ++ [j], pooled)
         | pooled == [] = (i, [], [[st0] | st0 <- stacked ++ [j]])
         | otherwise = (i, [], [pl0 ++ [st0] | pl0 <- pooled, st0 <- stacked ++ [j]])
    makeSh2 ns = map g2 [0 .. length ginzas - 1]
      where
        g2 m
         | elem m ns = ["此れ"]
         | elem m $ map (+1) ns = []
         | otherwise = [ginTok $ ginzas !! m]
    nextSensPrim = map (map head . filter (\x -> not $ x == []) . makeSh2) furked
    -- fst is next Sentense, snd is position of "此れ"(shrinked position of nextSensPrim)
    nextSensWShId = map ff  nextSensPrim
      where
        ff nextSen = (snd res, filter (\x -> 1 < length x) $ fst res)
          where
            res
              | resKoreIds == [] = (resSt, resStTok)
              | otherwise = (snocL resSt resKoreIds, resStTok ++ "此れ")
            res2@(resKoreIds, (resSt, resStTok)) = foldl f2 ([], ([], "")) $ indexing nextSen
            f2 y@(koreIds, (st, stTok)) x@(i, tok)
              | (not $ koreIds == []) && (tok == "此れ") = (snocL koreIds i, (st, stTok))
              | (not $ koreIds == []) && (not $ tok == "此れ") = ([], (snocL st koreIds, stTok ++ "此れ" ++ tok))
              | (koreIds == []) && (tok == "此れ") = ([i], (st, stTok))
              | otherwise = ([], (st, stTok ++ tok))
  resres2s <- mapM shrinkGinzaIO $ takeFstL nextSensWShId
  putStrLn ""

snocL xs x = xs ++ [x]

shrinkGinzaIO str = do
  resRaw <- getGinzad str
  let
    trueRes = tail $ init resRaw
    resSepped = map toGinza trueRes
    patterns
     | length resSepped < 3 = []
     | otherwise = result
      where
        indices = [0 .. (length resSepped - 2)]
        result = map f indices
           where
             f i = substituted
              where
                prev
                 | i == 0 = []
                 | otherwise = [0 .. (i - 1)]
                succ
                 | i == (length resSepped - 2) = []
                 | otherwise = [(i + 2) .. (length resSepped - 1)]
                substTable = (-1, -1) : map (\j -> (j, j)) prev ++ [(i, i), (i + 1, i)] ++ map (\j -> (j, j - 1)) succ
                prevSepped = fetch resSepped prev
                succSepped = fetch resSepped succ
                substituted = map g prevSepped ++ [createNPGinza i (ginDep $ g $ resSepped !! i)] ++ map g succSepped
                  where
                    g ginz = ginz {ginId = giddTrans, ginDep = ginDeppTrans}
                      where
                        gidd = ginId ginz
                        ginDepp = ginDep ginz
                        giddTrans = snd $ head $ filter (\x@(x1, x2) -> gidd == x1) substTable
                        ginDeppTrans = snd $ head $ filter (\x@(x1, x2) -> ginDepp == x1) substTable
    sensR = map g2 patterns
      where
        g2 gz = concatMap ginTok gz
  resRecsRaw <- mapM getGinzad sensR
  let
    resRecs = map (map toGinza . tail . init) resRecsRaw
    zipped = zip patterns resRecs
    diffs = map (\x@(x1, x2) -> getDiff x1 x2) zipped
    isSuccess gins = isFst1 && isSnd1 && isSucc
       where
         firsts = filter isFirst gins
         seconds = filter isSecond gins
         isFst1 = length firsts == 1
         isSnd1 = length seconds == 1
         idFst = case head firsts of
           First a -> ginId a
           Second a -> ginId a
           Both a b -> ginId a
         idSnd = case head seconds of
           First a -> ginId a
           Second a -> ginId a
           Both a b -> ginId a
         isSucc = idFst == idSnd
    succeedRes = filter (\x -> isSuccess (snd x))$ indexing diffs
    reshaped = map g2 succeedRes
      where
        g2 (i, gins) = (i, map g3 $ filter (\x -> not $ isSecond x) gins)
        g3 x = case x of
          First a -> a
          Second a -> a
          Both a b -> a
  return (takeFstL reshaped, resSepped)

foreign import ccall "poppler_index_iter_new" poppler_index_iter_new ::
  Ptr GPop.Document -> IO (Ptr GPop.IndexIter)

retrIndexTree doc = do
  -- doc <- GPop.documentNewFromFile (Text.pack "file:///home/polymony/Desktop/Professional CUDA C Programming.pdf") Nothing
  -- doc <- GPop.documentNewFromFile (Text.pack "file:///home/polymony/Desktop/poppySBookVol1_gisho10_20201220Ver7.pdf") Nothing
  docRaw <- unsafeManagedPtrGetPtr doc
  iterRaw <- poppler_index_iter_new docRaw
  let
    isNoIter = iterRaw == nullPtr
  if isNoIter
    then return []
    else do
      iter <- GPop.indexIterNew doc
      indexTreePrim <- fromIter doc [] iter [0]
      let
        indexTree = map (\x@(x1, (x2, x3)) -> (x1, (f "" x2,f 0 x3))) indexTreePrim
          where
            f v0 vMaybe = case vMaybe of
              Nothing -> v0
              Just v -> v
      return indexTree

foreign import ccall "poppler_index_iter_get_child" poppler_index_iter_get_child ::
    Ptr GPop.IndexIter -> IO (Ptr GPop.IndexIter)

fromIter doc alrdList iter currIndices = do
  act <- GPop.indexIterGetAction iter
  let
    f0 :: Maybe a -> (a -> IO b) -> IO (Maybe b)
    f0 a g = case a of
      Nothing -> return Nothing
      Just a0 -> do
        res <- g a0
        return (Just res)
    f1 :: Maybe a -> (a -> IO (Maybe b)) -> IO (Maybe b)
    f1 a g = case a of
      Nothing -> return Nothing
      Just a0 -> g a0
  atype <- GPop.getActionType act
  currTitleEtNPage <- do
      if atype == GPop.ActionTypeGotoDest
        then do
          actGotoDest <- GPop.getActionGotoDest act
          dest <- GPop.getActionGotoDestDest actGotoDest
          dtype <- f0 dest GPop.getDestType
          title <- GPop.getActionGotoDestTitle actGotoDest
          destName <- f1 dest GPop.getDestNamedDest
          destNamed <- f0 destName (GPop.documentFindDest doc)
          destPageNum0 <- f0 destNamed GPop.getDestPageNum
          destPageNum1 <- f0 dest GPop.getDestPageNum
          let
            destPageNum
             | destPageNum1 == Nothing = destPageNum0
             | destPageNum1 == Just 0 = destPageNum0
             | otherwise = destPageNum1
          return (title, destPageNum)
{-
          if dtype == Just GPop.DestTypeNamed
            then do
              title <- GPop.getActionGotoDestTitle actGotoDest
              destName <- f1 dest GPop.getDestNamedDest
              destNamed <- f0 destName (GPop.documentFindDest doc)
              destPageNum <- f0 destNamed GPop.getDestPageNum
              return (title, destPageNum)
            else return (Nothing, Nothing)
-}
        else return (Nothing, Nothing)
  let
    alrdNew = snocL alrdList (currIndices, currTitleEtNPage)
  iterRaw <- unsafeManagedPtrGetPtr iter
  childRaw <- poppler_index_iter_get_child iterRaw
  let
    isHasChild = not $ childRaw == nullPtr
    retChild = do
      if isHasChild
        then do
          child <- GPop.indexIterGetChild iter
          childConcated <- fromIter doc alrdNew child (snocL currIndices 0)
          return childConcated
        else
          return alrdNew
  res <- retChild
  isHasNext <- GPop.indexIterNext iter
  if isHasNext
    then do
     let
       nextIndexLast = last currIndices + 1
     res2 <- fromIter doc res iter (snocL (init currIndices) nextIndexLast)
     return res2
    else do
      return res
