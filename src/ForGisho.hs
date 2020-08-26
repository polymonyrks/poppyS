{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module ForGisho where
-- import FromPDF
import qualified Data.Text as Text
import qualified Turtle
import qualified Control.Foldl as Fold
import Lib (iVecFromFile, iVecFromFileJP, oVecToFile, oVecToFileJP, delimitAtWO2, delimitAtWO2By, vNub, takeFst, takeSnd, takeFstT, takeSndT, takeThdT, execShell, delimitAtW2, getDivideLine, countPrefix, countSuffix, indexingL, oFileJP)
import PopSExp (indexingSP, forgetSExp, injectSExpI, reconsSExp, showSP, mapNode, isBottomBy, takeSpecTags)
import SExp
import PopSExp
import Text.Show.Unicode
import qualified Data.Vector as V
import System.Info


-- *************************************************************************
-- 「Haskellでつくる不思議な読書体験用」

testSentense = "本書はPDFリーダーを自身の手で作ることを通してプログラミング言語Haskellを学ぶ本の第一弾です。"

readSample = toSExpsJP <$> getMecabed testSentense

runTest = do
  sexp0 <- readSample
  let
    -- result  = foldNPsTest sexp0
    result  = forgetNPSubs $ foldNPsTest sexp0
  showSP $ mapNode (\x -> (jTag x, jTag1 x, jTagIsNP x)) id result

runTestHelp = do
  sexp0 <- readSample
  let
    result  = (foldPrefix . foldSameTags0) sexp0
  return result


data JTag = CJTag {
    jTag :: String
  , jTag1 :: String
  , jTagIsNP :: Bool
 }
  deriving (Show, Eq, Ord, Read)

 -- SExp'は説明用で実際は使いません
data SExp' a b where
  Nil'  ::                    SExp' a b
  Atom' :: a -> b          -> SExp' a b
  Opr'  :: a -> [SExp' a b] -> SExp' a b


 -- SExp''は説明用で実際は使いません
data SExp'' where
  Nil''  ::                    SExp''
  Atom'' :: JTag -> String  -> SExp''
  Opr''  :: JTag -> [SExp''] -> SExp''

foldNPsTest :: SExp JTag String -> SExp JTag String
foldNPsTest = foldNo . foldPrefix . foldSameTags0
  where

-- 「不思議な読書体験」の書籍を読み進める際は、foldNo1 sexpをundefinedに書き換えてからスタートします。
foldNo :: SExp JTag String -> SExp JTag String
foldNo sexp = foldNo1 sexp

snocL xs x = xs ++ [x]

nilJTag  = CJTag {jTag = "nil", jTag1 = "nil", jTagIsNP = False}
getTagJP :: SExp JTag String -> JTag
getTagJP Nil = nilJTag
getTagJP (Atom tag _) = tag
getTagJP (Opr tag _) = tag

forgetNPSubs :: SExp JTag String -> SExp JTag String
forgetNPSubs Nil = Nil
forgetNPSubs (Atom a b) = (Atom a b)
forgetNPSubs (Opr tg sexps)
  | tg == rootJTag = (Opr tg $ map forgetNPSubs sexps)
  | jTagIsNP tg == True = (Atom tg $ concatMap getSubTokens sexps)
  | otherwise = (Opr tg $ map forgetNPSubs sexps)

forgetSubs :: SExp JTag String -> SExp JTag String
forgetSubs Nil = Nil
forgetSubs (Atom a b) = (Atom a b)
forgetSubs (Opr tg sexps)
  | tg == rootJTag = (Opr tg $ map forgetSubs sexps)
  | otherwise = (Atom tg $ concatMap getSubTokens sexps)

getSubTokens :: SExp JTag String -> String
getSubTokens Nil = ""
getSubTokens (Atom a b) = b
getSubTokens (Opr tg sexps) = concat (map getSubTokens sexps)



-- PDFで結果を確認する際は、FromPDF.hsの中にある、foldNPsJPRecursiveをGisho.foldNPsTest sexp0と置き換えてください

-- *************************************************************************
-- 「Haskellでつくる不思議な読書体験用」はここまで

-- foldNoの完成形。参照用
foldNo1 :: SExp JTag String -> SExp JTag String
foldNo1 Nil = Nil
foldNo1 (Atom a b) = Atom a b
foldNo1 (Opr a ss)
 | lenOfSS < 3 = (Opr a ss)
 | otherwise = Opr a $ pl ++  st
  where
    lenOfSS = length ss
    res@(st, pl) = foldl f (take 3 ss, []) $ drop 3 ss
      where
        f y@(stacked, pooled) x
         | isNotFullStacked = (snocL stacked x, pooled) -- <- not full Stacked
         | isNP = ([x], snocL pooled foldedOpr) -- <- stack terminated
         | otherwise = (snocL poppedResidue x, snocL pooled popped) -- popped
          where
            newJTag = CJTag {jTag = "名詞", jTag1 = "名詞名詞", jTagIsNP = True}
            popped = head stacked
            poppedResidue = tail stacked
            foldedOpr = Opr newJTag stacked
            isNotFullStacked = length stacked < 3
            isNP = isHeadNoun && isMidJoshiAndNo && isLastNoun
              where
                isHeadNoun = (jTag $ getTagJP $ head stacked) == "名詞"
                isMidJoshiAndNo = (Atom (CJTag "助詞" "連体化" False) "の") == (forgetSubs $ stacked !! 1)
                isLastNoun = (jTag $ getTagJP $ last stacked) == "名詞"

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

rootJTag = CJTag {jTag = "root", jTag1 = "root", jTagIsNP = False}

get2 :: (Turtle.MonadIO m) => Text.Text -> m [Turtle.Line]
get2 cmd = do
  Turtle.fold (Turtle.inshell cmd Turtle.empty) Fold.list

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
