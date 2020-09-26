{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module AutoNP where
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
import ForGisho (JTag (CJTag), jTag, jTag1, jTagIsNP, foldNPsTest)
import qualified Data.Algorithm.Diff as Diff

testSentense = "本書はPDFリーダーを自身の手で作ることを通してプログラミング言語Haskellを学ぶ本の第一弾です。"

readSample :: IO (SExp JTag String)
readSample = toSExpsJP <$> getMecabed testSentense

testes = do
  sexp <- readSample
  let
    proNPTag = CJTag "名詞" "代名詞" True
    npSexp = Atom proNPTag "それ"
  res1 <- foldNPFromMecab npSexp sexp
  return ""

foldNPFromMecab npSexp sexp = do
  let
    replaceds2 = contract2SExpG npSexp sexp
    as = concat $ filterJust $ takeSndL $ forgetSExp sexp
    -- hoge = replaceds2 !! n
    retrReplSucceed hoge = do
      let
        bs = concat $ filterJust $ takeSndL $ forgetSExp $ snd hoge
      mecbAs <- (V.toList) <$> getMecabed as
      mecbBs <- (V.toList) <$> getMecabed bs
      let
        res = Diff.getDiffBy (==) mecbAs mecbBs
        func x = case x of
          Diff.First y ->  ("First", (mToken y, mTag y, mTag1 y, mAttr1 y, mAttr2 y, mGr1 y, mGr2 y, mVoc1 y, mVoc2 y, mVoc3 y))
          Diff.Second y -> ("Second", (mToken y, mTag y, mTag1 y, mAttr1 y, mAttr2 y, mGr1 y, mGr2 y, mVoc1 y, mVoc2 y, mVoc3 y))
          Diff.Both y1 y2 -> ("Both", (mToken y1, mTag y1, mTag1 y1, mAttr1 y1, mAttr2 y1, mGr1 y1, mGr2 y1, mVoc1 y1, mVoc2 y1, mVoc3 y1))
        resDiff = map func res
        isSucceedDiff
          | not isLeng3 = False
          | otherwise = isSucc && isReplacable
          where
            nums = takeFstL filtered
            isSucc = and $ map (\x@(n1, n2) -> n2 == n1 + 1) $ zip (init nums) (tail nums)
            judged = takeFstL $ takeSndL filtered
            isReplacable = judged == ["First", "First", "Second"]
            isLeng3 = length filtered == 3
            filtered = filter (\x@(_, x2) -> "First" == (fst x2) || "Second" == (fst x2)) $ indexingL resDiff
      -- return resDiff
      return isSucceedDiff
  ress <- mapM (\n -> retrReplSucceed $ replaceds2 !! n) [0 .. (length replaceds2) - 1]
  let
    res = zip ress replaceds2
    filtered = takeSndL $ filter (\x -> fst x == True) res
  return $ takeFstL filtered

contract2SExpG :: SExp a b -> SExp a b -> [([Int], SExp a b)]
contract2SExpG sexp0 sexp = replaceds
  where
    poss = tail $ posi $ deleteLastSexps sexp
    replaceds = map (\pos -> (pos, contract2SExp pos sexp0 sexp)) poss
    deleteLastSexps Nil = Nil
    deleteLastSexps (Atom a b) = (Atom a b)
    deleteLastSexps (Opr a bs) = (Opr a $ map deleteLastSexps $ init bs)

-- S式の縮約、つまり[(置き換える位置, 置き換えるS式), (置き換える位置, 置き換えるS式), ..]に従ってfoldで逐次縮約
contract2SExpAll sexp nssSexp0 = foldl (\y -> \nsSexp0s@(ns, sexp0) -> contract2SExp ns sexp0 y) sexp nssSexp0

-- 一つだけを狙い撃ちして縮約
contract2SExp :: [Int] -> SExp a b -> SExp a b -> SExp a b
contract2SExp ns sexp0 Nil = Nil
contract2SExp ns sexp0 (Atom a b) = Atom a b
contract2SExp [] sexp0 (Opr a bs) = sexp0
contract2SExp (n : ns) sexp0 (Opr a bs)
  | isLastIndex = (Opr a (take n bs ++ [sexp0] ++ drop (n + 2) bs))
  | otherwise = (Opr a (take n bs ++ [contract2SExp ns sexp0 $ bs !! n] ++ drop (n + 1) bs))
  where
    isLastIndex = ns == []

snocL xs x = xs ++ [x]

getNotEqIndex :: Eq a => [a] -> [a] -> Int
getNotEqIndex  as bs = undefined
  where
    res = Diff.getDiffBy (==) as bs




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

get2 :: (Turtle.MonadIO m) => Text.Text -> m [Turtle.Line]
get2 cmd = do
  Turtle.fold (Turtle.inshell cmd Turtle.empty) Fold.list

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

rootJTag = CJTag {jTag = "root", jTag1 = "root", jTagIsNP = False}

cshowIL :: Show b => [b] -> IO ()
cshowIL lis = mapM_ uprint $ zip [0 .. (length lis - 1)] lis

cshowL :: (Foldable t, Show a) => t a -> IO ()
cshowL lis = mapM_ uprint  lis

cshowI :: Show b => V.Vector b -> IO ()
cshowI lis = cshowIL $ V.toList lis

cshow :: Show a => V.Vector a -> IO ()
cshow lis = cshowL $ V.toList lis

takeFstL :: [(a, b)] -> [a]
takeFstL = map fst

takeSndL :: [(a, b)] -> [b]
takeSndL = map snd

filterJust :: [Maybe a] -> [a]
filterJust lis = concatMap fmb lis
  where
    fmb ll = case ll of
      Nothing -> []
      Just a -> [a]
