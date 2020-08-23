{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module ForGisho where
import FromPDF
import SExp
import PopSExp
import Text.Show.Unicode


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


 -- JTag'は説明用で実際は使いません
data JTag' = CJTag' {
    jTag' :: String
  , jTag1' :: String
  , jTagIsNP' :: Bool
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

-- foldPrefix, foldSameTags0は、fromPDF.hsに定義されている。
foldNPsTest :: SExp JTag String -> SExp JTag String
-- foldNPsTest = foldPrefix . foldSameTags0
foldNPsTest = foldNo . foldPrefix . foldSameTags0
  where

-- 書籍を読み進める際は、foldNo1 sexpをundefinedに書き換えてからスタートします。
foldNo :: SExp JTag String -> SExp JTag String
foldNo sexp = foldNo1 sexp



-- snocL'は説明用で実際には使いません。
snocL' xs x = xs ++ [x]

-- nilJTag', getTagJP'は説明用で実際には使いません。
nilJTag'  = CJTag {jTag = "nil", jTag1 = "nil", jTagIsNP = False}
getTagJP' :: SExp JTag String -> JTag
getTagJP' Nil = nilJTag
getTagJP' (Atom tag _) = tag
getTagJP' (Opr tag _) = tag

-- forgetNPSubs'は説明用で実際には使いません。
forgetNPSubs' :: SExp JTag String -> SExp JTag String
forgetNPSubs' Nil = Nil
forgetNPSubs' (Atom a b) = (Atom a b)
forgetNPSubs' (Opr tg sexps)
  | tg == rootJTag = (Opr tg $ map forgetNPSubs' sexps)
  | jTagIsNP tg == True = (Atom tg $ concatMap getSubTokens' sexps)
  | otherwise = (Opr tg $ map forgetNPSubs' sexps)


-- getSubTokens'は説明用で実際には使いません。
getSubTokens' :: SExp JTag String -> String
getSubTokens' Nil = ""
getSubTokens' (Atom a b) = b
getSubTokens' (Opr tg sexps) = concat (map getSubTokens' sexps)



-- PDFで結果を確認する際は、FromPDF.hsの中にある、foldNPsJPRecursiveをfoldNPsTest sexp0と置き換えてください

-- *************************************************************************

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
