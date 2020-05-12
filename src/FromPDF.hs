{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module FromPDF where

import Control.Monad (when)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Text as Text
import qualified Data.List as Lis
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Network.HTTP.Simple as HT
import Data.Aeson
import Network.HTTP.Client
import qualified Control.Lens as LENS
import qualified Data.Aeson.Lens as AL
import Data.Text.ICU.Convert as ICU
import qualified Data.ByteString.Lazy as BL
import Lib (iVecFromFile, iVecFromFileJP, oVecToFile, delimitAtWO2, delimitAtWO2By, vNub, takeFst, takeSnd, takeFstT, takeSndT, takeThdT, execShell, delimitAtW2, getHistogram, getDivideLine, vSortBy)
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
import Foreign.Ptr (castPtr)

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

main4 = do
  aa <- getArgs
  let
    minn = read $ aa !! 0 :: Int
    maxa = read $ aa !! 1 :: Int
    ff n = do
      a <- getNPs n
      putStrLn $ show n
      return a
  aa <- mapM ff [minn .. maxa]
  putStrLn ""

getNPs nPage = do
  aa <- getSExpsIO pdfPathLoc nPage
  let
    res = map f aa
      where
        f bb = ee
          where
            detached = mapNode snd id bb
            cc = takeSpecTags (\x -> x == NP) detached
            dd = filter (\y -> isBottomBy id y) cc
            ee = map (mapNode id (\x -> (fst x, synSqs $ snd x))) dd
  return res

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

checkPage nPage nSens = do
  aa <- getSExpsIO pdfPathLoc nPage
  let
    ret = map (mapNode id fst) aa
  putStrLn ""

{-
pdfPath = Text.pack "file:///home/polymony/poppyS/pdfs/3331554.3342603.pdf"
pdfPath = Text.pack "file:///home/polymony/poppyS/pdfs/Leinster.pdf"
pdfPath = Text.pack "file:///home/polymony/poppyS/pdfs/CTFP.pdf"
nPage = fromIntegral 1
layoutsPrim = [CSq{sqTop = 0.0, sqLeft = 0.0, sqBot = 1.0, sqRight = 1.0}]
layoutMode = TwoCols
docPathSuffix = "./pdfs/CTFP.pdf"
tokSqTrues <- getTokenPositions docPathSuffix -- tokSqTrues,, left top right bot
nPage = 106
tokSqTruePrim = tokSqTrues V.! nPage
doc <- GPop.documentNewFromFile pdfPath Nothing
page <- GPop.documentGetPage doc nPage
-}

getSExpsIOPoppy1 tokSqTruePrim page = do
  presNubPrim <- iVecFromFile "tessPresDiv.txt"
  postNubPrim <- iVecFromFile "tessPostDiv.txt"
  tessRess <- tesseractPartlyIO page tokSqTruePrim
  tessRessChar@(stackedSens, charSqV) <- tesseractPartlyIOChar page tokSqTruePrim
  charSq <- pageGetTokSqLibs2 page
  let
    retrSexpS (sens, charSqs) = do
      let
        presNub = V.map (\x -> read x) presNubPrim :: V.Vector (String, String)
        postNub = V.map (\x -> read x) postNubPrim :: V.Vector (String, String)
      stanRess <- stanIOPoppy1 sens
      let
        sexps = stanAssign2Poppy1 charSqs stanRess
      return sexps
  bb <- V.mapM retrSexpS $ V.zip stackedSens charSqV
  return $ concatMap id bb

stanAssign2Poppy1 charSqs stanRess = map reconsSExp resSExpForg
  where
    indexed = map forgetIndexed stanRess
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

{-
tessStanIOTess page tokSqTruePrim = do
  presNubPrim <- iVecFromFile "tessPresDiv.txt"
  postNubPrim <- iVecFromFile "tessPostDiv.txt"
  tessRess <- tesseractPartlyIO page tokSqTruePrim
  let
   f tessRes = do
     let
       presNub = V.map (\x -> read x) presNubPrim :: V.Vector (String, String)
       postNub = V.map (\x -> read x) postNubPrim :: V.Vector (String, String)
       sens = fromTokensToSens $ takeFst tessRes :: String
     res <- retrStanfordRes sens isLinux
     let
       parsed = parseNPs2 res
       stParsed = V.concatMap (\x -> takeSnd x) parsed
       stParsedFull = V.concatMap id parsed
       tessResMutated = replaceEtDivideHead presNub $ replaceEtDivideLast postNub tessRes
       stData = assSqsToPhraseNeo stParsed tessResMutated
       ooo = V.zip stData stParsedFull
     return (res, stParsed, takeFst ooo, takeSnd ooo)
  resPartlySyned <- V.mapM f tessRess
  let
    res@(a, b, c, d) = V.foldl' f (V.empty, V.empty, V.empty, V.empty) resPartlySyned
      where
        f y@(st1, st2, st3, st4) x@(x1, x2, x3, x4) = (st1 V.++ x1, st2 V.++ x2, st3 V.++ x3, st4 V.++ x4)
  return res
-}

tesseractPartlyIOChar page tokSqTruePrim = do
  (colPrim, rowPrim) <- GPop.pageGetSize page
  charsPrim <- getCharPositionFromPopplerPrim page tokSqTruePrim
  let
    tokSqsRatioPartlyChars = popplerTokenizePartlyS charsPrim
    stackedSens = takeFst tokSqsRatioPartlyChars
    stackedChars = takeSnd tokSqsRatioPartlyChars
    resS = V.map tokSqs stackedChars
     where
      tokSqs tokSqsRatio = V.map (\x@(c, sqs) -> (c, V.concatMap (V.map f) sqs)) tokSqsRatio
       where
         row = rowPrim
         col = colPrim
         f ((r1, c1), (r2, c2)) = ((g1 r1 row, g1 c1 col), (g2 r2 row, g2 c2 col))
              where
                g1 :: Double -> Double -> Double
                g1 ratio space
                 | floored - 1 < 0 = floored
                 --  | otherwise = floored - 1
                 | otherwise = floored
                    where
                      floored = ratio * space
                g2 :: Double -> Double -> Double
                g2 ratio space
                 | floored + 1 > space - 1 = floored
                 --  | otherwise = floored + 1
                 | otherwise = floored
                    where
                      floored = ratio * space
    res2S = V.map (\xs -> V.map (\x@(tok, sqs) -> (tok, V.map g sqs)) xs) resS
       where
         g ((row1, col1), (row2, col2)) = CSq {sqTop = rowPrim - row2, sqLeft = col1, sqBot = rowPrim - row1, sqRight = col2}
  return (stackedSens, res2S)

-- rksprpr
tesseractPartlyIO page tokSqTruePrim = do
  (colPrim, rowPrim) <- GPop.pageGetSize page
  charsPrim <- getCharPositionFromPopplerPrim page tokSqTruePrim
  tokSqsRatioPartly <- popplerTokenizePartlyIO charsPrim
  let
    res
     | tokSqsRatioPartly == (V.fromList [V.fromList [("",V.empty)]]) = V.empty
     | otherwise = V.map (concatLastBar3 . tokSqs) tokSqsRatioPartly
     where
      tokSqs tokSqsRatio = V.map (\x@(str, sqs) -> (str, V.map f sqs)) tokSqsRatio
       where
         row = rowPrim
         col = colPrim
         f ((r1, c1), (r2, c2)) = ((g1 r1 row, g1 c1 col), (g2 r2 row, g2 c2 col))
              where
                g1 :: Double -> Double -> Double
                g1 ratio space
                 | floored - 1 < 0 = floored
                 | otherwise = floored - 1
                    where
                      floored = ratio * space
                g2 :: Double -> Double -> Double
                g2 ratio space
                 | floored + 1 > space - 1 = floored
                 | otherwise = floored + 1
                    where
                      floored = ratio * space
    res2 = V.map (\xs -> V.map (\x@(tok, sqs) -> (tok, V.map g sqs)) xs) res
       where
         g ((row1, col1), (row2, col2)) = CSq {sqTop = rowPrim - row2, sqLeft = col1, sqBot = rowPrim - row1, sqRight = col2}
  return res2

concatLastBar3 :: V.Vector (String, V.Vector SquareD) -> V.Vector (String, V.Vector SquareD)
concatLastBar3 arrPrim
  | arrPrim == V.empty = V.empty
  | otherwise = pl V.++ (V.singleton st)
     where
       arr = V.filter (\x@(str,sqs) -> (not $ str == "") && (not $ sqs == V.empty)) arrPrim
       headArr@(headTok, headSqs) = V.head arr
       tailArr = V.tail arr
       folded@(st, pl) = V.foldl' f ((headTok, headSqs), V.empty) tailArr
       f y@(stacked@(cTok, cSqs), pooled) x@(tok, sqs)
         | cSqs == V.empty = resConcated
         | newCol < fronteerCol && isNewTokLastBar = resConcated
         | otherwise = resNormal
            where
              fronteerCol = snd $ snd $ V.last cSqs :: Double
              newCol = snd $ snd $ V.last sqs :: Double
              isNewTokLastBar
                | length tok < 2 = False
                | otherwise = last cTok == '-'
              resConcated = (((init cTok) ++ tok, cSqs V.++ sqs), pooled)
              resNormal = ((tok, sqs), pooled V.++ (V.singleton stacked))

type Square = ((Int, Int), (Int, Int))
type SquareD = ((Double, Double), (Double, Double))
-- docName = "dijkstra"
-- n = 2
-- rksprpr
popplerTokenizePartlyS charsPrim = folded
  where
    marks = V.fromList [',', '.', ';', head ":", '!', '?']
    folded
      | charsPrim == V.empty = V.empty
      | hChar == '\n' = V.empty
      | hChar == ' ' = V.empty
      | otherwise = V.snoc pooledBlocksF (stackedStrF, pooledLineF V.++ stackedCharF)
      where
        folddd@(stackedStrF, stackedCharF, prevColF, pooledLineF, pooledBlocksF) = V.foldl' g ([hChar], V.singleton $ V.head charsPrim, 0.0, V.empty, V.empty) $ V.tail charsPrim
        headhead@(hChar, hSqss) = V.head charsPrim

        g y@(stackedStr, stackedChar, prevLineCol, pooledLine, pooledBlocks) x@(c, sqss)
          | isBlockTerminate = ("", V.empty, currCol, V.empty, V.snoc pooledBlocks (stackedStr, pooledLine V.++ (V.snoc stackedChar x)))
          | isBlockTerminateMarked = ("", V.empty, currCol, V.empty, V.snoc pooledBlocks (stackedStr, pooledLine V.++ (V.snoc stackedChar x)))
          | isContinueLine = (stackedStr ++ [c], V.snoc stackedChar x, prevLineCol, pooledLine, pooledBlocks)
          | isPrevBarContinueNextLine = (init stackedStr     , V.empty, currCol, pooledLine V.++ (V.snoc stackedChar x), pooledBlocks)
          | isContinueNextLine        = (snocL stackedStr ' ', V.empty, currCol, pooledLine V.++ (V.snoc stackedChar x), pooledBlocks)
          | otherwise = (snocL stackedStr ' ', V.empty, currCol, pooledLine V.++ (V.snoc stackedChar x), pooledBlocks)
          where
            currCol = snd $ snd $ V.head $ V.head sqss
            -- currCol = snd $ snd $ V.last $ V.last sqss -- <- correct?
            isPrevSpace = 0 < length stackedStr && last stackedStr == ' '
            isPrevBar = 0 < length stackedStr && last stackedStr == '-'
            isPrevMark = 0 < length stackedStr && (V.elem (last stackedStr) marks)
            isIso = 1 < length stackedStr && (last $ tail stackedStr) == ' '
            isReturned = c == '\n'
            isNotPrevBlocked = not $ pooledLine == V.empty

            isBlockTerminate = isReturned && (0.001 < abs (prevLineCol - currCol)) && isNotPrevBlocked
            isBlockTerminateMarked = isReturned && isPrevMark && (not isIso) && isNotPrevBlocked
            isContinueLine = not isReturned
            isPrevBarContinueNextLine = isPrevBar && (not isIso) && isReturned && (not isBlockTerminate) && (not isBlockTerminateMarked)
            isContinueNextLine        =                             isReturned && (not isBlockTerminate) && (not isBlockTerminateMarked)

popplerTokenizePartlyIO chars = do
      let
        marks = V.fromList [',', '.', ';', head ":", '!', '?']
        regularReturnCols
          | returns == V.empty = V.empty
          | otherwise = continued
          where
            returns = V.map (\x@(c,sq) -> snd $ snd $ V.head $ V.concatMap id sq) $ V.filter (\x@(c,_) -> c == '\n') chars
            g x@(_,scoreX) y@(_,scoreY) = compare scoreX scoreY
            -- continued = vNub $ takeFst $ V.filter (\x@(x1, x2) -> x1 - x2 == 0.0) $ V.zip (V.init returns) (V.tail returns)
            continued = vNub $ takeFst $ V.filter (\x@(x1, x2) -> abs (x1 - x2) < 0.001) $ V.zip (V.init returns) (V.tail returns)
        charsDivided = V.snoc pol st
            where
              res@(st,pol) = V.foldl' f3 (V.empty, V.empty) chars
              f3 y@(stacked, pooled) x@(c,sqss)
                | c == '\n' && (not $ V.elem col2 regularReturnCols) = (V.empty, V.snoc pooled (V.snoc stacked x))
                | otherwise = (V.snoc stacked x, pooled)
                 where
                   col2 = snd $ snd $ V.head $ V.concatMap id sqss
        getSens chars = V.snoc resPooled $ (residueStr, residueStacked)
          where
            res@(residueStr, residueStacked, resPooled) = V.foldl' f ("", V.empty, V.empty) chars
            f y@(stStr, stackedSqs, pooled) x@(c, vsqs)
              | isFinalized && stStr == "" = y
              | isFinalized && isMarksNotInsulated && isLastMarks = ("", V.empty, pooled V.++ (V.fromList [(init stStr, V.init stackedSqs), ([last stStr], V.singleton $ V.last stackedSqs)]))
              | isFinalized = ("", V.empty, V.snoc pooled (stStr, stackedSqs))
              | otherwise = (stStr ++ [c], V.snoc stackedSqs vsqs, pooled)
                where
                  isFinalized = (c == ' ' || c == '\n')
                  isMarksNotInsulated = (not $ V.length stackedSqs < 2) && (not $ length stStr < 2)
                  isLastMarks = V.elem (last stStr) marks
        resSens = V.map (\x -> (f. getSens) x) charsDivided
           where
             f vs = V.map (\y@(str, sqss) -> (str, V.concatMap id $ V.concatMap id sqss)) vs
      return resSens

getCharPositionFromPopplerPrim page tokSqTruePrim = do
  pageStr <- GPop.pageGetText page
  let
    tokens = V.map (Text.pack . V.toList) $ V.concatMap (\x -> delimitAtV '\n' x) $ V.fromList $ map V.fromList $ delimitAtWO2 ' ' id  $ Text.unpack pageStr
  posi <- V.mapM (popPPageFindText page) tokens
  let
    zippedToken = V.zip tokens posi
    flattened = vNub $ V.concatMap (\x@(tok, sqs) -> V.fromList $ map (\y -> (tok, y)) sqs) zippedToken
  pSize@(totWidth, totHeight) <- GPop.pageGetSize page
  let
    tokSqTrue = res
      where
        res
          | tokSqTruePrim == V.empty = V.empty
          | otherwise = pl V.++ st
              where
                resPrim@(st,pl,_,_,_) = V.foldl' h3 (V.singleton headVal, V.empty, (c1, r1, c2, r2), c2,r2) (V.tail tokSqTruePrim)
                headVal@(tk, (PopPRectangle c1 r1 c2 r2)) = V.head tokSqTruePrim

                h3 y@(stacked, pooled, (minC, minR, maxC, maxR), frons,fronsRow) x@(tktk, (PopPRectangle cc1 cr1 cc2 cr2))
                  | isEntered = (V.singleton x, pooled V.++ revisedStacked, (neoMinC, neoMinR, neoMaxC, neoMaxR), cc2,cr2)
                  | isReturned = (V.singleton x, pooled V.++ revisedStacked, (neoMinC, neoMinR, neoMaxC, neoMaxR), cc2,cr2)
                  | otherwise = (V.snoc stacked x, pooled, (neoMinC, neoMinR, neoMaxC, neoMaxR), cc2,cr2)
                  where
                    isEntered = cc1 < frons
                    isReturned = fronsRow < cr2
                    neoMinC
                     | isEntered || isReturned = cc1
                     | cc1 < minC = cc1
                     | otherwise = minC
                    neoMinR
                     | isEntered || isReturned = cr1
                     | cr1 < minR = cr1
                     | otherwise = minR
                    neoMaxC
                     | isEntered || isReturned = cc2
                     | maxC < cc2 = cc2
                     | otherwise = maxC
                    neoMaxR
                     | isEntered || isReturned = cr2
                     | maxR < cr2 = cr2
                     | otherwise = maxR
                    revisedStacked = V.map h4 stacked
                       where
                         h4 (xtk, (PopPRectangle xc1 xr1 xc2 xr2)) = (xtk, (PopPRectangle xc1 minR xc2 maxR))
    tokSqTrueWided = V.map (\x@(x1,x2) -> (x1, f x2)) tokSqTrue
       where
         f (PopPRectangle xc1 xr1 xc2 xr2) = (PopPRectangle (ashort xc1) (ashort xr1) (along xc2) (along xr2))
            where
              along r = fromIntegral (1 + floor r)
              ashort r = fromIntegral ((floor r) - 1)
    chars = extractChars $ Text.unpack pageStr :: [Char]
  charPossRect <- getCharPossSqRect chars page V.empty
  let
    charPossRectFlattened :: V.Vector (Char, PopPRectangle)
    charPossRectFlattened = V.concatMap (\x@(c, sqs) -> V.map (\sq -> (c, sq)) sqs) charPossRect
    charPossRectFlattenedSorted = vSortBy h2 $ vSortBy h1 charPossRectFlattened
      where
        h1 (_,ns@(PopPRectangle a b c d)) (_,nd@(PopPRectangle e f g h)) = compare a e
        h2 (_,ns@(PopPRectangle a b c d)) (_,nd@(PopPRectangle e f g h)) = compare h d
    charPossRectFlattenedShrinked = V.map (\x@(x1,x2) -> (x1, f x2)) charPossRectFlattened
       where
         f (PopPRectangle xc1 xr1 xc2 xr2) = (PopPRectangle (along xc1) (along xr1) (ashort xc2) (ashort xr2))
            where
              along r = fromIntegral (1 + floor r)
              ashort r = fromIntegral ((floor r) - 1)
    --tokSqTrueFiltered = V.filter (\x@(tok, _) -> not $ tok == "\n") tokSqTrue
    tokSqTrueFiltered = V.filter (\x@(tok, _) -> not $ tok == "\n") tokSqTrueWided
    salvaged2Prim = V.map f $ takeSnd tokSqTrueFiltered
      where
        f sq = includesSorted
          where
            --includes = V.filter (\x@(c, sqn) -> isIncludedPopplerRect sq sqn) charPossRectFlattened
            includes = V.filter (\x@(c, sqn) -> isIncludedPopplerRect sq sqn) charPossRectFlattenedShrinked
            includesSorted = vSortBy h2 includes
               where
                 h2 (_,ns@(PopPRectangle a b c d)) (_,nd@(PopPRectangle e f g h)) = compare c g
    zipped = V.zip (takeFst tokSqTrue) (V.map (V.toList . takeFst) salvaged2Prim)
    zippedAll = V.zip tokSqTrue (V.map takeSnd salvaged2Prim)
    salvaged2 :: V.Vector (Char, PopPRectangle)
    salvaged2 = V.concatMap id salvaged2Prim
    salvagedRecovered = V.map (\x -> f x) salvaged2
       where
         f (ch, pRect) = including
           where
             including = V.head $ V.filter (\x@(c,sq) -> ch == c && isIncludedPopplerRect sq pRect) charPossRectFlattened

    cSqsDivid :: V.Vector (Char, Sq Double)
    cSqsDivid = V.map (\x@(c, sqRect) -> (c, pRectToSq totHeight sqRect)) $ salvagedRecovered
    pageStrVec = V.fromList $ Text.unpack pageStr :: V.Vector Char

    diffPrim = vGetDiffBy (==) pageStrVec $ takeFst cSqsDivid
    diff2 = assignByDiff pageStrVec $ V.map (\x@(c, sq) -> (c, V.singleton sq)) cSqsDivid :: V.Vector (Char, V.Vector (V.Vector (Sq Double)))

    diffDoubled = V.map (\x@(c, sqss) -> (c, V.map (\sqs -> V.map g sqs) sqss)) diff2
       where
         -- g sq = ((1.0 - neoRLow, neoCLeft), (1.0 - neoRHigh, neoCRight))
         g sq = ((1.0 - neoRLow, neoCLeft), (1.0 - neoRHigh, neoCRight))
            where
              cLeft = sqLeft sq
              rHigh = sqTop sq
              cRight = sqRight sq
              rLow = sqBot sq
              neoCLeft = cLeft / totWidth
              neoCRight = cRight / totWidth
              neoRHigh = rHigh / totHeight
              neoRLow = rLow / totHeight
    diffDoubledDeEmpty = V.filter (\x@(tok, sqs) -> not $ g sqs) diffDoubled
       where
         g sqs = sqs == V.empty || sqs == V.singleton V.empty
  -- return diffDoubledDeEmpty
  return diffDoubled

isIncludedPopplerRect ns@(PopPRectangle a b c d) nd@(PopPRectangle e f g h)
  = (a <= e) && (b <= f) && (g <= c) && (h <= d)

popPPageFindText page token = do
  rects <- GPop.pageFindText page token
  rectsSwapped <- mapM (gpopRectToPopPRect page) rects
  return rectsSwapped

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


getTokenPositions docPathSuffix = do
  let
    -- docPathSuffix = "./pdfs/Leinster.pdf"
  execShell $ "pdftotext -bbox " ++ "\"" ++ docPathSuffix ++ "\""
  let
    inPath = (reverse $ dropWhile (\c -> not $ c == '.') $ reverse docPathSuffix) ++ "html"
  res <- parsePDFHtml <$> iVecFromFileJP inPath
  removeFile inPath
  return res

replaceStr nd ns str = Text.unpack $ Text.replace nd ns $ Text.pack str

parsePDFHtml :: V.Vector String -> V.Vector (V.Vector (String, PopPRectangle))
parsePDFHtml xs = V.map (\x -> h x) paged
  where
    handi = 0.000001
    paged = V.snoc pl st
       where
         res@(st, pl) = V.foldl' f (V.empty, V.empty) xs
         f y@(stacked, pooled) x
           | isTriggerred = (V.singleton x, pooled)
           | isTermed = (V.empty, V.snoc pooled stacked)
           | otherwise = (V.snoc stacked x, pooled)
          where
            deSpaced = replaceStr " " "" x
            isTriggerred = Lis.isPrefixOf "<page" deSpaced
            isTermed = Lis.isPrefixOf "</page" deSpaced
    h pgsPrim = V.map (parseWord heit) wds
      where
        (heit, wds, widt) = g pgsPrim
    g pgs = (heit, wds, widt)
       where
         (widt, heit) = (read wid :: Double, read hei :: Double)
            where
              sizeStrPrim = V.head pgs
              delimited = delimitAtWO2 ' ' id sizeStrPrim
              hei = takeWhile (\c -> not $ c == '\"') $ tail $ dropWhile (\c -> not $ c == '\"') $ delimited !! 4
              wid = takeWhile (\c -> not $ c == '\"') $ tail $ dropWhile (\c -> not $ c == '\"') $ delimited !! 3
         wds = V.filter (\x -> Lis.isPrefixOf "<word" (replaceStr " " "" x)) $ V.tail pgs
    parseWord hei cs = (token, PopPRectangle (xMin -handi) (hei - yMax - handi) (xMax + handi) (hei - yMin + handi))
    -- parseWord hei cs = (token, CSq { sqLeft = (xMin - handi), sqTop =  (hei - yMax - handi), sqRight = (xMax + handi), sqBot = (hei - yMin + handi)})
       where
         delimited = delimitAtWO2 ' ' id cs
         toDouble str = read str :: Double
         xMin = toDouble $ init $ tail $ dropWhile (\c -> not $ c == '\"') $ delimited !! 5
         yMin = toDouble $ init $ tail $ dropWhile (\c -> not $ c == '\"') $ delimited !! 6
         xMax = toDouble $ init $ tail $ dropWhile (\c -> not $ c == '\"') $ delimited !! 7
         yMaxPrim = init $ tail $ dropWhile (\c -> not $ c == '\"') $ delimited !! 8
         yMax = toDouble $ takeWhile (\c -> not $ c == '\"') yMaxPrim
         token = tail $ dropWhile (\c -> not $ c == '>') $ takeWhile (\c -> not $ c == '<') yMaxPrim

getSExpsIOOldNew pdfPath nPage = do
  doc <- GPop.documentNewFromFile pdfPath Nothing
  nOfPage <- GPop.documentGetNPages doc
  pages <- mapM (\n -> GPop.documentGetPage doc n) [0 .. nOfPage - 1]
  let
    page = pages !! nPage
  (linLibs, tokLibs, chLibs) <- pageGetTokSqLibs page
  (wid, hei) <- GPop.pageGetSize page
  let
    (linResult, tokAssigneds, aaaa) = fillLibs linLibs tokLibs chLibs hei
    charSq = filter (\x -> not $ x == []) $ map (rebuildCharSq2 tokAssigneds chLibs) linResult
    groupedLines = groupLineWLastX $ map (map (\x -> (head $ Text.unpack $ fst x, snd x))) charSq
    retrSexp chSqs = do
      stanRess <- stanIO chSqs
      let
        sexps = stanAssign2 chSqs stanRess
      return sexps
  aa <- mapM retrSexp groupedLines
  return $ concat aa

getSExpsIONew pdfPath nPage layoutsPrim layoutMode = do
  doc <- GPop.documentNewFromFile pdfPath Nothing
  nOfPage <- GPop.documentGetNPages doc
  pages <- mapM (\n -> GPop.documentGetPage doc n) [0 .. nOfPage - 1]
  let
    page = pages !! nPage
  (wid, hei) <- GPop.pageGetSize page
  let
    layouts = map f layoutsPrim
       where
         f sq = CSq {sqTop = tp * hei, sqLeft = lef * wid, sqBot = bot * hei, sqRight = righ * wid}
           where
             tp = sqTop sq
             bot = sqBot sq
             lef = sqLeft sq
             righ = sqRight sq
  charSqs <- pageGetTokSqLibs3 page layouts layoutMode
  let
    getResEachLayout charSq = do
      let
        groupedLines = groupLineWLastX $ map (map (\x -> (head $ Text.unpack $ fst x, snd x))) charSq
        retrSexp chSqs = do
          stanRess <- stanIO chSqs
          let
            sexps = stanAssign2 chSqs stanRess
          return sexps
      aa <- mapM retrSexp groupedLines
      return $ concat aa
  resres <- mapM getResEachLayout charSqs
  return $ concat resres

getSExpsIO pdfPath nPage = do
  doc <- GPop.documentNewFromFile pdfPath Nothing
  nOfPage <- GPop.documentGetNPages doc
  pages <- mapM (\n -> GPop.documentGetPage doc n) [0 .. nOfPage - 1]
  let
    page = pages !! nPage
  charSq <- pageGetTokSqLibs2 page
  let
    groupedLines = groupLineWLastX $ map (map (\x -> (head $ Text.unpack $ fst x, snd x))) charSq
    retrSexp chSqs = do
      stanRess <- stanIO chSqs
      let
        sexps = stanAssign2 chSqs stanRess
      return sexps
  aa <- mapM retrSexp groupedLines
  return $ concat aa

getSExpsIOOld pdfPath nPage = do
  doc <- GPop.documentNewFromFile pdfPath Nothing
  nOfPage <- GPop.documentGetNPages doc
  pages <- mapM (\n -> GPop.documentGetPage doc n) [0 .. nOfPage - 1]
  let
    page = pages !! nPage
  (linLibs, tokLibs, chLibs) <- pageGetTokSqLibs page
  (wid, hei) <- GPop.pageGetSize page
  let
    (linResult, tokAssigneds, aaaa) = fillLibs linLibs tokLibs chLibs hei
    charSq = filter (\x -> not $ x == []) $ map (rebuildCharSq2 tokAssigneds chLibs) linResult
    groupedLines = groupLineWLastX $ map (map (\x -> (head $ Text.unpack $ fst x, snd x))) charSq
    retrSexp chSqs = do
      stanRess <- stanIO chSqs
      let
        sexps = stanAssign2 chSqs stanRess
      return sexps
  aa <- mapM retrSexp groupedLines
  return $ concat aa

stanAssign2 chSqs stanRess = map reconsSExp resSExpForg
  where
    indexed = map forgetIndexed stanRess
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
    chSqsMute = V.fromList $ map (\x@(c, sq) -> (c, V.singleton sq)) $ concat chSqs
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

stanIO chSqs = do
  let
    str = toSentences chSqs
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


-- 値が全て埋まっているということ
isTSFinalized :: Eq a => TokSq a b -> Bool
isTSFinalized ts = isAllAssigned
  where
    isAllAssigned = filtered == []
    filtered = (filter (\x -> (snd x) == []) $ tsValTable ts)

isUnique :: Eq a => TokSq a b -> Bool
isUnique ts = ([] == tsIncOrphs ts) || (not isSelfOrphs)
  where
    orphs = takeFstL $ tsIncOrphs ts
    tok = tsRawVal ts
    isSelfOrphs = and $ map (\orp -> orp == tok) orphs

rebuildCharSq
  :: Eq b1 =>
     [TokSq Text.Text b2]
     -> [TokSq a1 b1] -> TokSq a2 b3 -> [(Char, Sq b1)]
rebuildCharSq tokAssigneds chLibs linRes = children
  where
    children = map (\x -> (fst x, head $ snd x)) $ filter (\x -> not $ snd x == []) $ map g $ concatMap tsValTable $ fetch tokAssigneds $ concat $ takeSndL $ tsValTable linRes
       where
         f = (\x@(c,ids) -> (head $ Text.unpack c, head $ concatMap tsRawSqs $ fetch chLibs ids))
         g = (\x@(c,ids) -> (head $ Text.unpack c, concatMap tsRawSqs $ fetch chLibs ids))

rebuildCharSq2
  :: Eq b1 =>
     [TokSq Text.Text b2]
     -> [TokSq a1 b1] -> TokSq a2 b3 -> [(a1, Sq b1)]
rebuildCharSq2 tokAssigneds chLibs linRes = chSqs
  where
    tokIds = concat $ takeSndL $ tsValTable linRes
    toks = map f tokIds
      where
        f i = head $ filter (\tlib -> i == (tsId tlib)) tokAssigneds
    charChars = map g toks
      where
        g tok = chrs
          where
            chIds = concat $ takeSndL $ tsValTable tok
            chrs = map f chIds
              where
                f i = head $ filter (\tlib -> i == (tsId tlib)) chLibs
    chSqs = concatMap (map g2) charChars
      where
        g2 chSq = (tsRawVal chSq, head $ tsRawSqs chSq)

rebuildSquare
  :: TokSq a1 b -> [TokSq a2 Double] -> (a1, [Sq Double])
rebuildSquare tokLib chLibs = (token, charSqs)
  where
    token = tsRawVal tokLib
    ids = concat $ takeSndL $ tsValTable tokLib
    charSqs = synSqs $ Lis.sortBy g $ concatMap tsRawSqs $ filter (\l -> elem (tsId l) ids) chLibs
      where
        g x y = compare (sqLeft x) (sqLeft y)

fillLibs linLibs tokLibs chLibs hei = (linResult, tokAssigneds, pureTokenSquares )
  where
    -- fAss tLibs1 lLib = res
    fAss tLibs1 lLib = lLib {tsValTable = res2, tsIncOrphs = orphs}
        where
          chldIds = map tsId $ filter g tLibs1
            where
              g tLib1 = or [isSqIncludeSq sq1 sq2 | sq1 <- (tsRawSqs lLib), sq2 <- (tsRawSqs tLib1)]
          fChld = fetch tLibs1 chldIds
          fChldSorted = map (\x -> (tsRawVal x, tsId x)) $ Lis.sortBy h fChld
            where
              h xTS yTS = compare (sqLeft $ head $ tsRawSqs xTS) (sqLeft $ head $ tsRawSqs yTS)
          valT = tsValTable lLib
          res = getDiffBy (\x -> \y -> (fst x) == (fst y)) valT $ map (\x -> (fst x, [snd x])) fChldSorted
          res2 = map f3 $ filter (\x -> not $ isSecond x) res
            where
              f3 z = case z of
                First x -> x
                Second x -> x
                Both x y -> y
          assigned = concat $ takeSndL res2
          orphs = filter (\x@(_,i) -> not $ elem i assigned) fChldSorted
    tokAssigneds = map (fAss chLibs) tokLibs
    linAssigneds = map (fAss tokLibs) linLibs
    outerOrphToks = filter g tokLibs
      where
        g ts = not $ elem (tsId ts) inners
        inners = concatMap f linAssigneds
          where
            f x = (concat $ takeSndL $ tsValTable x) ++ (takeSndL $ tsIncOrphs x)


    fAssOrphs tLibs1 lLib
      | anchors == [] = (0.0, [])
      | otherwise = (score, res)
        where
          valT = takeFstL $ tsValTable lLib
          assed = map f valT
            where
              f tok = filter (\x -> (tsRawVal x) == tok) tLibs1
          anchors = filter (\x -> length x == 1) assed
          centerY = average $ map (\sq -> 0.5 * (sqTop sq + sqBot sq)) $ concatMap (tsRawSqs . head) $ filter (\x -> length x == 1) assed
          assed2 = map (filter f) assed
            where
              isIncY y sq = (sqTop sq) <= y && y <= (sqBot sq)
              f sq = or $ map (isIncY centerY) $ tsRawSqs sq
          applicSorted = Lis.sortBy g2 $ Lis.nub $ concat assed2
            where
                g2 x y = compare (sqsTakeLeft $ tsRawSqs x) (sqsTakeLeft $ tsRawSqs y)
          forDiffAss = V.fromList $ map (\x -> (tsRawVal x, V.singleton x)) applicSorted
          score = (fromIntegral nBoth) / (fromIntegral (length rawScore))
            where
              rawScore = getDiff valT $ map tsRawVal applicSorted
              nBoth = length $ filter isBoth rawScore
          res = V.toList $ V.map (\x@(tk,sqs) -> (tk, V.map tsId $ V.concatMap id sqs)) $ assignByDiff (V.fromList valT) forDiffAss

    fAssOrphsEasy tLibs1 lLib = (score, res)
        where
          valT = takeFstL $ tsValTable lLib
          rawScore = getDiff valT $ map tsRawVal tLibs1
          forDiffAss = V.fromList $ map (\x -> (tsRawVal x, V.singleton x)) tLibs1
          score = (fromIntegral nBoth) / (fromIntegral (length rawScore))
            where
              rawScore = getDiff valT $ map tsRawVal tLibs1
              nBoth = length $ filter isBoth rawScore
          res = V.toList $ V.map (\x@(tk,sqs) -> (tk, V.map tsId $ V.concatMap id sqs)) $ assignByDiff (V.fromList valT) forDiffAss

    longLegs = map tsId $ filter (\tsq -> not $ (isTSFinalized tsq && isUnique tsq)) linAssigneds
    suffed = nonFetch linAssigneds longLegs
    llClusters = groupSucc longLegs
    isl :: [([Int], Double, Double)]
    isl = map h4 llClusters
       where
         h4 is = (is, topDec, botSucc)
           where
             decc = head is - 1
             succ = last is + 1
             isDeccLT0 = decc < 0
             isSuccGT = length linAssigneds - 1 < succ
             sqDecs = tsRawSqs $ linAssigneds !! decc
             sqSuccs = tsRawSqs $ linAssigneds !! succ
             topDec
               | isDeccLT0 = 0.0
               | otherwise = maximum $ map sqTop sqDecs
             botSucc
               | isSuccGT = hei
               | otherwise = minimum $ map sqBot sqSuccs
    islAssed = concatMap f isl
      where
        f x@(is, deccTop, succTop)
          | tried2 == [] = []
          | otherwise = map ff $ zip (map (\i -> linLibs !! i) is) tried2
           where
             ff (x1, x2) = x1 {tsValTable = x2}
             lenOfIs = length is
             applicants
               | deccTop <= succTop = filter g outerOrphToks
               | otherwise = outerOrphToks
                where
                  g y = deccTop <= sqTop1 && sqTop1 <= succTop
                     where
                       sqTop1 = sqsTakeTop $ tsRawSqs y
             applClustered
               | applicants == [] = []
               | otherwise = map (g5 . g4) $ clusterSqs (map (sqsTakeTop . tsRawSqs) applicants) (lenOfIs - 1)
                where
                  g4 (rmin, rmax) = filter (\ts -> rmin <= (sqsTakeTop $ tsRawSqs ts) && (sqsTakeTop $ tsRawSqs ts) <= rmax) applicants
                  g5 xs = Lis.sortBy h5 xs
                    where
                      h5 x y = compare (sqsTakeLeft $ tsRawSqs x) (sqsTakeLeft $ tsRawSqs y)
             longLegs2 = map (\i -> linAssigneds !! i) is
             pairs = zip applClustered longLegs2
             tried1 = map (\x@(orphs, leg) -> fAssOrphs orphs leg) pairs
             tried2
               | applClustered == [] = []
               | otherwise = map (map (\y -> (fst y, V.toList $ snd y))) $ snd $ foldl f1 ([], []) longLegs2
               where
                 f1 y2@(noId, st) ls = (noId ++ [resPrimIndex], st ++ [snd $ snd resPrim])
                    where
                      resPrimIndex = fst resPrim
                      resPrim = Lis.maximumBy g3 scoreEtRes
                      scoreEtRes = indexing $ map (\x -> fAssOrphsEasy x ls) $ nonFetch applClustered noId
                      g3 x y = compare (fst $ snd x) (fst $ snd y)
    linResult = Lis.sortBy gg $ suffed ++ islAssed2
      where
        islAssed2 = map f islAssed
          where
            f x = x {tsRawSqs = [resSq]}
              where
                resSq = CSq {sqTop = toptop, sqLeft = leftleft, sqBot = botbot, sqRight = rightright}
                  where
                    toptop = sqsTakeTop lineSqs
                    botbot = sqsTakeBot lineSqs
                    leftleft = sqsTakeLeft lineSqs
                    rightright = sqsTakeRight lineSqs
                    lineSqs = concatMap tsRawSqs $ fetch tokAssigneds $ concat $ takeSndL $ tsValTable x
        gg x y = compare (tsId x) (tsId y)
    pureTokenSquares = map (\n -> rebuildSquare (tokAssigneds !! n) chLibs) [0 .. (length tokAssigneds - 1)]



nonFetch :: [a] -> [Int] -> [a]
nonFetch as ns
  | ns == [] = as
  | otherwise = res
  where
    res = map (\m -> as !! m) $ filter (\n -> not $ elem n ns) [0 .. length as - 1]

clusterSqs :: [Double] -> Int -> [(Double, Double)]
clusterSqs tops n -- n is number of borderlines
  | tops == [] = []
  | n < 1 = [(minimum tops, maximum tops)]
  | otherwise = res
  where
    minTop = minimum tops - 1.0
    maxTop = maximum tops + 1.0
    pitch = 1.0
    nOfAppl = floor $ (maxTop - minTop) / pitch
    appls = V.toList $ V.enumFromStepN minTop pitch (nOfAppl + 1)
    getTotalDists elems r = sum $ map (\x -> (r - x) ^ 2) elems
    zipped = zip appls $ map (getTotalDists tops) appls
    res = foldl f [(minTop, maxTop)] $ [0 .. n - 1]
      where
        f yAreas _ = res
          where
            ns = [0 .. (length yAreas - 1)]
            optR = fst $ Lis.maximumBy f3 $ map g ns
              where
                f3 x y = compare (snd x) (snd y)
                g n
                  | appls == [] = (mi, 0.0)
                  | otherwise = res2
                  where
                    area@(mi, ma) = yAreas !! n
                    elems = filter (\x -> mi <= x && x <= ma) tops
                    nOfAppl = floor $ (ma - mi) / pitch
                    appls = V.toList $ V.enumFromStepN minTop pitch (nOfAppl + 1)
                    getTotalDists elems r = sum $ map (\x -> (r - x) ^ 2) elems
                    zeroScore = getTotalDists elems mi
                    zipped = zip appls $ map (\r -> zeroScore - (getTotalDists tops r)) appls
                    res2 = (\x -> (fst x, (snd x) / (fromIntegral $ length appls) )) $ Lis.maximumBy f3 zipped
            res = concatMap g2 yAreas
              where
                g2 area@(r1, r2)
                 | r1 == optR && r2 == optR = [(r1, r2)]
                 | r1 <= optR && optR <= r2 = [(r1, optR), (optR, r2)]
                 | otherwise = [(r1, r2)]

sqsTakeTop sqs = maximum $ map sqTop sqs
sqsTakeLeft sqs = maximum $ map sqLeft sqs
sqsTakeBot sqs = maximum $ map sqBot sqs
sqsTakeRight sqs = maximum $ map sqRight sqs

average :: Fractional a => Num a => [a] -> a
average ts = (sum ts) / (fromIntegral $ length ts)

flattenNestL :: [[a]] -> [[a]]
flattenNestL [] = []
flattenNestL (xs : xss) = foldl f (map (\x -> [x]) xs) xss
  where
    f yss xs = [(ys ++ [x]) | x <- xs, ys <- yss]

nmem :: (Foldable t, Eq a) => [a] -> t a -> [a]
nmem lis ngLis = res
  where
    res = filter (\x -> not $ Lis.elem x ngLis) lis

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
data Sq a = CSq {
   sqTop :: a
 , sqLeft :: a
 , sqBot :: a
 , sqRight :: a}
  deriving (Show, Eq, Ord)

pdfPathLoc :: Text.Text
pdfPathLoc = "file:///home/polymony/poppyS/hott.pdf"

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
  x1 <- GPop.getRectangleX1 rect
  x2 <- GPop.getRectangleX2 rect
  y1 <- GPop.getRectangleY1 rect
  y2 <- GPop.getRectangleY2 rect
  -- return ((x1, y1), (x2, y2))
  return $ PopPRectangle x1 y1 x2 y2

showRect :: GPop.Rectangle -> IO (Sq Double)
showRect rect = do
  x1 <- GPop.getRectangleX1 rect
  x2 <- GPop.getRectangleX2 rect
  y1 <- GPop.getRectangleY1 rect
  y2 <- GPop.getRectangleY2 rect
  -- return ((x1, y1), (x2, y2))
  return $ CSq {sqTop = y1, sqLeft = x1, sqBot = y2, sqRight = x2}

cshowIL lis = mapM_ uprint $ zip [0 .. (length lis - 1)] lis
cshowL lis = mapM_ uprint  lis

cshowI lis = cshowIL $ V.toList lis
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

command = "http://localhost:9000/?annotators=parse&outputFormat=json&timeout=10000"
text = "The quick brown fox jumped over the lazy dog."

buildRequest :: String -> RequestBody -> IO Request
buildRequest url body = do
  nakedRequest <- parseRequest url
  return (nakedRequest { method = "POST", requestBody = body })

{-
:set -XOverloadedStrings
-}
nPage = 15



groupLineWLastX :: Fractional a => Ord a => Num a => [[(Char, Sq a)]] -> [[[(Char, Sq a)]]]
groupLineWLastX ls
  | length ls == 0 = []
  | s == [] = p
  | otherwise = p ++ [s]
  where
    headLSLast@(c, sq) = last $ head ls
    getRight chSq@(c, sq) = sqRight sq
    res@(xr,s,p) = foldl f (sqRight sq, [head ls], []) $ tail ls
    f y@(prevX, stacked, pooled) x
     | th < abs dist && dist < 0 = (newX, [], pooled ++ [stacked ++ [x]])
     | th < abs dist && 0 <= dist && (not $ stacked == []) = (newX, [x], pooled ++ [stacked])
     | otherwise = (newX, stacked ++ [x], pooled)
      where
        newX = getRight $ last x
        th = 1.0
        dist = newX - prevX

toSentences :: Num a => [[(Char, Sq a)]] -> String
toSentences chls = init res
  where
    res = concat $ map f chls
    f chl
     | last strs == '-' = init strs
     | otherwise = strs ++ [' ']
      where
        strs = takeFstL chl

forgetIndexed res = forgotten
  where
    reshaped = reshapeSexp $ V.fromList $ map Text.unpack $ Text.split (== '\n')  res
    sexp = fst $ head $ parse pSExp reshaped
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

indexing xs = zip [0 .. (length xs - 1)] xs

takeLeft x = fst $ fst x
takeTop x = snd $ fst x
takeBot x = fst $ snd x
takeRight x = snd $ snd x

flattenSnd :: Foldable t => t (a, [b]) -> [(a, b)]
flattenSnd xs = concatMap (\x -> map (\y -> (fst x, y)) $ snd x) xs

pageGetTokSqLibs3 page layouts layoutMode = do
  rawText <- GPop.pageGetText page
  let
    nubbedChars = map (\c -> Text.pack [c]) $ (Lis.nub . Text.unpack) rawText
  charSqs <- (Lis.nub . flattenSnd) <$> mapM (\x -> getRectSW page x [GPop.FindFlagsCaseSensitive]) nubbedChars
  (wid1, hei1) <- GPop.pageGetSize page
  let
    sqs = takeSndL charSqs
    lefts = map sqLeft sqs
    tops = map sqLeft sqs
    defaultLayout = [CSq {sqTop = 0.0, sqBot = hei1, sqLeft = 0.0, sqRight = wid1}]
    layouts2 = case layoutMode of
      OneTotal -> defaultLayout
      TwoCols -> resTwoCols
       where
         dividCol = getDivideLine lefts
         dividLines = getDivideLayout hei1 wid1 sqs
         resTwoCols = case dividLines of
           Nothing -> defaultLayout
           Just ((resCol, resScore), resTop, resBot) -> [squareTop, squareMidLeft, squareMidRight, squareBot]
             where
               squareTop = CSq {sqTop = 0.0, sqBot = fromIntegral resTop, sqLeft = 0.0, sqRight = wid1}
               squareMidLeft = CSq {sqTop = fromIntegral resTop, sqBot = fromIntegral resBot, sqLeft = 0.0, sqRight = fromIntegral resCol}
               squareMidRight = CSq {sqTop = fromIntegral resTop, sqBot = fromIntegral resBot, sqLeft = fromIntegral resCol, sqRight = wid1}
               squareBot = CSq {sqTop = fromIntegral resBot, sqBot = hei1, sqLeft = 0.0, sqRight = wid1}
  let
    charSqsDivided = map g layouts2
      where
        g sqLayout = filter g2 charSqs
           where
             g2 charSq@(_, sqc) = isSqIncludeSq sqLayout sqc
    eachGetTokSqLib charSqs = clusteredRes
      where
          charSqsSorted = Lis.sortBy f charSqs
            where
              f x@(xTok, xSq) y@(yTok, ySq) = compare (sqBot xSq) (sqBot ySq)
          clusteredOnce
            | charSqsSorted == [] = []
            | otherwise = snocL pl ((tpp, btt), st)
              where
                hed = head charSqsSorted
                res@((tpp, btt), st, pl) = foldl g ((sqTop $ snd hed, sqBot $ snd hed), [hed], []) $ tail charSqsSorted
                g y@((tp, bt), stacked, pooled) x@(tk, sq)
                  | isOverlapped = ((revisedTop, revisedBot), snocL stacked x, pooled)
                  | otherwise = ((sqTop sq, sqBot sq), [x], snocL pooled ((tp, bt), stacked))
                      where
                        isOverlapped = (tp <= (sqTop sq) && (sqTop sq) <= bt) || (tp <= (sqBot sq) && (sqBot sq) <= bt)
                        revisedTop
                          | tp < (sqTop sq) = tp
                          | otherwise = (sqTop sq)
                        revisedBot
                          | bt < (sqBot sq) = sqBot sq
                          | otherwise = bt
          clusteredOnceSorted = Lis.sortBy g2 clusteredOnce
            where
              g2 x y = compare (snd $ fst x) (snd $ fst y)
          clustering clust
            | clust == [] = []
            | clust == tempRes = clust
            | otherwise = clustering tempRes
            where
              lenOfClus = length clust
              hed@(range, stak) = head clust
              foldRes@((tpp, btt), st, pl) = foldl g (range, stak, []) $ tail clust
                where
                  g y@(rangy@(yTop, yBot), stacked, pooled) x@(rangx@(xTop, xBot), xVal)
                    | isOverlapped = ((revisedTop, revisedBot), stacked ++ xVal, pooled)
                    | otherwise = ((xTop, xBot), xVal, snocL pooled ((yTop, yBot), stacked))
                    where
                      isOverlapped = (yTop <= xTop && xTop <= yBot) || (yTop <= xBot && xBot <= yBot)
                      revisedTop
                        | yTop < xTop = yTop
                        | otherwise = xTop
                      revisedBot
                        | yBot < xBot = xBot
                        | otherwise = yBot
              tempRes = Lis.sortBy g2 $ snocL pl ((tpp, btt), st)
                where
                  g2 x y = compare (snd $ fst x) (snd $ fst y)
          clusteredRes = map (\ys -> map g5 $ Lis.groupBy g4 $ Lis.sortBy g3 ys) $ takeSndL $ clustering clusteredOnceSorted
            where
              g3 x y = compare (sqLeft $ snd x) (sqLeft $ snd y)
              g4 x y = (snd x) == (snd y)
              g5 xs = head $ Lis.sortBy (\x -> \y -> compare (fst x) (fst y)) xs
  return $ map eachGetTokSqLib charSqsDivided

pageGetCharSqs page = do
  charSq <- pageGetTokSqLibs2 page
  rawText <- GPop.pageGetText page
  let
    nubbedChars = map (\c -> Text.pack [c]) $ (Lis.nub . Text.unpack) rawText
    space = [Text.pack [' ']]

  charSqs <- (Lis.nub . flattenSnd) <$> mapM (\x -> getRectSW page x [GPop.FindFlagsCaseSensitive]) nubbedChars
  return charSqs

pageGetTokSqLibs2 page = do
  rawText <- GPop.pageGetText page
  let
    nubbedChars = map (\c -> Text.pack [c]) $ (Lis.nub . Text.unpack) rawText
    space = [Text.pack [' ']]

  charSqs <- (Lis.nub . flattenSnd) <$> mapM (\x -> getRectSW page x [GPop.FindFlagsCaseSensitive]) nubbedChars
  let
    charSqsSorted = Lis.sortBy f charSqs
      where
        f x@(xTok, xSq) y@(yTok, ySq) = compare (sqBot xSq) (sqBot ySq)
    clusteredOnce
      | charSqsSorted == [] = []
      | otherwise = snocL pl ((tpp, btt), st)
         where
           hed = head charSqsSorted
           res@((tpp, btt), st, pl) = foldl g ((sqTop $ snd hed, sqBot $ snd hed), [hed], []) $ tail charSqsSorted
           g y@((tp, bt), stacked, pooled) x@(tk, sq)
             | isOverlapped = ((revisedTop, revisedBot), snocL stacked x, pooled)
             | otherwise = ((sqTop sq, sqBot sq), [x], snocL pooled ((tp, bt), stacked))
                where
                  isOverlapped = (tp <= (sqTop sq) && (sqTop sq) <= bt) || (tp <= (sqBot sq) && (sqBot sq) <= bt)
                  revisedTop
                   | tp < (sqTop sq) = tp
                   | otherwise = (sqTop sq)
                  revisedBot
                   | bt < (sqBot sq) = sqBot sq
                   | otherwise = bt
    clusteredOnceSorted = Lis.sortBy g2 clusteredOnce
       where
         g2 x y = compare (snd $ fst x) (snd $ fst y)
    clustering clust
      | clust == [] = []
      | clust == tempRes = clust
      | otherwise = clustering tempRes
      where
        lenOfClus = length clust
        hed@(range, stak) = head clust
        foldRes@((tpp, btt), st, pl) = foldl g (range, stak, []) $ tail clust
          where
            g y@(rangy@(yTop, yBot), stacked, pooled) x@(rangx@(xTop, xBot), xVal)
              | isOverlapped = ((revisedTop, revisedBot), stacked ++ xVal, pooled)
              | otherwise = ((xTop, xBot), xVal, snocL pooled ((yTop, yBot), stacked))
               where
                isOverlapped = (yTop <= xTop && xTop <= yBot) || (yTop <= xBot && xBot <= yBot)
                revisedTop
                  | yTop < xTop = yTop
                  | otherwise = xTop
                revisedBot
                  | yBot < xBot = xBot
                  | otherwise = yBot
        tempRes = Lis.sortBy g2 $ snocL pl ((tpp, btt), st)
          where
            g2 x y = compare (snd $ fst x) (snd $ fst y)
    clusteredRes = map (Lis.nubBy g4 . Lis.sortBy g3 . Lis.sortBy g30) $ takeSndL $ clustering clusteredOnceSorted
       where
         g30 x y = compare (fst x) (fst y)
         g3 x y = compare (sqLeft $ snd x) (sqLeft $ snd y)
         g4 x y = snd x == snd y
    clusterdResLastSpace = map ff clusteredRes
       where
         ff xs = snocL xs dummySpace
           where
             lastCharSq = snd $ last xs
             tp = sqTop lastCharSq
             bt = sqBot lastCharSq
             rig = sqRight lastCharSq
             dummySpace = (Text.pack " ", CSq {sqTop = tp, sqBot = bt, sqLeft = rig, sqRight = rig})
    forCheck = map (concat . (map Text.unpack) . takeFstL) clusteredRes
  return clusterdResLastSpace

pageGetTokSqLibs :: GPop.Page -> IO
          ([TokSq Text.Text Double], [TokSq Text.Text Double],
           [TokSq Text.Text Double])
pageGetTokSqLibs page = do
  rawText <- GPop.pageGetText page
  let
    linesPrim = Text.lines rawText -- Don't nub.
    histo = getHistogram linesPrim
    specLargeChars
      | histo == [] = []
      | otherwise = map (\c -> Text.pack [c]) $ takeSndL $ filter (\x@(n,x2) -> 500 < n) charsHisto
        where
          charsHisto = getHistogram $ filter (\c -> not $ c == ' ' || c == '\n') $ Text.unpack rawText

    isTooMany = (not $ [] == specLargeChars)
    lines
      | isTooMany = filter (\x -> not $ x == (Text.pack " ")) $ filter (\x -> not $ x == (Text.pack "")) $ map (deleteChars $ specLargeChars ++ [Text.pack "  "]) linesPrim
      | otherwise = linesPrim
    nubbedTokens = map (Text.pack) $ Lis.nub $ concatMap (\lin -> delimitAtWO2 ' ' id $ Text.unpack lin) lines
    nubbedChars = filter (\c -> not $ elem c specLargeChars) $ map (\c -> Text.pack [c]) $ (Lis.nub . Text.unpack) rawText
    space = [Text.pack [' ']]

  lineSqs <- mapM (\x -> getRectSW page x [GPop.FindFlagsCaseSensitive, GPop.FindFlagsWholeWordsOnly]) lines
  spaceSqs <- (Lis.nub . flattenSnd) <$> mapM (\x -> getRectSW page x [GPop.FindFlagsCaseSensitive]) space
  tokenSqs <- (\y -> y ++ spaceSqs) <$> (Lis.nub . flattenSnd) <$> mapM (\x -> getRectSW page x [GPop.FindFlagsCaseSensitive, GPop.FindFlagsWholeWordsOnly]) nubbedTokens
  charSqs <- (Lis.nub . flattenSnd) <$> mapM (\x -> getRectSW page x [GPop.FindFlagsCaseSensitive]) nubbedChars
  let
    lineLibs = map create2 $ indexing lineSqs
      where
        create2 (i, (tok, sqs)) = CTokSq { tsId = i, tsRawVal = tok, tsRawSqs = sqs, tsValTable = delimited, tsIncOrphs = []}
          where
            delimited = map (\tok -> (Text.pack tok, [])) $ delimitAtW2 ' ' id $ Text.unpack tok
    tokenLibs = map create $ indexing tokenSqs
      where
        create (i, (tok, sq)) = CTokSq { tsId = i, tsRawVal = tok, tsRawSqs = [sq], tsValTable = delimited, tsIncOrphs = []}
          where
            delimited = map (\c -> (Text.pack [c], [])) $ Text.unpack tok
    charLibs = map create $ indexing charSqs
      where
        create (i, (tok, sq)) = CTokSq { tsId = i, tsRawVal = tok, tsRawSqs = [sq], tsValTable = delimited, tsIncOrphs = []}
          where
            delimited = map (\c -> (c, [])) [tok]
  return (lineLibs, tokenLibs, charLibs)

deleteChars :: [Text.Text] -> Text.Text -> Text.Text
deleteChars strs str = foldl (\y -> \x -> Text.replace x (Text.pack "") y) str strs

groupSucc :: [Int] -> [[Int]]
groupSucc [] = []
groupSucc (i:is) = plp ++ [stp]
  where
    resPrim@(ip, stp, plp) = foldl f (i, [i], []) is
    f y@(ind, st, pl) x
     | ind + 1 == x = (x, st ++ [x], pl)
     | otherwise = (x, [x], pl ++ [st])

getDivideLayout :: Double -> Double -> [Sq Double] -> Maybe ((Int, Int), Int, Int)
getDivideLayout hei1 wid1 sqs
  | sqs == [] = Nothing
  | otherwise = result
  where
    rowCentre = floor $ hei1 * 0.5
    colCentre = floor $ wid1 * 0.5
    minCol = floor $ minimum $ map sqLeft sqs :: Int
    maxCol = floor $ maximum $ map sqRight sqs :: Int
    spanCol = [minCol .. maxCol]
    spanRowUpper = reverse [0 .. (rowCentre - 1)]
    spanRowLower = [rowCentre .. ((floor hei1) - 1)]
    caves = filter (\x@(n, c) -> not $ c == 0) $ map f spanCol
      where
        f c = (c, lenCaveUpper + lenCaveLower)
          where
            lenCaveUpper = length $ takeWhile g spanRowUpper
            lenCaveLower = length $ takeWhile g spanRowLower
            g r = [] == filter (isIncludePoint (fromIntegral r, fromIntegral c)) sqs
    cavesConnected
     | caves == [] = []
     | otherwise = res3
      where
        res@(pN, stt, pll) = foldl ff (fst $ head caves, [head caves], []) $ tail caves
           where
             ff x@(prevN, st, pl) y@(n, coun)
               | prevN + 1 == n = (n, snocL st y, pl)
               | otherwise = (n, [y], snocL pl st)
        res2 = snocL pll stt
        res3 = map gg res2
          where
            gg :: [(Int, Int)] -> (Int, Int)
            gg xs = (floor ressss, sum $ takeSndL xs)
              where
                expanded = concatMap (\xx@(xx1, xx2) -> replicate xx2 xx1) xs
                ressss = (fromIntegral $ sum expanded) / (fromIntegral $ length expanded)
    centerCol
     | cavesConnected == [] = Nothing
     | otherwise = Just (Lis.maximumBy (\x -> \y -> compare (snd x) (snd y)) cavesConnected)
    result = case centerCol of
      Nothing -> Nothing
      Just (colRes, score) -> Just (f colRes)
        where
          f c = ((colRes, score), topRes, botRes)
            where
              topRes = last $ takeWhile g spanRowUpper
              botRes = last $ takeWhile g spanRowLower
              g r = [] == filter (isIncludePoint (fromIntegral r, fromIntegral c)) sqs




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

getMecabed :: String -> IO (V.Vector MData)
getMecabed sens = do
  let
    cmd = Turtle.fromString $ "echo " ++ "\""  ++ sens ++ "\"" ++ " | ginzame"
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

parseNPsJP :: [(String, String)] -> [[(String, String)]]
parseNPsJP pairs
 | pairs == [] = []
 | otherwise = pooled ++ [stacked]
    where
      resPrim@(stacked, pooled) = foldl f ([head pairs], []) (tail pairs)
         where
           nouns = ["名詞", "形容詞", "接頭詞", "連体詞"]
           f y@(stacked, pooled) x
             | elem (snd x) nouns && (not $ stacked == []) && (elem lTag nouns) = (stacked ++ [x], pooled)
             | otherwise = ([x], pooled ++ [stacked])
                 where
                   lTag = snd $ last stacked

parseNPsJP1 :: V.Vector MData -> V.Vector (V.Vector MData)
parseNPsJP1 pairs
 | pairs == V.empty = V.empty
 | otherwise = V.snoc pooled stacked
    where
      resPrim@(stacked, pooled) = V.foldl' f (V.singleton $ V.head pairs, V.empty) (V.tail pairs)
         where
           nouns = V.fromList ["名詞", "形容詞", "接頭詞", "連体詞"]
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

makeInitialJPSExp :: [MData] -> [SExp (Bool, MData) String]
makeInitialJPSExp mds = map f mds
   where
     f md = Atom (False, md) $ mToken md

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
