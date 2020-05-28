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
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.ForeignPtr (withForeignPtr)

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
prp
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
nPage = 21
nPage = 24
nPage = 289
nPage = 118
doc <- GPop.documentNewFromFile pdfPath Nothing
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


--prp
getSExpsIOS page = do
  (wid, hei) <- GPop.pageGetSize page
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
        -- blockLineChars = V.map (V.map (V.map fst)) charSqV -- :: V.Vector (V.Vector (V.Vector Char)) -- first is Block second is lines in Block last is Line in Lines
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
  -- prp
    retrSexpS (sens, charSqsPrim) = do
      stanRess <- stanIOPoppy1 sens
      let
        charSqs = V.filter (\x -> (not $ fst x == '\n')) charSqsPrim
        sexps = stanAssign2Poppy1 charSqs stanRess
      return sexps
    zipped = V.zip stackedSens charSqVConcated
  bb <- V.mapM retrSexpS zipped
  return $ concatMap id bb

data ChInfo = CChInfo {
    chIChar :: Text.Text
  , chISq :: Sq Double
  , chIFontName :: [Maybe Text.Text]
  , chIFontSize :: [Double]
  }
    deriving (Eq, Ord, Show)

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

-- prp
getBlock chInfos hei wid
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

--kesuna
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



isIncludedPopplerRect ns@(PopPRectangle a b c d) nd@(PopPRectangle e f g h)
  = (a <= e) && (b <= f) && (g <= c) && (h <= d)

popPPageFindText page token = do
  -- rects <- GPop.pageFindText page token
  rects <- GPop.pageFindTextWithOptions page token [GPop.FindFlagsCaseSensitive]
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

pRectToSqNoSwap :: PopPRectangle -> Sq Double
pRectToSqNoSwap (PopPRectangle smallX smallY bigX bigY) = CSq {sqTop =  smallY, sqLeft = smallX, sqBot = bigY, sqRight = bigX}

-- docPathSuffix = "./pdfs/CTFP.pdf"
replaceStr nd ns str = Text.unpack $ Text.replace nd ns $ Text.pack str




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

indexing xs = zip [0 .. (length xs - 1)] xs

takeLeft x = fst $ fst x
takeTop x = snd $ fst x
takeBot x = fst $ snd x
takeRight x = snd $ snd x

flattenSnd :: Foldable t => t (a, [b]) -> [(a, b)]
flattenSnd xs = concatMap (\x -> map (\y -> (fst x, y)) $ snd x) xs






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
