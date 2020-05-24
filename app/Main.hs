{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (when, void)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import qualified Data.Text as Text
import qualified Data.List as Lis
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import qualified Network.HTTP.Simple as HT
import System.Environment
import Data.Aeson
import Data.Time
import System.Timeout
import Network.HTTP.Client
import qualified Control.Lens as LENS
import qualified Data.Aeson.Lens as AL
import Data.Text.ICU.Convert as ICU
import qualified Data.ByteString.Lazy as BL
import Lib (iVecFromFile, oVecToFile, delimitAtWO2, delimitAtWO2By, vNub, takeFst, takeSnd, takeFstT, takeSndT, takeThdT, execShell, delimitAtW2, getDivideLine, countPrefix, countSuffix)
-- import PopSExp (indexingSP, forgetSExp, injectSExpI, reconsSExp, showSP)
import PopSExp
import ParserP (parse, pSExp)
import SExp
import System.Process
import System.Directory
import Data.Algorithm.Diff
import FromPDF
import Data.IORef
import Control.Monad.ST (RealWorld)
import GHC.Int (Int32)
import Data.Char
import NLP.Snowball
import Text.Show.Unicode

import Data.GI.Base

import qualified GI.Cairo as GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as GP
import qualified GI.Gtk as Gtk
import qualified GI.Poppler as GPop

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)


--pdfPathPrefix :: String
--pdfPathPrefix = "file:///home/polymony/poppyS/pdfs/"

pdfFilesDir :: String
pdfFilesDir = "./pdfs"

--configFilesDir :: String
--configFilesDir = "./configs"

stemEng = Text.unpack . (stem English) . Text.pack
toLowers = map toLower
toUppers = map toUpper

initialSize :: Int
initialSize = 1024

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))

-- ppp main = mainGtk
main = do
  homePath <- getEnv "HOME"
  let
    poppySPath = homePath ++ "/poppyS"
  fpathPrim <- head <$> getArgs
  cwd <- getCurrentDirectory
  let
    fpath
     | isNotFull = "file://" ++ cwd ++ "/" ++ fpathPrim
     | takeWhile (\c -> not $ c == ':') fpathPrim == "file" = fpathPrim
     | (0 < length fpathPrim) && (take 1 fpathPrim == "/") = "file://" ++ fpathPrim
     | otherwise = "file:///" ++ fpathPrim
       where
         isNotFull = not $ Lis.isPrefixOf cwd fpathPrim

  -- oVecToFile (V.fromList hogeCheck) "/home/polymony/rks.txt"
  mainGtk fpath poppySPath

mainGtk :: String -> String -> IO ()
mainGtk fpath poppySPath = do
  _ <- Gtk.init Nothing
  (window, movePallete) <- initWidgets poppySPath
  docs <- initDocs poppySPath
  docsRef <- newIORef docs
  doc <- initDoc docsRef fpath
  docRef <- newIORef doc
  let
    currDoc = dkCurrDoc doc
    currPage = dkCurrPage doc
  mvarsPrim <- initMVars docRef
  mVars <- newMVar mvarsPrim

  on window #keyPressEvent $ \event -> do
    name <- event `get` #keyval >>= Gdk.keyvalName
    let
      fff name = case name of
        Nothing -> return ()
        Just nm -> do
          modifyIORef docsRef (\x -> x {dksKeysStacked = (dksKeysStacked x) ++ [Text.unpack nm]})
      incl n nOfPage = mod (n + 2) nOfPage
      decl n nOfPage = mod (n - 2) nOfPage
      incl1 n nOfPage = mod (n + 1) nOfPage
      decl1 n nOfPage = mod (n - 1) nOfPage
      registeredKeys = [["j"], ["k"], ["f"], ["d"], ["Down"], ["Left"], ["Right"], ["p"], ["x"], ["c", "c"], ["Escape"], ["colon", "w", "Return"], ["g","g"]]
    fff name
    stKeys <- dksKeysStacked <$> readIORef docsRef
    let
      isSomethingMatched = or $ map (\keys -> Lis.isPrefixOf stKeys keys) registeredKeys
    Gtk.windowSetTitle window $ Text.pack $ show stKeys
    when (stKeys == ["j"] || stKeys == ["f"]) $ do
      goOtherPage window docRef incl incl
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["k"] || stKeys == ["d"]) $ do
      goOtherPage window docRef decl decl
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["Down"]) $ do
      resizeFromCurrPageSqs window docsRef docRef mVars
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["Left"]) $ do
      goOtherPage window docRef decl1 decl1
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["Right"]) $ do
      goOtherPage window docRef incl1 incl1
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["g", "g"]) $ do
      goOtherPage window docRef (\n -> \m -> 0) (\n -> \m -> 1)
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["p"]) $ do
      modifyIORef docRef (\x -> x {dkConfig = dkConfigYank x})
      Gtk.widgetQueueDraw window
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["x"]) $ do
      docs <- readIORef docsRef
      let
        isDeleting = (dksIsDeleting docs) == True
        windowRepr
         | isDeleting = "Deleting Stopped."
         | otherwise = "Deleting Start."
      modifyIORef docsRef (\x -> x {dksIsDeleting = not $ dksIsDeleting x})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window windowRepr
      return ()
    when (stKeys == ["c", "c"]) $ do
      modifyIORef docRef (\x -> x {dkConfigYank = dkConfig x, dkConfig = []})
      Gtk.widgetQueueDraw window
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["colon", "w", "Return"]) $ do
      docs <- readIORef docsRef
      doc <- readIORef docRef
      let
        colors = dksColors docs
        conf = res
           where
             res = map (\x -> (fst x) ++ "," ++ (decodeConfig colors $ snd x)) $ dkConfig doc
        confGlobal = res
           where
             res = map (\x -> (fst x) ++ "," ++ (decodeConfig colors $ snd x)) $ dksGlobalConfig docs
        currPage = dkCurrPage doc
        poppySPath = dksPoppySPath docs
        configFilesDir = poppySPath ++ "/configs"
        outout = V.fromList $ (show currPage) : conf
        currDocName = dkPDFDocName doc
        configFilePath = configFilesDir ++ "/" ++ currDocName ++ "_config.txt"
      oVecToFile outout configFilePath
      oVecToFile (V.fromList confGlobal) globalConfigFilePath
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window "Config Saved."
      return ()
    when (name == Just "Escape") $ do
      --modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      --modifyIORef docsRef (\x -> x {dksIsDeleting = False})
      --Gtk.windowSetTitle window "Neutralized."
      Gtk.mainQuit
    when (not isSomethingMatched) $ do
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
    return False

  forkIO $ do
    stackStan window mVars docsRef docRef

  on window #buttonPressEvent $ \event -> do
    isNonMis <- dksForMisDoublePress <$> readIORef docsRef
    if (isNonMis == False)
      then do
       modifyIORef docsRef (\x -> x {dksForMisDoublePress = True})
       return False
      else
       do
        modifyIORef docsRef (\x -> x {dksForMisDoublePress = False})
        docs <- readIORef docsRef
        doc <- readIORef docRef
        mvs <- readMVar mVars
        let
          globalConf = dksGlobalConfig docs
          isDeleting = dksIsDeleting docs
          clipSq = dkClipSq doc
          pageLeft = sqLeft clipSq
          pageTop = sqTop clipSq
          pageRight = sqRight clipSq
          pageBot = sqBot clipSq
          clipSqNext = dkClipSq doc
          pageLeftNext = sqLeft clipSqNext
          pageTopNext = sqTop clipSqNext
          pageRightNext = sqRight clipSqNext
          pageBotNext = sqBot clipSqNext
          currDoc = dkCurrDoc doc
          currPage = dkCurrPage doc
          nextPage = dkNextPage doc
        page <- GPop.documentGetPage currDoc currPage
        pageNext <- GPop.documentGetPage currDoc currPage
        (pWid', pHei') <- GPop.pageGetSize page
        (pWidNext', pHeiNext') <- GPop.pageGetSize pageNext
        hw@(wWid, wHei) <- Gtk.windowGetSize window
        let
          ratio = (fromIntegral wHei) / (pageBot - pageTop)
          sexpsMV = mVarSExps mvs
        colReal <- get event #x
        rowReal <- get event #y
        button <- get event #button -- left click is 1, right click is 3
        sexpsMaybe <- MV.read sexpsMV (fromIntegral currPage)
        sexpsMaybeNext <- MV.read sexpsMV (fromIntegral nextPage)
        let
          detacheds =
            case sexpsMaybe of
              Nothing -> []
              Just sexps -> map g2 $ filter g $ takeSndL $ concatMap forgetSExp $ map (mapNode snd (\x -> (fst x, synSqs $ snd x))) sexps
                where
                  g x = case x of
                    Nothing -> False
                    Just _ -> True
                  g2 x = case x of
                    Nothing -> ("", [])
                    Just y -> y
          detachedsNext =
            case sexpsMaybeNext of
              Nothing -> []
              Just sexps -> map g2 $ filter g $ takeSndL $ concatMap forgetSExp $ map (mapNode snd (\x -> (fst x, synSqs $ snd x))) sexps
                where
                  g x = case x of
                    Nothing -> False
                    Just _ -> True
                  g2 x = case x of
                    Nothing -> ("", [])
                    Just y -> y

          offSetXAll = - pageLeft
          offSetYAll = - pageTop
          point = CPos {posX = colReal / ratio - offSetXAll, posY = rowReal / ratio - offSetYAll}
          offSetXAllNext = (pWid' - pageLeftNext - (pWid' - pageRight) - pageLeft)
          offSetYAllNext = - pageTopNext
          pointNext = CPos {posX = colReal / ratio -offSetXAllNext , posY = rowReal / ratio - offSetYAllNext}
          stemmed = map (\x -> (stemEng $ fst x, snd x)) detacheds
          stemmedNext = map (\x -> (stemEng $ fst x, snd x)) detachedsNext
          wordSq = filter f stemmed
            where
              f x@(str, sqs) = or $ map (\sq -> isSqIncludePoint sq point) sqs
          wordSqNext = filter f stemmedNext
            where
              f x@(str, sqs) = or $ map (\sq -> isSqIncludePoint sq pointNext) sqs
          words = takeFstL wordSq
          wordsNext = takeFstL wordSqNext

          colors = dksColors docs
          isFail = words == []
          isFailNext = wordsNext == []
          word
            | isFail && isFailNext = ""
            | not isFail = head words
            | otherwise = head wordsNext
          currConfig = dkConfig doc
          getAlrdConfigsSimilar globalConf
            | filtered == [] = []
            | otherwise = [mostSimilar]
            where
              mostSimilar = fst $ Lis.maximumBy g filtered
              filtered = filter (\x@(x1, n) -> (4 < n ) || (n == lenOfWord)) $  map f2 globalConf
              g x y = compare (snd x) (snd y)
              lenOfWord = length word
              f2 x@(tok, _)
              --   | nCommPref < nCommSuf = (x, nCommSuf)
                | otherwise = (x, nCommPref)
                where
                  nCommPref = length $ countPrefix tok word
                  nCommSuf = length $ countSuffix tok word
          -- to be fixed point.
          alrdGlobalConfigs = filter (\x -> (fst x) == word) globalConf
          alrdLocalConfigs  = filter (\x -> (fst x) == word) currConfig
          alrdGlobalConfigsSimilar = getAlrdConfigsSimilar globalConf
          alrdLocalConfigsSimilar  = getAlrdConfigsSimilar currConfig
          isAlreadyG = not $ alrdGlobalConfigs == []
          isAlreadyL = not $ alrdLocalConfigs == []
          isAlreadyGSimilar = not $ alrdGlobalConfigsSimilar == []
          isAlreadyLSimilar = not $ alrdLocalConfigsSimilar == []
          forWinTitle = (show word)
          colorIndex = dkTogColIndex doc
          (newInd, newColor)
            | isAlreadyL && button == 1 = (colorIndex, togging currCol)
            | isAlreadyL = (colorIndex, toggingRev currCol)
            | isAlreadyG = (colorIndex, currCol)
            | isAlreadyLSimilar = (colorIndex, currColSimilarL)
            | isAlreadyGSimilar = (colorIndex, currColSimilarG)
            | otherwise = (newIndex, color)
             where
              currCol = snd $ head alrdGlobalConfigs
              currColSimilarL = snd $ head alrdLocalConfigsSimilar
              currColSimilarG = snd $ head alrdGlobalConfigsSimilar
              newIndexTemp
                | button == 1 = (colorIndex + 1)
                | otherwise = (colorIndex - 1)
              color
                | mod newIndexTemp 8 == 0 = colLime colors
                | mod newIndexTemp 8 == 1 = colRed colors
                | mod newIndexTemp 8 == 2 = colBlue colors
                | mod newIndexTemp 8 == 3 = colGreen colors
                | mod newIndexTemp 8 == 4 = colPurple colors
                | mod newIndexTemp 8 == 5 = colOrange colors
                | mod newIndexTemp 8 == 6 = colPink colors
                | otherwise = colAqua colors
              newIndex = mod newIndexTemp 8
              togging col
               | col == colRed colors = colBlue colors
               | col == colBlue colors = colGreen colors
               | col == colGreen colors = colPurple colors
               | col == colPurple colors = colOrange colors
               | col == colOrange colors = colPink colors
               | col == colPink colors = colAqua colors
               | col == colAqua colors = colLime colors
               | otherwise = colRed colors
              toggingRev col
               | col == colBlue colors = colRed colors
               | col == colGreen colors = colBlue colors
               | col == colPurple colors = colGreen colors
               | col == colOrange colors = colPurple colors
               | col == colPink colors = colOrange colors
               | col == colAqua colors = colPink colors
               | col == colLime colors = colAqua colors
               | otherwise = colLime colors
          newIndex = mod newInd 8
        if not (isFail && isFailNext)
          then
           if not isDeleting
           then
             do
              let
                 dropped = filter (\x -> not $ (fst x) == word) $ dkConfig doc
                 droppedG = filter (\x -> not $ (fst x) == word) $ dksGlobalConfig docs
              modifyIORef docRef (\doc -> doc {dkCurrToken = word, dkConfig = [(word, newColor)] ++ dropped, dkTogColIndex = newIndex})
              modifyIORef docsRef (\docs -> docs {dksGlobalConfig = [(word, newColor)] ++ droppedG})
              Gtk.widgetQueueDraw window
           else
             do
               let
                 dropped = filter (\x -> not $ (fst x) == word) $ dkConfig doc
               modifyIORef docRef (\doc -> doc {dkCurrToken = word, dkConfig = dropped, dkTogColIndex = newIndex})
               -- modifyIORef docsRef (\docs -> docs {dksIsDeleting = False})
               Gtk.widgetQueueDraw window
          else return ()
        Gtk.windowSetTitle window $ Text.pack forWinTitle
        return False

  on window #buttonReleaseEvent $ \event -> do
    pressType <- get event #type
    return False

  on window #motionNotifyEvent $ \event -> do
    col <- get event #x
    row <- get event #y
    return False

  on window #draw $ \context -> do
    docs <- readIORef docsRef
    doc <- readIORef docRef
    mvs <- readMVar mVars
    let
      clipSq = dkClipSq doc
      pageLeft = sqLeft clipSq
      pageTop = sqTop clipSq
      pageRight = sqRight clipSq
      pageBot = sqBot clipSq
      clipSqNext = dkClipSq doc
      pageLeftNext = sqLeft clipSqNext
      pageTopNext = sqTop clipSqNext
      pageRightNext = sqRight clipSqNext
      pageBotNext = sqBot clipSqNext
      currDoc = dkCurrDoc doc
      currPage = dkCurrPage doc
      nextPage = dkNextPage doc
      colors = dksColors docs
    page <- GPop.documentGetPage currDoc currPage
    nOfPage <- GPop.documentGetNPages currDoc
    pageNext <- GPop.documentGetPage currDoc currPage
    (pWid', pHei') <- GPop.pageGetSize page
    (pWidNext', pHeiNext') <- GPop.pageGetSize pageNext
    hw@(wWid, wHei) <- Gtk.windowGetSize window
    let
      ratio = (fromIntegral wHei) / (pageBot - pageTop)
      ratioNext = (fromIntegral wHei) / (pageBotNext - pageTopNext)
      sexpsMV = mVarSExps mvs
    sexpsMaybe <- MV.read sexpsMV (fromIntegral currPage)
    sexpsMaybeNext <- MV.read sexpsMV (fromIntegral nextPage)
    let
      configs = dkConfig doc
      alreadies = takeFstL configs
      detacheds =
        case sexpsMaybe of
          Nothing -> []
          Just sexps -> map g2 $ filter g $ takeSndL $ concatMap forgetSExp $ map (mapNode snd (\x -> (fst x, synSqs $ snd x))) $ concatMap (takeSpecTags (\tg -> (snd tg) == NP)) sexps
            where
              g x = case x of
                Nothing -> False
                Just _ -> True
              g2 x = case x of
                Nothing -> ("", [])
                Just y -> y
    let
      detachedsNext =
        case sexpsMaybeNext of
          Nothing -> []
          Just sexps -> map g2 $ filter g $ takeSndL $ concatMap forgetSExp $ map (mapNode snd (\x -> (fst x, synSqs $ snd x))) $ concatMap (takeSpecTags (\tg -> (snd tg) == NP)) sexps
            where
              g x = case x of
                Nothing -> False
                Just _ -> True
              g2 x = case x of
                Nothing -> ("", [])
                Just y -> y
      ngStems = ["that", "this", "them", "their", "these", "those", "from"]
      stemmed = filter (\x -> (3 < (length $ fst x)) && (not $ elem (fst x) ngStems)) $ map (\x -> (stemEng $ fst x, snd x)) detacheds
      stemmedNext = filter (\x -> (3 < (length $ fst x)) && (not $ elem (fst x) ngStems)) $  map (\x -> (stemEng $ fst x, snd x)) detachedsNext
      stemmedOrdered = stemmed ++ stemmedNext
      specials = takeFstL $ filter (\x -> not $ elem (fst x) $ alreadies) $ take 5 $ reverse $ Lis.sortBy f $ V.toList $ getHistogram $ V.fromList $ takeFstL stemmedOrdered
         where
           f x y = compare (snd x) (snd y)
      specialsPrius = map g specials
         where
           stemmedOrderedTagged = (map (\x -> (0, x)) stemmed) ++ (map (\x -> (1, x)) stemmedNext)
           g tok = head $ filter (\y -> tok == (fst $ snd y)) stemmedOrderedTagged
      rectsSpecial@(special0, special1) =
        let
          concated = map (\x@(i, (tk, sq)) -> (i, sq)) $ specialsPrius
          concated0 = concat $ takeSndL $ filter (\x -> fst x == 0) concated
          concated1 = concat $ takeSndL $ filter (\x -> fst x == 1) concated
          gold = colGold colors
          res0 = map (\x -> (\y -> (gold, y)) <$> sqToRect x) concated0
          res1 = map (\x -> (\y -> (gold, y)) <$> sqToRect x) concated1
        in
          (res0, res1)
    let
      sexps = case sexpsMaybe of
        Just sexpsPrim -> sexpsPrim
        Nothing -> []
      nextSexps = case sexpsMaybeNext of
        Just sexpsPrim -> sexpsPrim
        Nothing -> []
      electeds = getColundRectangles sexps configs
      electedsNext = getColundRectangles nextSexps configs
    rects <- sequence electeds
    rectsNext <- sequence electedsNext
    rectsSpec <- sequence special0
    rectsSpecNext <- sequence special1
    page <- GPop.documentGetPage currDoc currPage
    hw@(width, height) <- Gtk.windowGetSize window
    (pWid', pHei') <- GPop.pageGetSize page
    let
    renderWithContext context $ do
      save
      setOperator OperatorSource
      setSourceRGBA 255 255 255 1
      paint
      restore
    renderWithContext context $ do
       save
       scale ratio ratio
       translate (- pageLeft) (- pageTop)
       zeroRect <- GPop.rectangleNew
       GPop.pageRender page context
       _ <- mapM (\x@(col, rect) -> GPop.pageRenderSelection page context rect zeroRect GPop.SelectionStyleGlyph col $ colWhite colors) $ rectsSpec ++ rects
       restore

    -- for NextPage(If exists)
    if (0 < nextPage) && (nextPage < (fromIntegral nOfPage) - 1) then
      renderWithContext context $ do
          pageNext <- GPop.documentGetPage currDoc nextPage
          save
          scale ratioNext ratioNext
          translate (pWid' - pageLeftNext - (pWid' - pageRight) - pageLeft) (- pageTopNext)
          zeroRect <- GPop.rectangleNew
          GPop.pageRender pageNext context
          _ <- mapM (\x@(col, rect) -> GPop.pageRenderSelection pageNext context rect zeroRect GPop.SelectionStyleGlyph col $ colWhite colors) $ rectsSpecNext ++ rectsNext
          restore
      else return ()
    return True
  on window #destroy Gtk.mainQuit

  #showAll window

  Gtk.main

data Colors = CColors
  { colRed :: GPop.Color
  , colBlue :: GPop.Color
  , colGreen :: GPop.Color
  , colWhite :: GPop.Color
  , colNavy :: GPop.Color
  , colLime :: GPop.Color
  , colAqua :: GPop.Color
  , colYellow :: GPop.Color
  , colPink :: GPop.Color
  , colOlive :: GPop.Color
  , colPurple :: GPop.Color
  , colBrown :: GPop.Color
  , colOrange :: GPop.Color
  , colGold :: GPop.Color
  }

 --aaa

data MVars = CMVars{
    mVarSExps :: (MV.MVector RealWorld (Maybe [(SExp (Posi, Tag) (String, [Sq Double]))]))
  , mVarLayouts :: (MV.MVector RealWorld [Sq Double])
  , mVarWordRootHisto :: (MV.MVector RealWorld (Maybe (String, Int)))
  , mVarDefaultLayout :: [Sq Double]
  , mVarDefaultLayoutMode :: LayoutMode
  }

data Mode = Hint | Adhoc | Gramatica | Primitive | Scout
  deriving (Show, Eq)


data Docs = CDocs {
    dksPoppySPath :: String
  , dksOffSetDX :: Double
  , dksOffSetDY :: Double
  , dksOffSetNextTo :: Double
  , dksDebug :: Mode
  , dksIsVanilla :: Bool
  , dksIsTang :: Bool
  , dksColors :: Colors
  , dksKeysStacked :: [String]
  , dksForMisDoublePress :: Bool
  , dksGlobalConfig :: [(String, GPop.Color)]
  , dksIsDeleting :: Bool
  }

data Doc = CDoc {
    dkPDFPath :: String
  , dkPDFDocName :: String
  , dkCurrDoc :: GPop.Document
  , dkConfig :: [(String, GPop.Color)]
  , dkConfigYank :: [(String, GPop.Color)]
  , dkCurrToken :: String
  , dkTogColIndex :: Int
  , dkCurrPage :: Int32
  , dkNextPage :: Int32
  , dkIsJapanese :: Bool
  , dkClipSq :: Sq Double
  , dkClipSqNext :: Sq Double
-- ppp  , dkTokSqTrues :: V.Vector (V.Vector (String, PopPRectangle))
  }


setColors :: IO Colors
setColors = do
    colorRed <- GPop.colorNew
    set colorRed [ #red := 65535 ]
    colorBlue <- GPop.colorNew
    set colorBlue [ #blue := 65535 ]
    colorGreen <- GPop.colorNew
    set colorGreen [#red := 39424, #green := 52480, #blue := 12800 ]
    -- set colorGreen [#red := 8704, #green := 35584, #blue := 8704 ]
    colorWhite <- GPop.colorNew
    set colorWhite [#red := 65535, #green := 65535, #blue := 65535 ]
    colorNavy <- GPop.colorNew
    set colorNavy [#red := 0, #green := 0, #blue := 32767]
    colorLime <- GPop.colorNew
    set colorLime [#red := 0, #green := 65535, #blue := 0 ]
    colorAqua <- GPop.colorNew
    set colorAqua [#red := 0, #green := 49087, #blue := 65535]
    colorYellow <- GPop.colorNew
    set colorYellow [#red := 65535, #green := 65535, #blue := 0 ]
    colorPink <- GPop.colorNew
    set colorPink [#red := 65535, #green := 0, #blue := 65535 ]
    colorOlive <- GPop.colorNew
    set colorOlive [#red := 32767, #green := 32767, #blue := 0 ]
    colorPurple <- GPop.colorNew
    set colorPurple [#red := 35446, #green := 11051, #blue := 58082 ]
    colorBrown <- GPop.colorNew
    set colorBrown [#red := 32767, #green := 0, #blue := 0 ]
    colorOrange <- GPop.colorNew
    set colorOrange [#red := 65535, #green := 35980, #blue := 0 ]
    colorGold <- GPop.colorNew
    set colorGold [#red := 65535, #green := 55255, #blue := 0 ]
    let
      colors = CColors {
          colRed = colorRed
        , colBlue = colorBlue
        , colGreen = colorGreen
        , colWhite = colorWhite
        , colNavy = colorNavy
        , colLime = colorLime
        , colAqua = colorAqua
        , colYellow = colorYellow
        , colPink = colorPink
        , colOlive = colorOlive
        , colPurple = colorPurple
        , colBrown = colorBrown
        , colOrange = colorOrange
        , colGold = colorGold
        }
    return colors

retrText page rect = do
  text1 <- GPop.pageGetSelectedText page GPop.SelectionStyleGlyph rect
  text2 <- GPop.pageGetSelectedText page GPop.SelectionStyleWord rect
  text3 <- GPop.pageGetSelectedText page GPop.SelectionStyleLine rect
  sRect <- showRect rect
  return (sRect, text1)


isSqIncludeSq :: Ord a => Num a => Sq a -> Sq a -> Bool
isSqIncludeSq sqs sqd = isInnerX && isInnerY
  where
    isInnerX = sqLeft sqs <= sqLeft sqd && sqRight sqd <= sqRight sqs
    isInnerY = sqTop sqs <= sqTop sqd && sqBot sqd <= sqBot sqs

retrTextFromPoint posTokens pos = satisfied
  where
    satisfied = filter (f pos) posTokens
      where
        f tPos posTok@(sq, tok) = isSqIncludePoint sq tPos

command = "http://localhost:9000/?annotators=parse&outputFormat=json"

{-
pageGetPosCharList :: GPop.Page -> IO [(Text.Text, [Sq Double])]
pageGetPosCharList page = do
  chars <- (map (\c -> Text.pack [c]) . Lis.nub . Lis.sort . head) <$> ((\c -> [c])  . Text.unpack) <$> GPop.pageGetText page
  res <- pageGetPosCharListPrim page chars
  return res

pageGetPosTokenList :: GPop.Page -> IO [(Sq Double, Text.Text)]
pageGetPosTokenList page = do
  tokens <- Text.words <$> GPop.pageGetText page
  res <- pageGetPosTokenListPrim page tokens
  return $ Lis.nub $ Lis.sort res

pageGetPosCharListPrim :: GPop.Page -> [Text.Text] -> IO [(Text.Text, [Sq Double])]
pageGetPosCharListPrim page chars = do
  let
    retrieveRect cStr = do
      let
        findOptions = [GPop.FindFlagsCaseSensitive]
      rectsPrim <- GPop.pageFindTextWithOptions page cStr findOptions
      rects <- sequence $ map (swapRectAns page) rectsPrim
      recrec <- sequence $ map showRect rects
      return (cStr, recrec)
  res <- mapM retrieveRect chars
  return res

pageGetPosCharListFlattened :: GPop.Page -> [Text.Text]-> IO [(Sq Double, Text.Text)]
pageGetPosCharListFlattened page chars = do
  res <- swapEtFlatten <$> pageGetPosCharListPrim page chars
  return $ Lis.nub res
pageGetPosTokenListPrim :: GPop.Page -> [Text.Text]-> IO [(Sq Double, Text.Text)]
pageGetPosTokenListPrim page tokens = do
  let
    retrieveRect cStr = do
      let
        findOptions = [GPop.FindFlagsCaseSensitive, GPop.FindFlagsWholeWordsOnly]
      rectsPrim <- GPop.pageFindTextWithOptions page cStr findOptions
      rects <- sequence $ map (swapRectAns page) rectsPrim
      recrec <- sequence $ map showRect rects
      return (cStr, recrec)
  res <- swapEtFlatten <$> mapM retrieveRect tokens
  return $ Lis.nub res
-}


initWidgets poppySPath = do
  vbox <- Gtk.boxNew Gtk.OrientationVertical 0
  window <- new Gtk.Window
            [ #decorated      := True
            , #resizable      := True
            , #appPaintable   := True
            , #icon           :=> GP.pixbufNewFromFile $ poppySPath ++ "/" ++ "poppyS.png"
            , #title          := "poppyS - per altera ad ipsa"
            , #defaultWidth   := fromIntegral initialSize
            , #defaultHeight  := fromIntegral initialSize
            ]
  Gtk.windowMaximize window
  screen <- window `get` #screen
  visual <- #getRgbaVisual screen
  #setVisual window visual

  movePallete <-Gtk.windowNew Gtk.WindowTypePopup
  _ <- set movePallete
            [ #decorated      := True
            , #resizable      := False
            , #defaultWidth   := 200
            , #defaultHeight  := 200
            , Gtk.containerBorderWidth := 0
            ]
  return (window, movePallete)


parseConfig colors str = (tok, confColor)
  where
    tok = takeWhile (\c -> not $ c == ',') str
    colorPrim = reverse $ takeWhile (\c -> not $ c == ',') $ reverse str
    confColor
      | colorPrim == "Red" = colRed colors
      | colorPrim == "Blue" = colBlue colors
      | colorPrim == "Green" = colGreen colors
      | colorPrim == "White" = colWhite colors
      | colorPrim == "Navy" = colNavy colors
      | colorPrim == "Lime" = colLime colors
      | colorPrim == "Aqua" = colAqua colors
      | colorPrim == "Yellow" = colYellow colors
      | colorPrim == "Pink" = colPink colors
      | colorPrim == "Olive" = colOlive colors
      | colorPrim == "Purple" = colPurple colors
      | colorPrim == "Brown" = colBrown colors
      | colorPrim == "Orange" = colOrange colors
      | colorPrim == "Gold" = colGold colors
      | otherwise = colBlue colors

decodeConfig colors y
  | y == (colRed colors) = "Red"
  | y == (colBlue colors) = "Blue"
  | y == (colGreen colors) = "Green"
  | y == (colWhite colors) = "White"
  | y == (colPurple colors) = "Purple"
  | y == (colOrange colors) = "Orange"
  | y == (colPink colors) = "Pink"
  | y == (colAqua colors) = "Aqua"
  | y == (colLime colors) = "Lime"
  | y == (colBrown colors) = "Brown"
  | y == (colNavy colors) = "Navy"
  | y == (colOlive colors) = "Olive"
  | y == (colYellow colors) = "Yellow"
  | y == (colGold colors) = "Gold"
  | otherwise = "Red"

globalConfigFilePath = "global_config.txt"
-- configFilesDir :: String
-- configFilesDir = "./configs"

initDocs :: String -> IO Docs
initDocs poppySPath = do
  configsPrim <- iVecFromFile $ poppySPath ++ "/" ++ globalConfigFilePath
  colors <- setColors
  let
    config = map (parseConfig colors) $ V.toList $ V.map (\x -> read x :: String) configsPrim
    res = CDocs {
        dksPoppySPath = poppySPath
      , dksOffSetDX = 8.0
      , dksOffSetDY = 8.0
      , dksOffSetNextTo = 16.0
      , dksDebug = Hint
      , dksIsVanilla = False
      , dksIsTang = False
      , dksColors = colors
      , dksKeysStacked = []
      , dksForMisDoublePress = False
      , dksGlobalConfig = config
      , dksIsDeleting = False
      }
  return res

-- ppp initDoc :: IORef Docs -> IO Doc
-- ppp initDoc docsRef = do
initDoc :: IORef Docs -> String -> IO Doc
initDoc docsRef fpath = do
  docs <- readIORef docsRef
  let
    poppySPath = dksPoppySPath docs
    configFilesDir = poppySPath ++ "/configs"
  files <- listDirectory configFilesDir
  let
    colors = dksColors docs
    configs = map (takeWhile (\c -> not $ c == '_')) $ filter (\x -> Lis.isSuffixOf ".txt" x) files
    currDocName = reverse $ takeWhile (\c -> not $ c == '/') $ tail $ dropWhile (\c -> not $ c == '.') $ reverse fpath
    isExistsConfig = elem currDocName configs
    configFilePath = configFilesDir ++ "/" ++ currDocName ++ "_config.txt"
  -- when (not isExistsConfig) $ oVecToFile (V.singleton $ show 10) configFilePath
  when (not isExistsConfig) $ oVecToFile (V.singleton $ show 0) configFilePath

  doc <- GPop.documentNewFromFile (Text.pack fpath) Nothing
  nOfPage <- GPop.documentGetNPages doc
  configsPrim <- iVecFromFile configFilePath
  let
    firstPageFromFile = read $ (tail . init ) $ V.head configsPrim :: Int32
    config = map (parseConfig colors) $ V.toList $ V.map (\x -> read x :: String) $ V.tail configsPrim
    firstPage = stopper id 0 (nOfPage - 2)  firstPageFromFile :: Int32
    isMonoPage = nOfPage == 1
    nextPage
     | isMonoPage = -1
     | otherwise = firstPage + 1
  page <- GPop.documentGetPage doc firstPage
  (wid, hei) <- GPop.pageGetSize page
  (widNext, heiNext) <- do
    if nextPage == -1
      then return (-1, -1)
      else do
        pageNext <- GPop.documentGetPage doc nextPage
        sizeN <- GPop.pageGetSize pageNext
        return sizeN
  let
    clipSq = CSq {sqLeft = 0, sqTop = 0, sqRight = wid, sqBot = hei}
    clipSqNext = CSq {sqLeft = 0, sqTop = 0, sqRight = widNext, sqBot = heiNext}
    res = CDoc {
      dkPDFPath = fpath
    , dkPDFDocName = currDocName
    , dkCurrDoc = doc
    , dkConfig = config
    , dkConfigYank = []
    , dkCurrToken = ""
    , dkCurrPage = firstPage
    , dkNextPage = nextPage -- -negative if not 2 pages
    , dkTogColIndex = 0
    , dkIsJapanese = False
    , dkClipSq = clipSq
    , dkClipSqNext = clipSqNext
    }
  return res


initMVars :: IORef Doc -> IO MVars
initMVars docRef = do
  doc <- readIORef docRef
  let
    currDoc = dkCurrDoc doc
    currPage = dkCurrPage doc

  page <- GPop.documentGetPage currDoc currPage
  let
    defaultLayout = [CSq{sqTop = 0.0, sqLeft = 0.0, sqBot = 1.0, sqRight = 1.0}]
  nOfPage <- GPop.documentGetNPages currDoc
  sexps  <- MV.replicate (fromIntegral nOfPage) Nothing
  layouts  <- MV.replicate (fromIntegral nOfPage) []
  histo  <- MV.replicate (fromIntegral nOfPage) Nothing
  let
    res = CMVars {
      mVarSExps = sexps
    , mVarWordRootHisto = histo
    , mVarLayouts = layouts
    , mVarDefaultLayout = defaultLayout
    , mVarDefaultLayoutMode = OneTotal
    }
  return res

stackStan window mvars docsRef docRef = do
  mvs <- readMVar mvars
  let
    mvSexps = mVarSExps mvs
    mvLayouts = mVarLayouts mvs
    mvLayoutDefo = mVarDefaultLayout mvs
    mvLayoutModeDefo = mVarDefaultLayoutMode mvs
  docs <- readIORef docsRef
  let
    -- docName = pdfs !! currDocId
  doc <- readIORef docRef
  let
    currDoc = dkCurrDoc doc
    currPage = fromIntegral $ dkCurrPage doc
  maxPage <- fromIntegral <$> GPop.documentGetNPages currDoc
  let
    forwardNums
      | currPage == maxPage - 1 = [0 .. (maxPage - 1)]
      | otherwise = [currPage .. (maxPage - 1)]
    backwardNums
      | currPage == 0 = reverse [0 .. (maxPage - 1)]
      | otherwise = reverse [0 .. (currPage - 1)]

  nextForward <- getNextNothing mvSexps currPage
  nextBackward <- getPrevNothing mvSexps currPage
  putStrLn $ show nextForward
  putStrLn $ show nextBackward
  putStrLn "processed"


  if nextForward == Nothing && nextBackward == Nothing
    then return ()
    else do
     let
      f2 n2 = case n2 of
        Just n -> do
          nP <- GPop.documentGetPage currDoc n
          -- aa <- getSExpsIONew pdfPath (fromIntegral n) mvLayoutDefo mvLayoutModeDefo
          -- aa <- getSExpsIO pdfPath (fromIntegral n)
          let
            -- ppp tokSqTruePrim = tokSqTrues V.! (fromIntegral n)
          aa <- getSExpsIOS nP
          -- aa <- getSExpsIOPoppy1 tokSqTruePrim nP
          return (Just aa)
        Nothing -> return Nothing
      f = case nextForward of
        Nothing -> case nextBackward of
          Nothing -> return mvSexps
          Just m -> do
            mP <- GPop.documentGetPage currDoc $ fromIntegral m
            -- newSExp <- getSExpsIONew pdfPath (fromIntegral m) mvLayoutDefo mvLayoutModeDefo
            -- newSExp <- getSExpsIO pdfPath (fromIntegral m)
            let
              -- ppp tokSqTruePrim = tokSqTrues V.! m
            -- newSExp <- getSExpsIOPoppy1 tokSqTruePrim mP
            newSExp <- getSExpsIOS mP
            MV.write mvSexps m (Just newSExp)
            return mvSexps
        Just m2 -> case nextBackward of
          Nothing -> do
            m2P <- GPop.documentGetPage currDoc $ fromIntegral m2
            -- newSExp <- getSExpsIONew pdfPath (fromIntegral m2) mvLayoutDefo mvLayoutModeDefo
            -- newSExp <- getSExpsIO pdfPath (fromIntegral m2)
            let
              -- ppp tokSqTruePrim = tokSqTrues V.! m2
            -- newSExp <- getSExpsIOPoppy1 tokSqTruePrim m2P
            newSExp <- getSExpsIOS m2P
            MV.write mvSexps m2 (Just newSExp)
            return mvSexps
          Just m -> do
            mP <- GPop.documentGetPage currDoc $ fromIntegral m
            --newSExp <- getSExpsIONew pdfPath (fromIntegral m) mvLayoutDefo mvLayoutModeDefo
            -- newSExp <- getSExpsIO pdfPath (fromIntegral m)
            let
              -- ppp tokSqTruePrim = tokSqTrues V.! m
            -- newSExp  <- getSExpsIOPoppy1 tokSqTruePrim mP
            newSExp  <- getSExpsIOS mP
            MV.write mvSexps m (Just newSExp)
            mP2 <- GPop.documentGetPage currDoc $ fromIntegral m2
            --newSExp2 <- getSExpsIONew pdfPath (fromIntegral m2) mvLayoutDefo mvLayoutModeDefo
            -- newSExp2 <- getSExpsIO pdfPath (fromIntegral m2)
            let
              -- ppp  tokSqTruePrim = tokSqTrues V.! m2
            -- newSExp2  <- getSExpsIOPoppy1 tokSqTruePrim mP2
            newSExp2  <- getSExpsIOS mP2
            MV.write mvSexps m2 (Just newSExp2)
            return mvSexps
     mvSexpsNeo <- f
     modifyMVar_ mvars (\mvrs -> return $ mvrs {mVarSExps = mvSexpsNeo})
     if nextForward == Just currPage
       then do
        Gtk.widgetQueueDraw window
        stackStan window mvars docsRef docRef
       else
        stackStan window mvars docsRef docRef
     --stackStan mvars docRef

getNextNothing :: MV.MVector RealWorld (Maybe a) -> Int -> IO (Maybe Int)
getNextNothing mv i = do
  hoge <- MV.read mv i
  let
    f :: IO (Maybe Int)
    f = case hoge of
      Nothing -> return (Just i)
      Just _ -> if (MV.length mv - 1) < (i + 1)
        then return Nothing
        else getNextNothing mv (i + 1)
  f

getPrevNothing :: MV.MVector RealWorld (Maybe a) -> Int -> IO (Maybe Int)
getPrevNothing mv i = do
  hoge <- MV.read mv i
  let
    f :: IO (Maybe Int)
    f = case hoge of
      Nothing -> return (Just i)
      Just _ -> if (i - 1) < 0
        then return Nothing
        else getPrevNothing mv (i - 1)
  f

goOtherPage window docRef inclF inclFNext = do
  doc <- readIORef docRef
  let
    currDoc = dkCurrDoc doc
  nOfPage <- GPop.documentGetNPages currDoc
  let
    currPagePrev = dkCurrPage doc
    currPage = inclF currPagePrev nOfPage
    nextPagePrev = dkNextPage doc
    nextPage = inclFNext nextPagePrev nOfPage
  modifyIORef docRef (\x -> x {dkCurrPage = currPage, dkNextPage = nextPage})
  Gtk.widgetQueueDraw window

resizeFromCurrPageSqs window docsRef docRef mVars = do
    doc <- readIORef docRef
    docs <- readIORef docsRef
    mvs <- readMVar mVars
    let
      currPage = dkCurrPage doc
      nextPage = dkNextPage doc
      currDoc = dkCurrDoc doc
      sexpss = mVarSExps mvs
      offsetDX = dksOffSetDX docs
      offsetDY = dksOffSetDY docs
      offsetNextTo = dksOffSetNextTo docs
    currSExps <- MV.read sexpss $ fromIntegral currPage
    nOfPage <- GPop.documentGetNPages currDoc
    page <- GPop.documentGetPage currDoc currPage
    pageNext <- GPop.documentGetPage currDoc nextPage
    hw@(width, height) <- Gtk.windowGetSize window
    (pWid', pHei') <- GPop.pageGetSize page
    (pWidNext', pHeiNext') <- GPop.pageGetSize pageNext
    let
      isNothing = currSExps == Nothing
      totalSquares = getTotalSquares currSExps (pHei', pWid')
      leftLinePrim = stopper (\x -> x - offsetDX) 0.0 pWid' $ sqLeft totalSquares
      topLinePrim = stopper (\x -> x - offsetDY) 0.0 pHei' $ sqTop totalSquares
      rightLinePrim = stopper (\x -> x + offsetDX + offsetNextTo) 0.0 pWid' $ sqRight totalSquares
      botLinePrim = stopper (\x -> x + offsetDY) 0.0 pHei' $ sqBot totalSquares
    nextSExps <- do
      let
        n = fromIntegral nextPage
      if n < 0 || (fromIntegral nOfPage) - 1 < n then return Nothing
          else do
            res <- MV.read sexpss n
            return res
    let
      nextSexps = case nextSExps of
        Just sexpsPrim -> sexpsPrim
        Nothing -> []
      isNothing2 = nextSExps == Nothing
      totalSquaresNext = getTotalSquares nextSExps (pHeiNext', pWidNext')
      leftLineNextPrim = stopper (\x -> x - offsetDX - offsetNextTo) 0.0 pWidNext' $ sqLeft totalSquaresNext
      topLineNextPrim = stopper (\x -> x - offsetDY) 0.0 pHeiNext' $ sqTop totalSquaresNext
      rightLineNextPrim = stopper (\x -> x + offsetDX) 0.0 pWidNext' $ sqRight totalSquaresNext
      botLineNextPrim = stopper (\x -> x + offsetDY) 0.0 pHeiNext' $ sqBot totalSquaresNext
      (sq, sqNext) = (sqCommon, sqCommon)
        where
          commonLeft
           | leftLineNextPrim < leftLinePrim = leftLineNextPrim
           | otherwise = leftLinePrim
          commonTop
           | topLineNextPrim < topLinePrim = topLineNextPrim
           | otherwise = topLinePrim
          commonRight
           | rightLineNextPrim < rightLinePrim = rightLinePrim
           | otherwise = rightLineNextPrim
          commonBot
           | botLineNextPrim < botLinePrim = botLinePrim
           | otherwise = botLineNextPrim
          sqCommon = CSq {sqTop = commonTop, sqLeft = commonLeft, sqRight = commonRight, sqBot = commonBot}
    modifyIORef docRef (\x -> x
                         {dkClipSq = sq
                        , dkClipSqNext = sqNext})
    Gtk.widgetQueueDraw window

stopper f minm maxm n
  | f n < minm = minm
  | maxm < f n = maxm
  | otherwise = f n

getColundRectangles sexps configs = electeds
  where
      detacheds = map (map g2) $ map (filter g) $ map takeSndL $ map forgetSExp $ concatMap ((filter (\y -> isBottomBy id y)) . (takeSpecTags (\x -> x == NP))) $  map (mapNode snd (\x -> (fst x, synSqs $ snd x))) sexps
        where
          g x = case x of
            Nothing -> False
            Just _ -> True
          g2 x = case x of
            Nothing -> ("", [])
            Just y -> y
      detachedAssigneds = Lis.nubBy g2 $ foldl g [] configs
         where
           g2 x y = snd x == snd y
           g y x@(stemX, color) = y ++ filtered
              where
                filtered = [(color, sqs) | stem@(stems, sqs) <- stemmeds, elem (toLowers stemX) stems]
           stemmeds = map (\xs -> (map (toLowers . stemEng) $ takeFstL xs, takeSndL xs)) detacheds
      electeds = map f3 $ concatMap (\x@(col, sqss) -> map (\x -> (col, x)) $ concat sqss) detachedAssigneds
        where
          f3 (color, sq) = do
            rect <- sqToRect sq
            return (color, rect)

getTotalSquares currSExps (pHei', pWid') = totalSquares
  where
      sexps = case currSExps of
        Just sexpsPrim -> sexpsPrim
        Nothing -> []
      squares = concat $ takeSndL $ map f $ takeSndL $ filter (\x -> not $ (snd x) == Nothing) $ concatMap forgetSExp sexps
        where
          f x = case x of
            Just x1 -> x1
            Nothing -> ("", [])
      totalSquares
        | squares == [] = CSq {sqTop = 0.0, sqLeft = 0.0, sqBot = pHei', sqRight = pWid'}
        | otherwise = CSq {sqTop = topFrons, sqLeft = leftFrons, sqBot = botFrons, sqRight = rightFrons}
        where
          topFrons = minimum $ map sqTop squares
          leftFrons = minimum $ map sqLeft squares
          botFrons = maximum $ map sqBot squares
          rightFrons = maximum $ map sqRight squares

