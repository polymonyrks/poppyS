{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (when, void)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Data.Text as Text
import qualified Data.List as Lis
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.Environment
import Lib (iVecFromFile, iVecFromFileJP, oVecToFile, oVecToFileJP, delimitAtWO2, delimitAtWO2By, vNub, takeFst, takeSnd, takeFstT, takeSndT, takeThdT, execShell, delimitAtW2, getDivideLine, countPrefix, countSuffix, indexingL, oFileJP)
import PopSExp
import ParserP (parse, pSExp)
import SExp
import System.Directory
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
import qualified GI.Gdk.Structs.Rectangle
import qualified GI.GdkPixbuf as GP
import qualified GI.Gtk as Gtk
import qualified GI.Poppler as GPop

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)

import GHC.IO.Encoding
import Control.Exception.Safe

import Codec.Text.IConv
import Codec.Binary.UTF8.String
import Data.Text.Encoding


hogeP = do
  doc <- GPop.documentNewFromFile (Text.pack "file:///home/polymony/poppyS/bug0.pdf") Nothing
  nmax <- GPop.documentGetNPages doc
  let
    showTxt n = do
      page <- GPop.documentGetPage doc n
      txt <- GPop.pageGetText page
      mRes <- getMecabed $ Text.unpack txt
      let
        toks = V.map mToken mRes
      cshowI toks
  mapM showTxt [0 .. nmax - 1]

pdfFilesDir :: String
pdfFilesDir = "./pdfs"

stemEng = Text.unpack . (stem English) . Text.pack
toLowers = map toLower
toUppers = map toUpper

initialSize :: Int
initialSize = 1024

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))

main :: IO ()
main = do
  --hogeP
  setLocaleEncoding utf8
  --cp932 <- mkTextEncoding "cp932"
  --setLocaleEncoding cp932
  let
    toSlush = (Text.unpack) . (Text.replace "\\" "/") . Text.pack
  homePath <- toSlush <$> getEnv "HOME"
  let
    poppySPath = homePath ++ "/poppyS"
  fpathPrim <- (toSlush . head) <$> getArgs
  cwd <- toSlush <$> getCurrentDirectory
  let
    fpath
     | isFullWindows = "file:///" ++ fpathPrim
     | isWindows = "file:///" ++ cwd ++ "/" ++ fpathPrim
     | isFullLinux = "file://" ++ fpathPrim
     | otherwise = "file://" ++ cwd ++ "/" ++ fpathPrim
       where
         isFullLinux = head fpathPrim == '/'
         isWindows = elem ':' $ takeWhile (\c -> not $ c == '/') homePath
         isFullWindows = elem ':' $ takeWhile (\c -> not $ c == '/') fpathPrim
  -- oVecToFile (V.fromList hogeCheck) "/home/polymony/rks.txt"
  mainGtk fpath poppySPath

{-
fpath = "file:///home/polymony/poppyS/bug0.pdf"
poppySPath = "/home/polymony/poppyS"
-}

mainGtk :: String -> String -> IO ()
mainGtk fpath poppySPath = do
  _ <- Gtk.init Nothing
  (window, movePallete) <- initWidgets poppySPath
  Gtk.windowMaximize window

  docs <- initDocs poppySPath window
  docsRef <- newIORef docs
  -- when git push, delete below
  {-
  modifyIORef docsRef (\x -> x {dksIsDualPage = False})

  get2 "gsettings set com.canonical.Unity.Launcher launcher-position Bottom"
  get2 "xrandr -o left"
  Gtk.windowUnmaximize window
  Gtk.windowResize window 1080 1600
-}
  gestureSwipe <- Gtk.gestureSwipeNew window

  doc <- initDoc docsRef fpath
  docRef <- newIORef doc
  Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef

  let
    currDoc = dkCurrDoc doc
    currPage = dkCurrPage doc
  mvarsPrim <- initMVars docRef
  mVars <- newMVar mvarsPrim
  zeroRect <- GPop.rectangleNew

  {-
  Gtk.afterGestureSwipeSwipe gestureSwipe $ \x -> \y -> do
    let
      isLeftSwiped = x < 0
      isSwiped = (10.0) ^ 2 < (x ^ 2 + y ^ 2)
      actionSwiped
       | isSwiped && isLeftSwiped = undefined
       | isSwiped                 = undefined
       | otherwise = return ()
    return ()
-}


  on window #keyPressEvent $ \event -> do
    name <- event `get` #keyval >>= Gdk.keyvalName
    nPage <- GPop.documentGetNPages currDoc
    let
      fff name = case name of
        Nothing -> return ()
        Just nm -> do
          modifyIORef docsRef (\x -> x {dksKeysStacked = (dksKeysStacked x) ++ [Text.unpack nm]})
      incl n nOfPage = mod (n + 2) nOfPage
      decl n nOfPage = mod (n - 2) nOfPage
      incl1 n nOfPage = mod (n + 1) nOfPage
      decl1 n nOfPage = mod (n - 1) nOfPage
      numChars = (map (\c -> [c]) ['a' .. 'z']) ++ (map show [1 .. 9])
      registeredKeysConfigIO = ["at", "0"] : concatMap (\c -> [["at", c], ["colon", "w", "at", c]]) numChars
      registeredKeys = [["Shift_L"], ["Shift_R"], ["J"], ["K"], ["L"], ["M"], ["j"], ["k"], ["Up"], ["Left"], ["Right"], ["p"], ["x"], ["d", "d"], ["Escape"], ["colon", "w", "Return"], ["g","g"], ["G"], ["space", "l", "t"], ["w"], ["v"], ["m"], ["n"], ["u"], ["r"], ["c", "c"]] ++ registeredKeysConfigIO
    fff name
    stKeys <- dksKeysStacked <$> readIORef docsRef
    let
      isSomethingMatched = or $ map (\keys -> Lis.isPrefixOf stKeys keys) registeredKeys
    Gtk.windowSetTitle window $ Text.pack $ ushow stKeys

    when (stKeys == ["Shift_L"] || stKeys == ["Shift_R"]) $ do
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()

    when (stKeys == ["c", "c"]) $ do
      nHint <- dksNofHint <$> readIORef docsRef
      modifyIORef docsRef (\x -> x {dksKeysStacked = [], dksNofHint = nHint + 1})
      Gtk.windowSetTitle window $ Text.pack $ "Cheated(Hint added): " ++  (show $ nHint + 1)
      Gtk.widgetQueueDraw window
      return ()

    when (stKeys == ["u"]) $ do
      configs <- dkConfig <$> readIORef docRef
      when (not $ configs == []) $ do
        configsForRedo <- dkConfigForRedo <$> readIORef docRef
        let
          isExistsConfig = 0 < (length configs)
          (nextForRedo, nextConfigs)
            | isExistsConfig = (head configs : configsForRedo, tail configs)
            | otherwise = (configsForRedo, configs)
        modifyIORef docRef (\x -> x {dkConfig = nextConfigs, dkConfigForRedo = nextForRedo})
        modifyIORef docsRef (\x -> x {dksKeysStacked = []})
        if isExistsConfig
          then Gtk.windowSetTitle window $ Text.pack $ "Popped: " ++ (fst $ head configs)
          else Gtk.windowSetTitle window $ Text.pack "Nothing Popped"
        Gtk.widgetQueueDraw window
      return ()

    when (stKeys == ["r"]) $ do
      configs <- dkConfig <$> readIORef docRef
      configsForRedo <- dkConfigForRedo <$> readIORef docRef
      when (not $ configsForRedo == []) $ do
        let
          isExistsConfigRedo = 0 < (length configsForRedo)
          (nextForRedo, nextConfigs)
            | isExistsConfigRedo = (tail configsForRedo, head configsForRedo : configs)
            | otherwise = (configsForRedo, configs)
        modifyIORef docRef (\x -> x {dkConfig = nextConfigs, dkConfigForRedo = nextForRedo})
        modifyIORef docsRef (\x -> x {dksKeysStacked = []})
        if isExistsConfigRedo
          then Gtk.windowSetTitle window $ Text.pack $ "PoppedBack: " ++ (fst $ head configsForRedo)
          else Gtk.windowSetTitle window $ Text.pack "Nothing PoppedBack"
        Gtk.widgetQueueDraw window
      return ()

    when (stKeys == ["v"]) $ do
      toVanillaMode docsRef docRef window
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["n"]) $ do
      mode <- dksDebug <$> readIORef docsRef
      let
        newMode
         | mode == Local = Hint
         | mode == Adhoc = Local
         | mode == Gramatica = Adhoc
         | mode == GramaticaPrim = Gramatica
         | mode == Primitive = GramaticaPrim
         | otherwise = Primitive
      modifyIORef docsRef (\x -> x {dksDebug = newMode})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      Gtk.widgetQueueDraw window
      return ()
    when (stKeys == ["m"]) $ do
      mode <- dksDebug <$> readIORef docsRef
      let
        newMode
         | mode == Hint = Local
         | mode == Local = Adhoc
         | mode == Adhoc = Gramatica
         | mode == Gramatica = GramaticaPrim
         | mode == GramaticaPrim = Primitive
         | otherwise = Hint
      modifyIORef docsRef (\x -> x {dksDebug = newMode})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      Gtk.widgetQueueDraw window
      return ()
    when (stKeys == ["Down"]) $ do
      doc <- readIORef docRef
      docs <- readIORef docsRef
      let
        currPage = dkCurrPage doc
        nextPage = dkNextPage doc
        currDoc = dkCurrDoc doc
      nOfPage <- GPop.documentGetNPages currDoc
      page <- GPop.documentGetPage currDoc currPage
      pageNext <- do
        if nOfPage == 1
          then return page
          else GPop.documentGetPage currDoc nextPage
      let
        isDefaultSize = dksIsDefaultSize docs
      if isDefaultSize
        then do
          resizeFromCurrPageSqs window docsRef docRef mVars
          Gtk.windowSetTitle window $ Text.pack "Cropped"
        else do
          (wid, hei) <- GPop.pageGetSize page
          (widNext, heiNext) <- GPop.pageGetSize pageNext
          let
            clipSq = CSq {sqLeft = 0, sqTop = 0, sqRight = wid, sqBot = hei}
            clipSqNext = CSq {sqLeft = 0, sqTop = 0, sqRight = widNext, sqBot = heiNext}
          modifyIORef docRef (\x -> x {dkClipSq = clipSq, dkClipSqNext = clipSqNext})
          Gtk.windowSetTitle window $ Text.pack "Dafaulted"
          Gtk.widgetQueueDraw window
      modifyIORef docsRef (\x -> x {dksKeysStacked = [], dksIsDefaultSize = not isDefaultSize})
      return ()
    when (stKeys == ["Up"]) $ do
      doc <- readIORef docRef
      let
        currDoc = dkCurrDoc doc
        currPage = dkCurrPage doc
      page <- GPop.documentGetPage currDoc currPage
      (pWid', pHei') <- GPop.pageGetSize page
      let
        clipSq = dkClipSq doc
        tp = sqTop clipSq
        bt = sqBot clipSq
        lf = sqLeft clipSq
        rg = sqRight clipSq
        rat = (bt - tp) / (rg - lf)
        pitchX = 3.0
        pitchY = pitchX * rat
        newClipSq
         | 0 < tpPrim && btPrim < pHei' && 0 < lfPrim && rgPrim < pWid' = newSqPrim
         | otherwise = clipSq
          where
            tpPrim = tp - pitchY
            btPrim = bt + pitchY
            lfPrim = lf - pitchX
            rgPrim = rg + pitchX
            newSqPrim = CSq
              { sqTop = tpPrim
              , sqBot = btPrim
              , sqLeft = lfPrim
              , sqRight = rgPrim
              }
      modifyIORef docRef (\x -> x {dkClipSq = newClipSq})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window $ Text.pack "Resized"
      Gtk.widgetQueueDraw window
      return ()
    when (stKeys == ["H"]) $ do
      setClipSq docsRef docRef DLeft
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window $ Text.pack "Resized H"
      Gtk.widgetQueueDraw window
      return ()
    when (stKeys == ["J"]) $ do
      setClipSq docsRef docRef DBot
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window $ Text.pack "Resized J"
      Gtk.widgetQueueDraw window
      return ()
    when (stKeys == ["K"]) $ do
      setClipSq docsRef docRef DTop
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window $ Text.pack "Resized K"
      Gtk.widgetQueueDraw window
      return ()
    when (stKeys == ["L"]) $ do
      setClipSq docsRef docRef DRight
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window $ Text.pack "Resized L"
      Gtk.widgetQueueDraw window
      return ()
-- setClipSq docsRef docRef dir
    when (stKeys == ["j"]) $ do
      docs <- readIORef docsRef
      let
        isDual = dksIsDualPage docs
        goFunc
         | isDual = incl
         | otherwise = incl1
      goOtherPage window docsRef docRef goFunc goFunc
      modifyIORef docRef (\x -> x {dkClickedSquare = (-1, [])})
      modifyIORef docsRef (\x -> x {dksKeysStacked = [], dksNofHint = 2})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      return ()
    when (stKeys == ["k"]) $ do
      docs <- readIORef docsRef
      let
        isDual = dksIsDualPage docs
        goFunc
         | isDual = decl
         | otherwise = decl1
      goOtherPage window docsRef docRef goFunc goFunc
      modifyIORef docRef (\x -> x {dkClickedSquare = (-1, [])})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      return ()
    when (stKeys == ["Left"]) $ do
      goOtherPage window docsRef docRef decl1 decl1
      modifyIORef docRef (\x -> x {dkClickedSquare = (-1, [])})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      return ()
    when (stKeys == ["Right"]) $ do
      goOtherPage window docsRef docRef incl1 incl1
      modifyIORef docRef (\x -> x {dkClickedSquare = (-1, [])})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      return ()
    when (stKeys == ["g", "g"]) $ do
      goOtherPage window docsRef docRef (\n -> \m -> 0) (\n -> \m -> 1)
      modifyIORef docRef (\x -> x {dkClickedSquare = (-1, [])})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      return ()
    when (stKeys == ["G"]) $ do
      goOtherPage window docsRef docRef (\n -> \m -> nPage - 1) (\n -> \m -> 0)
      modifyIORef docRef (\x -> x {dkClickedSquare = (-1, [])})
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      return ()
    when (stKeys == ["w"]) $ do
      docs <- readIORef docsRef
      hw@(wWid, wHei) <- Gtk.windowGetSize window
      let
        nextIsDual = not $ dksIsDualPage docs
      modifyIORef docsRef (\x -> x {dksIsDualPage = nextIsDual})
      if nextIsDual == True
        then do
          Gtk.windowMaximize window
          Gtk.windowSetTitle window $ Text.pack "Maximized"
        else do
          Gtk.windowUnmaximize window
          Gtk.windowSetTitle window $ Text.pack "UnMaximized"
          -- Gtk.windowResize window (floor $ (fromIntegral wHei) / 2.0) (floor $ (fromIntegral wWid) / 2.0)
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["p"]) $ do
      modifyIORef docRef (\x -> x {dkConfig = dkConfigYank x})
      Gtk.widgetQueueDraw window
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window $ Text.pack "Pasted"
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
    when (stKeys == ["d", "d"]) $ do
      deleteColors docsRef docRef window
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      return ()
    when (stKeys == ["colon", "w", "Return"]) $ do
      saveConfigs docsRef docRef
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      Gtk.windowSetTitle window "Config Saved."
      return ()
    let
      saveConfig c = do
        when (stKeys == ["colon", "w", "at", c]) $ do
          docs <- readIORef docsRef
          doc <- readIORef docRef
          let
            colors = dksColors docs
            conf = res
              where
                res = map (\x -> (fst x) ++ "," ++ (decodeConfig colors $ snd x)) $ dkConfig doc
            currPage = dkCurrPage doc
            poppySPath = dksPoppySPath docs
            configFilesDir = poppySPath ++ "/configsPreset"
            outout = V.fromList $ (ushow currPage) : conf
            configFilePath = configFilesDir ++ "/" ++ c ++ "_config.txt"
            configFilePath0 = configFilesDir ++ "/0_config.txt"
          outoutPrevPrim <- iVecFromFile configFilePath
          let
            outoutPrev = V.map (\x -> read x :: String) outoutPrevPrim
          oVecToFileJP outoutPrev configFilePath0
          oVecToFileJP outout configFilePath
          modifyIORef docsRef (\x -> x {dksKeysStacked = []})
          Gtk.windowSetTitle window $ Text.pack $ "Config Saved to #Slot " ++ c
          return ()
      loadConfig c = do
        when (stKeys == ["at", c]) $ do
          docs <- readIORef docsRef
          doc <- readIORef docRef
          let
            colors = dksColors docs
            poppySPath = dksPoppySPath docs
            configFilesDir = poppySPath ++ "/configsPreset"
            configFilePath = configFilesDir ++ "/" ++ c ++ "_config.txt"
          configsPrim <- iVecFromFile configFilePath
          let
            config = map (parseConfig colors) $ V.toList $ V.map (\x -> read x :: String) $ V.tail configsPrim
          modifyIORef docRef (\x -> x {dkConfig = config})
          modifyIORef docsRef (\x -> x {dksKeysStacked = []})
          Gtk.widgetQueueDraw window
          Gtk.windowSetTitle window $ Text.pack $ "Config loaded from #Slot " ++ c
          return ()
    mapM loadConfig numChars
    mapM saveConfig numChars
    when (stKeys == ["space", "l", "t"]) $ do
      modifyIORef docRef (\x -> x {dkIsJapanese = not $ dkIsJapanese x})
      Gtk.widgetQueueDraw window
      modifyIORef docsRef (\x -> x {dksKeysStacked = []})
      doc <- readIORef docRef
      mvs <- readMVar mVars
      let
        cdoc = dkCurrDoc doc
        mvSexps = mVarSExps mvs
      nOfPage <- GPop.documentGetNPages cdoc
      mapM (\m -> MV.write mvSexps m Nothing) [0 .. (fromIntegral nOfPage) - 1]
      modifyMVar_ mVars (\mvrs -> return $ mvrs {mVarSExps = mvSexps})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
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
          nextIsDual = dksIsDualPage docs
          globalConf = dksGlobalConfig docs
--          baseConf = dksBaseConfig docs
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
          isJapanese = dkIsJapanese doc
        page <- GPop.documentGetPage currDoc currPage
        nOfPage <- GPop.documentGetNPages currDoc
        (pWid', pHei') <- GPop.pageGetSize page
        hw@(wWid, wHei) <- Gtk.windowGetSize window
        let
          ratio = (fromIntegral wHei) / (pageBot - pageTop)
          sexpsMV = mVarSExps mvs
        colReal <- get event #x
        rowReal <- get event #y
        button <- get event #button -- left click is 1, right click is 3
        sexpsMaybe <- MV.read sexpsMV (fromIntegral currPage)
        sexpsMaybeNext <- case nOfPage of
          1 -> return Nothing
          _ -> MV.read sexpsMV (fromIntegral nextPage)
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
          (stemmed, stemmedNext)
           | isJapanese = (detacheds, detachedsNext)
           | otherwise = (stemmedPrim, stemmedNextPrim)
            where
              stemmedPrim = map (\x -> (stemEng $ fst x, snd x)) detacheds
              stemmedNextPrim = map (\x -> (stemEng $ fst x, snd x)) detachedsNext
          wordSq = filter f stemmed
            where
              f x@(str, sqs) = or $ map (\sq -> isSqIncludePoint sq point) sqs
          wordSqNext = filter f stemmedNext
            where
              f x@(str, sqs) = or $ map (\sq -> isSqIncludePoint sq pointNext) sqs
          words =  wordSq
          wordsNext = wordSqNext

          colors = dksColors docs
          isFail = words == []
          isFailNext = wordsNext == []
          iword@(isLeft, (word, tarSqs))
            | isFail && isFailNext = (-1, ("", []))
            | not isFail = (0, head words)
            | otherwise =  (1, head wordsNext)
          previousDkClickedSquares = dkClickedSquare doc
          isSameClicked = previousDkClickedSquares == (isLeft, tarSqs)
        modifyIORef docRef (\dk -> dk {dkClickedSquare = (isLeft, tarSqs)})

        let
          isLeftSideClicked = isHalfLeft -- used when tablet mode (which wise color toggles)
            where
              point0
                | isLeft == 0 = point
                | otherwise = pointNext
              totalDist = sum $ map (\x -> (sqRight x) - (sqLeft x)) tarSqs
              res@(isIncFound, resDist, _) = foldl f (False, 0.0, 0.0) tarSqs
                where
                  f y@(isRes, iterRes, iter) sq
                    | isRes == True = y
                    | isIncluded = (True, currDist, nextDist)
                    | otherwise = (False, iterRes, nextDist)
                    where
                      nextDist = iter + (sqRight sq) - (sqLeft sq)
                      isIncluded = isSqIncludePoint sq point0
                      distXWhenInc = (posX point0) - (sqLeft sq)
                      currDist = iter + distXWhenInc
              isHalfLeft
                | isIncFound = resDist / totalDist < 0.3
                | otherwise  = False

        mode <- dksDebug <$> readIORef docsRef
        let
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
          forWinTitle = (ushow word)
          isTablet = dksIsTablet docs
          colorIndex = dkTogColIndex doc
          (newInd, newColor)
            | isTablet = (newIndex, colorTablet)
            | mode == Local = (newIndex, colorLocal)
            | isAlreadyL && button == 1 = (colorIndex, togging currCol)
            | isAlreadyL = (colorIndex, toggingRev currCol)
            | isAlreadyG = (colorIndex, currColG)
            | isAlreadyLSimilar = (colorIndex, currColSimilarL)
            | isAlreadyGSimilar = (colorIndex, currColSimilarG)
            | otherwise = (newIndex, color)
             where
              isAlreadyL = not $ alrdLocalConfigs == []
              alrdLocalConfigs  = filter (\x -> (fst x) == word) currConfig
              currCol = snd $ head alrdLocalConfigs

              isAlreadyG = not $ alrdGlobalConfigs == []
              alrdGlobalConfigs = filter (\x -> (fst x) == word) globalConf
              currColG = snd $ head alrdGlobalConfigs

              isAlreadyLSimilar = not $ alrdLocalConfigsSimilar == []
              alrdLocalConfigsSimilar  = getAlrdConfigsSimilar currConfig
              currColSimilarL = snd $ head alrdLocalConfigsSimilar

              isAlreadyGSimilar = not $ alrdGlobalConfigsSimilar == []
              alrdGlobalConfigsSimilar = getAlrdConfigsSimilar globalConf
              currColSimilarG = snd $ head alrdGlobalConfigsSimilar


              newIndexTemp
                | button == 1 = (colorIndex + 1)
                | otherwise = (colorIndex - 1)
              newIndexTempTab
                | isSameClicked && isLeftSideClicked = (colorIndex - 1)
                | otherwise = colorIndex + 1
              color
                | mod newIndexTemp 8 == 0 = colLime colors
                | mod newIndexTemp 8 == 1 = colRed colors
                | mod newIndexTemp 8 == 2 = colBlue colors
                | mod newIndexTemp 8 == 3 = colGreen colors
                | mod newIndexTemp 8 == 4 = colPurple colors
                | mod newIndexTemp 8 == 5 = colOrange colors
                | mod newIndexTemp 8 == 6 = colPink colors
                | otherwise = colAqua colors
              colorLocal
                | mod newIndexTemp 8 == 0 = colBlue colors
                | mod newIndexTemp 8 == 1 = colPink colors
                | mod newIndexTemp 8 == 2 = colAqua colors
                | mod newIndexTemp 8 == 3 = colLime colors
                | mod newIndexTemp 8 == 4 = colOrange colors
                | mod newIndexTemp 8 == 5 = colPurple colors
                | mod newIndexTemp 8 == 6 = colGreen colors
                | otherwise = colRed colors
              colorTablet
                | mod newIndexTempTab 9 == 0 = colBlack colors
                | mod newIndexTempTab 9 == 1 = colPink colors
                | mod newIndexTempTab 9 == 2 = colAqua colors
                | mod newIndexTempTab 9 == 3 = colLime colors
                | mod newIndexTempTab 9 == 4 = colOrange colors
                | mod newIndexTempTab 9 == 5 = colPurple colors
                | mod newIndexTempTab 9 == 6 = colGreen colors
                | mod newIndexTempTab 9 == 7 = colRed colors
                | otherwise = colBlue colors
              newIndex
                | isTablet = mod newIndexTempTab 9
                | otherwise = mod newIndexTemp 8
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
          isBothFail = (isFail && isFailNext)
          newIndex
            | isTablet && isBothFail = colorIndex
            | isTablet = mod newInd 9
            | otherwise = mod newInd 8
          incl n nOfPage = mod (n + 2) nOfPage
          decl n nOfPage = mod (n - 2) nOfPage
          incl1 n nOfPage = mod (n + 1) nOfPage
          decl1 n nOfPage = mod (n - 1) nOfPage

          actionClicked
           -- | isTablet && isClickedOuterXLeft && (not isClickedUpperRow) && (not nextIsDual) = actionBothFailLeftNoNextDual
           | isTablet && isClickedOuterXLeft && (isClickedUpperRow) && (not nextIsDual) = actionBothFailLeftNoNextDual
           | isTablet && isClickedOuterXLeft && (isClickedUpperRow) && nextIsDual = actionBothFailRightNoNextDual
           | isTablet && isClickedOuterXLeft && (not isClickedUpperRow) && nextIsDual = actionBothFailRightNextDual
           | isTablet && isClickedOuterXLeft                 = actionBothFailRightNoNextDual
           | isTablet && isClickedOuterYTop && isClickedLeftCol = toVanillaMode docsRef docRef window
           | isTablet && isClickedOuterYTop     = deleteColors docsRef docRef window
           | isTablet && isClickedOuterXRightNext && nextIsDual  && (isClickedUpperRow)        = actionBothFailLeftNoNextDual
           | isTablet && isClickedOuterXRightNext && nextIsDual  && (not isClickedUpperRow)        = actionBothFailLeftNextDual
           | isTablet && isClickedOuterXRight     && (not nextIsDual)   = actionBothFailLeftNoNextDual
           | isTablet && (not isDeleting)       = actionNotBothFailNoDeleting
           | isTablet                           = actionNotBothFailDeleting

           | not isBothFail && (not isDeleting) = actionNotBothFailNoDeleting
           | not isBothFail                     = actionNotBothFailDeleting
           | isLeftClicked && nextIsDual        = actionBothFailLeftNextDual
           | isLeftClicked                      = actionBothFailLeftNoNextDual
           | nextIsDual                         = actionBothFailRightNextDual
           | otherwise                          = actionBothFailRightNoNextDual
           where
             -- isBothFail = (isFail && isFailNext)
             isLeftClicked = button == 1
             actionBothFailLeftNextDual = do
                 goOtherPage window docsRef docRef incl incl
                 Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
             actionBothFailLeftNoNextDual = do
                 goOtherPage window docsRef docRef incl1 incl1
                 Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
             actionBothFailRightNextDual = do
                 goOtherPage window docsRef docRef decl decl
                 Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
             actionBothFailRightNoNextDual = do
                 goOtherPage window docsRef docRef decl1 decl1
                 Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
             actionNotBothFailNoDeleting = do
              let
                 dropped = filter (\x -> not $ (fst x) == word) $ dkConfig doc
                 droppedG = filter (\x -> not $ (fst x) == word) $ dksGlobalConfig docs
              modifyIORef docRef (\doc -> doc {dkCurrToken = word, dkConfig = [(word, newColor)] ++ dropped, dkTogColIndex = newIndex})
              modifyIORef docsRef (\docs -> docs {dksGlobalConfig = [(word, newColor)] ++ droppedG})
              Gtk.widgetQueueDraw window
              Gtk.windowSetTitle window $ Text.pack forWinTitle
             actionNotBothFailDeleting = do
               let
                 dropped = filter (\x -> not $ (fst x) == word) $ dkConfig doc
               modifyIORef docRef (\doc -> doc {dkCurrToken = word, dkConfig = dropped, dkTogColIndex = newIndex})

               Gtk.widgetQueueDraw window
               Gtk.windowSetTitle window $ Text.pack forWinTitle
             squares = concatMap id $ takeSndL detacheds
             squaresNext = concatMap id $ takeSndL detachedsNext
             isSquaresVoid = squares == []
             isSquaresNextVoid = squaresNext == []
             rowCenter
               -- | nextIsDual = 0.5 * (leftPageCenter + rightPageCenter)
               | otherwise = leftPageCenter
               where
                 leftPageCenter = 0.5 * (pageTop + pageBot)
                 rightPageCenter = 0.5 * (pageTopNext + pageBotNext)
             colCenter
               | nextIsDual = bothPageColCenter
               | otherwise = leftPageColCenter
               where
                 leftPageColCenter = 0.5 * (pageLeft + pageRight)
                 bothPageColCenter = pageRight
             mostLeft
               | (isSquaresVoid || isSquaresNextVoid) = colCenter
               | otherwise = minimum $ map sqLeft squares
             mostRight
               | (isSquaresVoid || isSquaresNextVoid)  = colCenter
               | nextIsDual = maximum $ map sqRight squaresNext
               | otherwise = maximum $ map sqRight squares
             mostTopPrim
               | isSquaresVoid || isSquaresNextVoid  = rowCenter
               | nextIsDual = minimum $ map sqTop $ squares ++ squaresNext
               | otherwise = minimum $ map sqTop squares
             mostTop
               | mostTopUB < mostTopPrim = mostTopUB
               | otherwise = mostTopPrim
               where
                 mostTopUB = pageTop + (0.2 * (pageBot - pageTop))
             mostBot
               | isSquaresVoid || isSquaresNextVoid  = rowCenter
               | nextIsDual = maximum $ map sqBot $ squares ++ squaresNext
               | otherwise = maximum $ map sqBot squares
             clickedX = posX point
             clickedY = posY point
             clickedXNext = posX pointNext
             clickedYNext = posY pointNext
             -- isClickedInner = mostTop < clickedY && clickedY < mostBot && mostLeft < clickedX && clickedX < mostRight
             isClickedOuterYTop = clickedY < mostTop
             isClickedLeftCol = clickedX < colCenter
             isClickedUpperRow = clickedY < rowCenter
             isClickedOuterXLeft = clickedX < mostLeft
             isClickedOuterXRight = mostRight < clickedX
             isClickedLeftColNext = clickedXNext < colCenter
             isClickedOuterXRightNext = mostRight < clickedXNext
        actionClicked
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
      isYondle = dksIsYondle docs
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
      isJapanese = dkIsJapanese doc
    page <- GPop.documentGetPage currDoc currPage
    nOfPage <- GPop.documentGetNPages currDoc
    pageNext <- do
      if nOfPage == 1
        then return page
        else GPop.documentGetPage currDoc currPage
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
          Just sexps -> map g2 $ filter g $ takeSndL $ concatMap forgetSExp $ map (mapNode snd (\x -> (fst x, synSqs $ snd x))) $ concatMap (takeSpecTags (\tg -> (snd tg) == NP || (snd tg) == ADJP || (snd tg) == VBN || (snd tg) == VBG)) sexps
            where
              g x = case x of
                Nothing -> False
                Just _ -> True
              g2 x = case x of
                Nothing -> ("", [])
                Just y -> y
      forCheck = case sexpsMaybe of
        Nothing -> []
        Just sexps -> sexps
    let
      detachedsNext =
        case sexpsMaybeNext of
          Nothing -> []
          Just sexps -> map g2 $ filter g $ takeSndL $ concatMap forgetSExp $ map (mapNode snd (\x -> (fst x, synSqs $ snd x))) $ concatMap (takeSpecTags (\tg -> (snd tg) == NP || (snd tg) == ADJP || (snd tg) == VBN || (snd tg) == VBG)) sexps
            where
              g x = case x of
                Nothing -> False
                Just _ -> True
              g2 x = case x of
                Nothing -> ("", [])
                Just y -> y
      ngStems = ["that", "this", "them", "their", "these", "those", "from"]
      ngStemsJP = ["あ", "い", "う", "え", "お", "か", "き", "く", "け", "こ", "さ", "し", "す", "せ", "そ", "た", "ち", "つ", "て", "と", "な", "に", "ぬ", "ね", "の", "は", "ひ", "ふ", "へ", "ほ", "ま", "み", "む", "め", "も", "や", "ゆ", "よ", "わ", "を", "ん", "ため", "よう", "この", "あの", "こと", "的"]
      (stemmed, stemmedNext)
       | isJapanese = (stemmedPrimJP, stemmedNextPrimJP)
       | otherwise = (stemmedPrim, stemmedNextPrim)
         where
            stemmedPrim = filter (\x -> (3 < (length $ fst x)) && (not $ elem (fst x) ngStems)) $ map (\x -> (stemEng $ fst x, snd x)) detacheds
            stemmedNextPrim = filter (\x -> (3 < (length $ fst x)) && (not $ elem (fst x) ngStems)) $  map (\x -> (stemEng $ fst x, snd x)) detachedsNext
            stemmedPrimJP = filter (\x -> (not $ elem (fst x) ngStemsJP)) detacheds
            stemmedNextPrimJP = filter (\x -> (not $ elem (fst x) ngStemsJP)) detachedsNext
      clickedSq@(isLeft, sqssqs) = dkClickedSquare doc
    let
      mode = dksDebug docs
      specials = takeFstL $ filter (\x -> not $ elem (fst x) $ alreadies) $ take nOfSpecialsTaken $ reverse $ Lis.sortBy f $ V.toList $ getHistogram $ V.fromList $ takeFstL applicant
         where
           nOfSpecialsTaken = dksNofHint docs
            -- | mode == Local = 4
            -- | mode == Local = dksNofHint docs
            -- | otherwise =  dksNofHint docs
           f x y = compare (snd x) (snd y)
           applicant
            | (isLeft == -1) = stemmed ++ stemmedNext
            | (isLeft == 0) = (dropWhile (\x@(_, sqs) -> not $ sqs == sqssqs) stemmed) ++ stemmedNext
            | (isLeft == 1) = dropWhile (\x@(_, sqs) -> not $ sqs == sqssqs) stemmedNext
            | otherwise = error "unexpected condition (leftPage clicked or right one or nothing)"
      specialsPrius = concatMap g specials
         where
           stemmedOrderedTagged
            | (isLeft == -1) = stemmedNormal ++ stemmedNormalNext
            | (isLeft == 0) = stemmedSelected ++ stemmedNormalNext
            | (isLeft == 1) = stemmedSelectedNext
            | otherwise = error "unexpected condition (leftPage clicked or right one or nothing)"
               where
                 stemmedSelected = map (\x -> (0, x)) $ dropWhile (\x@(_, sqs) -> not $ sqs == sqssqs) stemmed
                 stemmedSelectedNext = map (\x -> (1, x)) $ dropWhile (\x@(_, sqs) -> not $ sqs == sqssqs) stemmedNext
                 --stemmedSelected = map (\x -> (0, x)) stemmed
                 --stemmedSelectedNext = map (\x -> (1, x)) stemmedNext
                 stemmedNormal = (map (\x -> (0, x)) stemmed)
                 stemmedNormalNext = (map (\x -> (1, x)) stemmedNext)
           g tok
             | filtered == [] = []
             | otherwise = [head filtered]
             where
               filtered = filter (\y -> tok == (fst $ snd y)) stemmedOrderedTagged
      rectsSpecial@(special0, special1) = (res0, res1)
        where
          concated = map (\x@(i, (tk, sq)) -> (i, sq)) $ specialsPrius
          concated0 = concat $ takeSndL $ filter (\x -> fst x == 0) concated
          concated1 = concat $ takeSndL $ filter (\x -> fst x == 1) concated
          gold = colGold colors
          res0 = map (\x -> (\y -> (gold, y)) <$> sqToRect x) concated0
          res1 = map (\x -> (\y -> (gold, y)) <$> sqToRect x) concated1
      sexps = case sexpsMaybe of
        Just sexpsPrim -> sexpsPrim
        Nothing -> []
      nextSexps = case sexpsMaybeNext of
        Just sexpsPrim -> sexpsPrim
        Nothing -> []
      electeds = getColundRectangles sexps configs isJapanese colors mode
      electedsNext = getColundRectangles nextSexps configs isJapanese colors mode
    rects <- sequence electeds
    rectsNext <- sequence electedsNext
    rectsSpec <- sequence special0
    rectsSpecNext <- sequence special1
    let
      colundRects
       | mode == Vanilla = []
       | mode == Primitive || mode == Adhoc = rects
       | otherwise = rectsSpec ++ rects
      colundRectsNext
       | mode == Vanilla = []
       | mode == Primitive || mode == Adhoc = rectsNext
       | otherwise = rectsSpecNext ++ rectsNext
    page <- GPop.documentGetPage currDoc currPage
    hw@(width, height) <- Gtk.windowGetSize window
    (pWid', pHei') <- GPop.pageGetSize page

    if isYondle
     then do
      forkIO $ do
       let
        -- strs = takeFstL detacheds
        strs = ["hoge", "fuga", "boke", "without"]
        renderString str = do
          renderWithContext context $ do
            save
            setOperator OperatorSource
            setSourceRGBA 255 255 255 1
            paint
            restore
          renderWithContext context $ do
            save
            scale ratio ratio
            -- translate (- pageLeft) (- pageTop)
            translate (200) (300)
            showText (str :: String)
            restore
          -- threadDelay (2 * 1000 * 1000)
       sequence $ map renderString strs
       return ()
      return True
     else do
      renderWithContext context $ do
        save
        setOperator OperatorSource
        setSourceRGBA 255 255 255 1
        -- setSourceRGBA 0 0 0 1
        paint
        restore
      renderWithContext context $ do
        save
        scale ratio ratio
        translate (- pageLeft) (- pageTop)
        -- zeroRect <- GPop.rectangleNew
        GPop.pageRender page context
        _ <- mapM (\x@(col, rect) -> GPop.pageRenderSelection page context rect zeroRect GPop.SelectionStyleGlyph col $ colWhite colors) colundRects
        restore

      -- for NextPage(If exists)
      if (0 < nextPage) && (nextPage < (fromIntegral nOfPage) - 1) && (dksIsDualPage docs)then
        renderWithContext context $ do
            pageNext <- GPop.documentGetPage currDoc nextPage
            save
            scale ratioNext ratioNext
            translate (pWid' - pageLeftNext - (pWid' - pageRight) - pageLeft) (- pageTopNext)
            -- zeroRect <- GPop.rectangleNew
            GPop.pageRender pageNext context
            _ <- mapM (\x@(col, rect) -> GPop.pageRenderSelection pageNext context rect zeroRect GPop.SelectionStyleGlyph col $ colWhite colors) colundRectsNext
            --_ <- mapM (\x@(col, rect) -> GPop.pageRenderSelection pageNext context rect zeroRect GPop.SelectionStyleGlyph col $ colSemiWhite colors) colundRectsNext
            restore
        else return ()
      return True
  on window #destroy $ do
    {-
    get2 "xrandr -o normal"
    get2 "gsettings set com.canonical.Unity.Launcher launcher-position Left"
-}
    Gtk.mainQuit

  #showAll window
  Gtk.main

data Colors = CColors
  { colRed :: GPop.Color
  , colBlue :: GPop.Color
  , colGreen :: GPop.Color
  , colWhite :: GPop.Color
  , colSemiWhite :: GPop.Color
  , colBlack :: GPop.Color
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
  }

data Mode = Hint | Gramatica | Vanilla | Primitive | Adhoc | Local | GramaticaPrim
  deriving (Show, Eq)

data Docs = CDocs {
    dksPoppySPath :: String
  , dksOffSetDX :: Double
  , dksOffSetDY :: Double
  , dksOffSetNextTo :: Double
  , dksDebug :: Mode
  , dksNofHint :: Int
  , dksIsVanilla :: Bool
  , dksIsTang :: Bool
  , dksColors :: Colors
  , dksKeysStacked :: [String]
  , dksForMisDoublePress :: Bool
  , dksGlobalConfig :: [(String, GPop.Color)]
  , dksBaseConfig :: [(String, GPop.Color)]
  , dksIsDeleting :: Bool
  , dksIsDualPage :: Bool
  , dksPresetConfigs :: [[([Char], GPop.Color)]]
  , dksIsYondle :: Bool
  , dksIsTablet :: Bool
  , dksIsDefaultSize :: Bool
  }

data Doc = CDoc {
    dkPDFPath :: String
  , dkPDFDocName :: String
  , dkCurrDoc :: GPop.Document
  , dkConfig :: [(String, GPop.Color)]
  , dkConfigYank :: [(String, GPop.Color)]
  , dkConfigForRedo :: [(String, GPop.Color)]
  , dkCurrToken :: String
  , dkTogColIndex :: Int
  , dkCurrPage :: Int32
  , dkNextPage :: Int32
  , dkIsJapanese :: Bool
  , dkClipSq :: Sq Double
  , dkClipSqNext :: Sq Double
  , dkClickedSquare :: (Int, [Sq Double]) -- fst is which of these, i.e. (-1: no clicked, 0: leftPage, 1:rightPage)
  }

setColors :: IO Colors
setColors = do
    colorRed <- GPop.colorNew
    set colorRed [ #red := 65535 ]
    colorBlue <- GPop.colorNew
    set colorBlue [ #blue := 65535 ]
    colorGreen <- GPop.colorNew
    set colorGreen [#red := 0, #green := 32767, #blue := 0]
    colorWhite <- GPop.colorNew
    set colorWhite [#red := 65535, #green := 65535, #blue := 65535 ]
    colorSemiWhite <- GPop.colorNew
    set colorSemiWhite [#red := 64250, #green := 64250, #blue := 64250 ]
    colorNavy <- GPop.colorNew
    set colorNavy [#red := 0, #green := 0, #blue := 32767]
    colorLime <- GPop.colorNew
    --set colorLime [#red := 0, #green := 65535, #blue := 0 ]
    set colorLime [#red := 0, #green := 60000, #blue := 0 ]
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
    colorBlack <- GPop.colorNew
    set colorBlack [#red := 0, #green := 0, #blue := 0 ]
    let
      colors = CColors {
          colRed = colorRed
        , colBlue = colorBlue
        , colGreen = colorGreen
        , colWhite = colorWhite
        , colSemiWhite = colorSemiWhite
        , colBlack = colorBlack
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

setColorsACM :: IO Colors
setColorsACM = do
    colorRed <- GPop.colorNew
    set colorRed [ #red := 54998,  #green := 10023,  #blue := 10280]
    colorBlue <- GPop.colorNew
    set colorBlue [ #red := 7967,  #green := 30583,  #blue := 46260]
    colorGreen <- GPop.colorNew
    set colorGreen [#red := 11308,  #green := 41120,  #blue := 11308]
    colorWhite <- GPop.colorNew
    set colorWhite [#red := 65535, #green := 65535, #blue := 65535 ]
    colorSemiWhite <- GPop.colorNew
    set colorSemiWhite [#red := 64250, #green := 64250, #blue := 64250 ]
    colorNavy <- GPop.colorNew
    set colorNavy [#red := 0, #green := 0, #blue := 32767]
    colorLime <- GPop.colorNew
    set colorLime [#red := 0, #green := 60000, #blue := 0 ]
    -- set colorLime [#red := 26985,  #green := 48059,  #blue := 26985]
    colorAqua <- GPop.colorNew
    set colorAqua [#red := 18504,  #green := 52171,  #blue := 55512]
    colorYellow <- GPop.colorNew
    set colorYellow [#red := 65535, #green := 65535, #blue := 0 ]
    colorPink <- GPop.colorNew
    set colorPink [#red := 58339,  #green := 30583,  #blue := 49858]
    colorOlive <- GPop.colorNew
    set colorOlive [#red := 32639,  #green := 32639,  #blue := 32639]
    colorPurple <- GPop.colorNew
    set colorPurple [#red := 38036,  #green := 26471,  #blue := 48573]
    colorBrown <- GPop.colorNew
    set colorBrown [#red := 35980,  #green := 22102,  #blue := 19275]
    colorOrange <- GPop.colorNew
    set colorOrange [#red := 65535,  #green := 32639,  #blue := 3598]
    colorGold <- GPop.colorNew
    set colorGold [#red := 48316,  #green := 48573,  #blue := 8995]
    colorBlack <- GPop.colorNew
    set colorBlack [#red := 0, #green := 0, #blue := 0 ]
    let
      colors = CColors {
          colRed = colorRed
        , colBlue = colorBlue
        , colGreen = colorGreen
        , colWhite = colorWhite
        , colSemiWhite = colorSemiWhite
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
        , colBlack = colorBlack
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
      | colorPrim == "SemiWhite" = colSemiWhite colors
      | colorPrim == "Black" = colBlack colors
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
  | y == (colSemiWhite colors) = "SemiWhite"
  | y == (colBlack colors) = "Black"
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
baseConfigFilePath = "base_config.txt"
presetConfigDirr = "configsPreset"
presetConfigFileNames = (map (\c -> [c] ++ "_config.txt") ['a' .. 'z']) ++ (map (\n -> (show n) ++ "_config.txt") [0 .. 9])

initDocs :: String -> Gtk.Window -> IO Docs
initDocs poppySPath window = do
  let
    fullPresetDirr = poppySPath ++ "/" ++ presetConfigDirr
  createDirectoryIfMissing False fullPresetDirr
  configsPrim <- iVecFromFile $ poppySPath ++ "/" ++ globalConfigFilePath
  configsBasePrim <- iVecFromFile $ poppySPath ++ "/" ++ baseConfigFilePath
  presetConfs <- listDirectory $ poppySPath ++ "/" ++ presetConfigDirr
  colors <- setColors
  colors2 <- setColorsACM
  let
    notExists = filter (\x -> not $ elem x presetConfs) presetConfigFileNames
  mapM (\x -> oVecToFileJP (V.singleton $ ushow 0) $ poppySPath ++ "/" ++ presetConfigDirr ++ "/" ++ x) notExists
  confContents <- V.mapM iVecFromFile $ V.fromList $ map (\x -> poppySPath ++ "/" ++ presetConfigDirr ++ "/" ++ x) presetConfigFileNames
  let
    getConfigContents configsPrim = map (parseConfig colors) $ V.toList $ V.map (\x -> read x :: String) $ V.tail configsPrim
    presetConfigs = map getConfigContents $ V.toList confContents
    config = map (parseConfig colors) $ V.toList $ V.map (\x -> read x :: String) configsPrim
    configBase = map (parseConfig colors) $ V.toList $ V.map (\x -> read x :: String) configsBasePrim
    -- isTablet = False
    isTablet = True
  isDual <- do
    if isTablet
      then do
        screen <- window `get` #screen
        monId <- Gdk.screenGetPrimaryMonitor screen
        area <- Gdk.screenGetMonitorWorkarea screen monId
        winHei <- GI.Gdk.Structs.Rectangle.getRectangleHeight area
        winWid <- GI.Gdk.Structs.Rectangle.getRectangleWidth area
        if winWid < winHei
         then return False
         else return True
      else return True
  let
    res = CDocs {
        dksPoppySPath = poppySPath
      , dksOffSetDX = 8.0
      , dksOffSetDY = 8.0
      , dksOffSetNextTo = 16.0
      -- , dksDebug = Hint
      , dksDebug = Local
      , dksNofHint = 2
      , dksIsVanilla = False
      , dksIsTang = False
      , dksColors = colors
      , dksKeysStacked = []
      , dksForMisDoublePress = False
      , dksGlobalConfig = config
      , dksBaseConfig = configBase
      , dksIsDeleting = False
      , dksIsDualPage = isDual
      , dksPresetConfigs = presetConfigs
      , dksIsYondle = False
      , dksIsTablet = isTablet
      , dksIsDefaultSize = True
      }
  return res

initDoc :: IORef Docs -> String -> IO Doc
initDoc docsRef fpath = do
  docs <- readIORef docsRef
  let
    poppySPath = dksPoppySPath docs
    configFilesDir = poppySPath ++ "/configs"
  createDirectoryIfMissing False configFilesDir
  files <- listDirectory configFilesDir
  let
    configSuffix = "_config.txt"
    colors = dksColors docs
    baseConfig = dksBaseConfig docs
    -- configs = map (takeWhile (\c -> not $ c == '_')) $ filter (\x -> Lis.isSuffixOf ".txt" x) files
    configs = map (\str -> take (length str - length configSuffix) str) $ filter (\x -> Lis.isSuffixOf configSuffix x) files
    currDocName = reverse $ takeWhile (\c -> not $ c == '/') $ tail $ dropWhile (\c -> not $ c == '.') $ reverse fpath
    isExistsConfig = elem currDocName configs
    configFilePath = configFilesDir ++ "/" ++ currDocName ++ configSuffix
  when (not isExistsConfig) $ oVecToFileJP (V.singleton $ ushow 0) configFilePath

  doc <- GPop.documentNewFromFile (Text.pack fpath) Nothing
  nOfPage <- GPop.documentGetNPages doc
  when (nOfPage == 1) $ modifyIORef docsRef (\dks -> dks {dksIsDualPage = False})
  configsPrim <- iVecFromFile configFilePath
  let
    firstPageFromFile = read $ (tail . init ) $ V.head configsPrim :: Int32
    config = map (parseConfig colors) $ V.toList $ V.map (\x -> read x :: String) $ V.tail configsPrim
    firstPage = stopper id 0 (nOfPage - 2)  firstPageFromFile :: Int32
    isMonoPage = nOfPage == 1
    nextPage
     -- | isMonoPage = -1
     | isMonoPage = 0
     | otherwise = firstPage + 1
  page <- GPop.documentGetPage doc firstPage
  txt <- Text.unpack <$> GPop.pageGetText page
  pageHalf <- GPop.documentGetPage doc $ div nOfPage 2
  txtHalf <- Text.unpack <$> GPop.pageGetText pageHalf
  (wid, hei) <- GPop.pageGetSize page
  (widNext, heiNext) <- do
        pageNext <- GPop.documentGetPage doc nextPage
        sizeN <- GPop.pageGetSize pageNext
        return sizeN
  let
    engRatio = (fromIntegral $ length $ filter f $ txt ++ txtHalf) / (fromIntegral $ length $ txt ++ txtHalf)
      where
        f c = elem c $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
    isJP
     | txt == "" = True
     | engRatio < 0.5 = True
     | otherwise = False
    clipSq = CSq {sqLeft = 0, sqTop = 0, sqRight = wid, sqBot = hei}
    clipSqNext = CSq {sqLeft = 0, sqTop = 0, sqRight = widNext, sqBot = heiNext}
    res = CDoc {
      dkPDFPath = fpath
    , dkPDFDocName = currDocName
    , dkCurrDoc = doc
    -- , dkConfig = config
    , dkConfig = config ++ baseConfig
    , dkConfigYank = []
    , dkConfigForRedo = []
    , dkCurrToken = ""
    , dkCurrPage = firstPage
    -- , dkNextPage = nextPage -- -negative if not 2 pages
    , dkNextPage = nextPage
    , dkTogColIndex = 0
    , dkIsJapanese = isJP
    , dkClipSq = clipSq
    , dkClipSqNext = clipSqNext
    , dkClickedSquare = (-1, [])
    }
  return res

getWindowTitleFromDoc :: IORef Docs -> IORef Doc -> IO [Char]
getWindowTitleFromDoc docsRef docRef = do
  docs <- readIORef docsRef
  doc <- readIORef docRef
  let
    currDoc = dkCurrDoc doc
    -- docName = dkPDFDocName doc
  docName <- catch (do
      docTitle <- GPop.documentGetTitle currDoc
      let
        titleRes
         | docTitle == "" = dkPDFDocName doc
         | otherwise = Text.unpack docTitle
      -- return $ Text.unpack docTitle
      return titleRes
      )
    (\ (err::SomeException) -> do
      return $ dkPDFDocName doc)
  -- docTitle <- GPop.documentGetTitle currDoc
  nOfPage <- GPop.documentGetNPages currDoc
  let
    currPage = dkCurrPage doc
    lang
     | dkIsJapanese doc = "JP"
     | otherwise = "EN"
    mode = dksDebug docs
    res = docName ++ ": " ++ "[" ++ (show currPage) ++ "/" ++ (show nOfPage) ++ "]" ++ " lang := " ++ lang ++ ": " ++ "mode := " ++ (show mode)
  return res

initMVars :: IORef Doc -> IO MVars
initMVars docRef = do
  doc <- readIORef docRef
  let
    currDoc = dkCurrDoc doc
    currPage = dkCurrPage doc

  page <- GPop.documentGetPage currDoc currPage
  nOfPage <- GPop.documentGetNPages currDoc
  sexps  <- MV.replicate (fromIntegral nOfPage) Nothing
  let
    res = CMVars {
      mVarSExps = sexps
    }
  return res

stackStan window mvars docsRef docRef = do
  mvs <- readMVar mvars
  let
    mvSexps = mVarSExps mvs
  docs <- readIORef docsRef
  doc <- readIORef docRef
  let
    currDoc = dkCurrDoc doc
    currPage = fromIntegral $ dkCurrPage doc
    isJapanese = dkIsJapanese doc
  maxPage <- fromIntegral <$> GPop.documentGetNPages currDoc
  nextForward <- getNextNothing mvSexps currPage
  nextBackward <- getPrevNothing mvSexps currPage

  if nextForward == Nothing && nextBackward == Nothing
    then putStrLn "parse Finished" >> return ()
    else do
    let
      f = case nextForward of
        Nothing -> case nextBackward of
          Nothing -> return mvSexps
          Just m -> do
            mP <- GPop.documentGetPage currDoc $ fromIntegral m
            newSExp <- getSExpsIOS mP isJapanese
            MV.write mvSexps m (Just newSExp)
            return mvSexps
        Just m2 -> case nextBackward of
          Nothing -> do
            m2P <- GPop.documentGetPage currDoc $ fromIntegral m2
            newSExp <- getSExpsIOS m2P isJapanese
            MV.write mvSexps m2 (Just newSExp)
            return mvSexps
          Just m -> do
            mP <- GPop.documentGetPage currDoc $ fromIntegral m
            newSExp  <- getSExpsIOS mP isJapanese
            MV.write mvSexps m (Just newSExp)
            mP2 <- GPop.documentGetPage currDoc $ fromIntegral m2
            newSExp2  <- getSExpsIOS mP2 isJapanese
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

getNextNothing :: MV.MVector RealWorld (Maybe a) -> Int -> IO (Maybe Int)
getNextNothing mv i = do
  let
    len = MV.length mv
  if len - 1 < i || i < 0
    then return Nothing
    else do
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
  let
    len = MV.length mv
  if len - 1 < i || i < 0
    then return Nothing
    else do
      hoge <- MV.read mv i
      let
        f :: IO (Maybe Int)
        f = case hoge of
          Nothing -> return (Just i)
          Just _ -> if (i - 1) < 0
            then return Nothing
            else getPrevNothing mv (i - 1)
      f

goOtherPage window docsRef docRef inclF inclFNext = do
  -- (winWid, winHei) <- Gtk.windowGetSize window
  doc <- readIORef docRef
  docs <- readIORef docsRef
  let
    isTablet = dksIsTablet docs
    currDoc = dkCurrDoc doc
  nOfPage <- GPop.documentGetNPages currDoc
  let
    currPagePrev = dkCurrPage doc
    currPage = inclF currPagePrev nOfPage
    nextPagePrev = dkNextPage doc
    nextPage = inclFNext nextPagePrev nOfPage
  modifyIORef docRef (\x -> x {dkCurrPage = currPage, dkNextPage = nextPage})
  modifyIORef docsRef (\x -> x {dksNofHint = 2})
  when isTablet $ saveConfigs docsRef docRef
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
    pageNext <- do
      if nOfPage == 1
        then return page
        else GPop.documentGetPage currDoc nextPage
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

stopper f minmPrim maxmPrim n
  | f n < minm = minm
  | maxm < f n = maxm
  | otherwise = f n
    where
      (minm, maxm)
       | maxmPrim < minmPrim = (maxmPrim, minmPrim)
       | otherwise = (minmPrim, maxmPrim)


forCheckForget sexps = do
  let
    forgotten = map ((concatMap g) . forgetSExp) sexps
        where
          g (x1, x2) = case x2 of
            Nothing -> []
            Just x22 -> [(x1, x22)]
  cshowIL forgotten
  return forgotten

-- isBottomBy needs to be replaced.
getColundRectangles
  :: Foldable t =>
     [SExp (a, Tag) ([Char], [Sq Double])]
     -> t ([Char], GPop.Color)
     -> Bool
     -> Colors
     -> Mode
     -> [IO (GPop.Color, GPop.Rectangle)]
getColundRectangles sexps configs isJapanese colors mode = electeds
  where
      detacheds = map (map g2)
        $ map (filter g)
        $ map takeSndL
        $ map forgetSExp
        $ concatMap ((filter filterFunction) . (takeSpecTags (\x -> x == NP || x == ADJP || x == VBN || x == VBG)))
        $ map (mapNode snd (\x -> (fst x, synSqs $ snd x))) sexps
        where
          filterFunction
           -- | isJapanese = (\y -> countNofChars (mapNode id fst y) < nOfWordsUB)
           | isJapanese = (\y -> True)
           | otherwise = (\y -> isBottomBy id y)
            where
              nOfWordsUB = 15
          g x = case x of
            Nothing -> False
            Just _ -> True
          g2 x = case x of
            Nothing -> ("", [])
            Just y -> y
      -- detachedsPrimitive = map listSubTokens sexps
      detachedsPrimitive = concatMap ((map snd) . (concatMap g) . forgetSExp) sexps
          where
            g (x1, x2) = case x2 of
              Nothing -> []
              Just x22 -> [(x1, x22)]

      detachedAssignPrimitive = Lis.nubBy g2 $ foldl g [] configs
         where
           g2 x y = snd x == snd y
           g y x@(stemX, color) = y ++ filtered
              where
                filtered = map (\xsq@(x, sq) -> (color, [sq])) $ filter (\xsq@(x, sq) -> (toLowers stemX) == x) stemmeds
           stemmeds
             | isJapanese = map (\xsq@(x, sq) -> (toLowers x, sq)) detachedsPrimitive
             | otherwise = map (\xsq@(x, sq) -> ((toLowers . stemEng) x, sq)) detachedsPrimitive

      detachedAssign detacheds0 = Lis.nubBy g2 $ foldl g [] configs
         where
           g2 x y = snd x == snd y
           g y x@(stemX, color) = y ++ filtered
              where
                filtered = [(color, sqs) | stem@(stems, sqs) <- stemmeds, elem (toLowers stemX) stems]
           stemmeds
             | isJapanese = map (\xs -> (map toLowers $ takeFstL xs, takeSndL xs)) detacheds0
             | otherwise = map (\xs -> (map (toLowers . stemEng) $ takeFstL xs, takeSndL xs)) detacheds0

      deChimera assed = filter f assed
          where
            f (col, sqs) = and $ map g assed
              where
                g (c, sq) = (not $ [] == filter (\x -> not $ elem x sq) sqs) || sq == sqs

      detachedAssignedsCyclic = assed
         where
           flattenedSqs = map takeSndL detacheds
           flattenedNotInc = filter f flattenedSqs
             where
               f sqs = and $ map g flattenedSqs
                 where
                   g sq = (not $ [] == filter (\x -> not $ elem x sq) sqs) || sq == sqs
           assed = map f $ indexingL flattenedNotInc
             where
               f (i, sqs)
                | mod i 2 == 0 = (colRed colors, sqs)
                | otherwise = (colBlue colors, sqs)

      detachedAssignedsCyclicPrimitive = map (\x@(x1, x2) -> (x1, [x2])) assed
         where
           flattenedSqs =  takeSndL detachedsPrimitive
           flattenedNotInc = filter f flattenedSqs
             where
               f sqs = and $ map g flattenedSqs
                 where
                   g sq = (not $ [] == filter (\x -> not $ elem x sq) sqs) || sq == sqs
           assed = map f $ indexingL flattenedNotInc
             where
               f (i, sqs)
                | mod i 2 == 0 = (colRed colors, sqs)
                | otherwise = (colBlue colors, sqs)

      detachedAssigneds
       -- | mode == Hint = detachedAssignedsHinted
       | mode == Hint  || mode == Adhoc || mode == Local = deChimera $ detachedAssign detacheds
       | mode == Gramatica = detachedAssignedsCyclic
       | mode == GramaticaPrim = detachedAssignedsCyclicPrimitive
       | mode == Primitive = deChimera detachedAssignPrimitive
       | otherwise = []
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

saveConfigs docsRef docRef = do
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
        outout = V.fromList $ (ushow currPage) : conf
        currDocName = dkPDFDocName doc
        configFilePath = configFilesDir ++ "/" ++ currDocName ++ "_config.txt"
      oVecToFileJP outout configFilePath
      oVecToFileJP (V.fromList confGlobal) globalConfigFilePath

deleteColors docsRef docRef window = do
      docs <- readIORef docsRef
      let
        baseConfig = dksBaseConfig docs
      -- modifyIORef docRef (\x -> x {dkConfigYank = dkConfig x, dkConfig = []})
      modifyIORef docRef (\x -> x {dkConfigYank = dkConfig x, dkConfig = baseConfig})
      modifyIORef docRef (\x -> x {dkClickedSquare = (-1, [])})
      Gtk.widgetQueueDraw window
      modifyIORef docRef (\doc -> doc  {dkTogColIndex = 0}) -- restart from Red (0)
      Gtk.windowSetTitle window $ Text.pack "DeColored"

toVanillaMode docsRef docRef window = do
      mode <- dksDebug <$> readIORef docsRef
      let
        newMode
         -- | mode == Vanilla = Hint
         | mode == Vanilla = Local
         | otherwise = Vanilla
      modifyIORef docsRef (\x -> x {dksDebug = newMode})
      Gtk.windowSetTitle window =<< Text.pack <$> getWindowTitleFromDoc docsRef docRef
      Gtk.widgetQueueDraw window

data Direction = DTop | DBot | DLeft | DRight
  deriving (Show, Eq)


setClipSq docsRef docRef dir = do
  doc <- readIORef docRef
  let
    currDoc = dkCurrDoc doc
    currPage = dkCurrPage doc
  page <- GPop.documentGetPage currDoc currPage
  (pWid', pHei') <- GPop.pageGetSize page
  let
    clipSq = dkClipSq doc
    tp = sqTop clipSq
    bt = sqBot clipSq
    lf = sqLeft clipSq
    rg = sqRight clipSq
    rat = (bt - tp) / (rg - lf)
    pitchX = 3.0
    pitchY = pitchX * rat
    newClipSq = newSqPrim
      where
        (tpPrim, btPrim, lfPrim, rgPrim)
         | dir == DTop && (tp + pitchY < bt)  = (tp + pitchY, bt, lf, rg)
         | dir == DBot  && (tp < bt - pitchY)  = (tp, bt - pitchY, lf, rg)
         | dir == DLeft  && (lf + pitchX < rg) = (tp, bt, lf + pitchX, rg)
         | dir == DRight  && (lf < rg - pitchX) = (tp, bt, lf, rg - pitchX)
         | otherwise     = (tp, bt, lf, rg)
        newSqPrim = CSq
          { sqTop = tpPrim
          , sqBot = btPrim
          , sqLeft = lfPrim
          , sqRight = rgPrim
          }
  modifyIORef docRef (\x -> x {dkClipSq = newClipSq})
  return ()
