{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Prelude as P
import System.IO
import qualified System.IO.Strict as STR
import qualified Data.Vector as V
import qualified Data.List as Lis
import Turtle
import qualified Data.Text as Text
import qualified Control.Foldl as Fold
import Text.Show.Unicode

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as T

readFileT :: String -> IO T.Text
readFileT fname = do
  sjis <- mkTextEncoding "CP932"
  readFile' utf8_bom `E.catches` [readTE sjis]
   where
    readTE te = E.Handler $ \(e::E.SomeException) -> readFile' te

    readFile' :: TextEncoding -> IO T.Text
    readFile' te =
      withFile fname ReadMode $ \h -> do
        hSetEncoding h te
        T.hGetContents h

takeFst = V.map fst
takeSnd = V.map snd

fstT = \x@(x1,x2,x3) -> x1
sndT = \x@(x1,x2,x3) -> x2
thdT = \x@(x1,x2,x3) -> x3

takeFstT = V.map fstT
takeSndT = V.map sndT
takeThdT = V.map thdT

takeFstL = map fst
takeSndL = map snd

takeFstTL = map fstT
takeSndTL = map sndT
takeThdTL = map thdT

someFunc :: IO ()
someFunc = putStrLn "someFunc"

vSortBy :: Ord a => (a -> a -> Ordering) -> V.Vector a -> V.Vector a
vSortBy f xs = V.fromList $  Lis.sortBy f $ V.toList xs

iVecFromFile :: String -> IO (V.Vector String)
iVecFromFile listPath = do
  fText <- STR.readFile listPath
  let forout = V.fromList $ lines fText :: V.Vector String
  return forout

iVecFromFileStrict :: String -> IO (V.Vector String)
iVecFromFileStrict listPath = do
  handle <- openFile listPath ReadMode
  fText <- hGetContents handle
  let forout = V.fromList $ lines fText :: V.Vector String
  return forout

oVecToFile :: Show a => V.Vector a -> String -> IO ()
oVecToFile vec fpath = do
  handle <- openFile fpath WriteMode
  V.mapM (\x -> hPutStrLn handle $ show x) vec
  hClose handle


iVecFromFileJP :: String -> IO (V.Vector String)
iVecFromFileJP listPath = do
  handle <- openFile listPath ReadMode
  hSetEncoding handle utf8
  fText <- hGetContents handle
  --fText <- T.unpack <$> readFileT listPath
  let forout = V.fromList $ lines fText :: V.Vector String
  return forout

oVecToFileJP :: Show a => V.Vector a -> String -> IO ()
oVecToFileJP vec fpath = do
  handle <- openFile fpath WriteMode
  hSetEncoding handle utf8
  V.mapM (\x -> hPutStrLn handle $ ushow x) vec
  hClose handle

oFileJP :: String -> IO ()
oFileJP fpath = do
  handle <- openFile fpath WriteMode
  hSetEncoding handle utf8
  hClose handle

cshow :: Show a => V.Vector a -> IO (V.Vector ())
cshow vec = do
  V.mapM (\x -> uprint x) vec

cshowIndexing :: Show a => V.Vector a -> IO (V.Vector ())
cshowIndexing vs = cshow $ indexing vs

cshowI :: Show a => V.Vector a -> IO (V.Vector ())
cshowI = cshowIndexing

vNub :: Eq a => V.Vector a -> V.Vector a
vNub xs = V.fromList $ Lis.nub $ V.toList xs

vNubBy :: Eq a => (a -> a -> Bool) -> V.Vector a -> V.Vector a
vNubBy f xs = V.fromList $ Lis.nubBy f $ V.toList xs

delimitAtWO2 :: Eq a => a -> ([a] -> b) -> [a] -> [b]
delimitAtWO2 del conv bulks =
  (conv $ snd res) : (fst res)
   where
     res = P.foldr (f del conv) ([],[]) bulks
       where
         f :: Eq a => a -> ([a] -> b)  -> a -> ([b], [a]) -> ([b], [a])
         f d cv a ([], []) = if a == d then ([], []) else ([], [a])
         f d cv a (bs, as) = if a == d
           then ((cv as) : bs, []) else (bs, a : as)

delimitAtW2 :: Eq a => a -> ([a] -> b) -> [a] -> [b]
delimitAtW2 del conv bulks =
  (conv $ snd res) : (fst res)
   where
     res = P.foldr (f del conv) ([],[]) bulks
       where
         f :: Eq a => a -> ([a] -> b)  -> a -> ([b], [a]) -> ([b], [a])
         f d cv a ([], []) = if a == d then ([cv [d]], []) else ([], [a])
         f d cv a (bs, as) = if a == d
           then ((cv [d]) : (cv as) : bs, []) else (bs, a : as)

delimitAtWO2By :: (a -> Bool) -> ([a] -> b) -> [a] -> [b]
delimitAtWO2By g conv bulks =
  (conv $ snd res) : (fst res)
   where
     res = P.foldr (f g conv) ([],[]) bulks
       where
         f :: (a -> Bool) -> ([a] -> b)  -> a -> ([b], [a]) -> ([b], [a])
         f g cv a ([], []) = if g a then ([], []) else ([], [a])
         f g cv a (bs, as) = if g a
           then ((cv as) : bs, []) else (bs, a : as)

indexing :: V.Vector a -> V.Vector (Int, a)
indexing xs = V.zip (V.fromList [0 .. (V.length xs - 1)]) xs

indexingL :: [a] -> [(Int, a)]
indexingL xs = zip [0 .. (length xs - 1)] xs

deMaybe :: a -> Maybe a -> a
deMaybe defo val =
  case val of
    Nothing -> defo
    Just v -> v

getHistogram :: Eq a => Ord a => [a] -> [(Int, a)]
getHistogram xsPrim = res
  where
    xs = Lis.sort xsPrim
    res
     | xs == [] = []
     | otherwise = pll ++ [st]
       where
         res2@(st,pll) = foldl g ((1, head xs), []) $ tail xs
         g y@((n2, y2), pl) x
          | y2 == x = ((n2 + 1, y2), pl)
          | otherwise = ((1, x), pl ++ [(n2, y2)])

execShell :: MonadIO io => String -> io ExitCode
execShell str = do
  shell (Text.pack str) Turtle.empty

--cmd = fromString $ "echo " P.++ "\""  P.++ sens P.++ "\"" P.++ " | mecab"

get2 :: (MonadIO m) => Text -> m [Line]
get2 cmd = do
  fold (inshell cmd Turtle.empty) Fold.list

getLSRes :: String -> IO (V.Vector String)
getLSRes path = getShellRes $ "ls " P.++ path

getShellRes :: String -> IO (V.Vector String)
getShellRes cmd = do
  let
    texized = Text.pack cmd
  res2 <- fmap (Text.unpack . lineToText) <$> get2 texized
  let
    res3 = V.fromList res2
  return res3

vlookup :: Eq a => a -> V.Vector (a, b) -> b -> b
vlookup query list dum = deMaybe dum (lookup query $ V.toList list)

vlookupBy :: Eq a => Eq b => a -> V.Vector (a, b) -> b -> (a -> a -> Bool) -> b
vlookupBy query list dum f =
  let
    aa = V.foldl' (g query) V.empty list
       where
         g que y x@(xa, xb)
          | (f que xa) == True = y V.++ (V.singleton xb)
          | otherwise = y
  in
   if
     aa == V.empty
   then dum
   else V.head aa

ands = V.foldl' (\y -> \x -> x && y) True
ors = V.foldl' (\y -> \x -> x || y) False

bundleBy :: Ord a => (a -> a -> Bool) -> (a -> a -> Ordering) -> V.Vector a -> V.Vector (V.Vector a)
bundleBy f fc xsPrim
  | xsPrim == V.empty = V.empty
  | otherwise = resPooled V.++ (V.singleton resStacked)
    where
      xs = vSortBy fc xsPrim
      res@(resStacked, resLabel, resPooled) = V.foldl' f2 (V.singleton $ V.head xs, V.head xs, V.empty) (V.tail xs)
      f2 y@(stacked, label, pooled) x
        | f label x == True = (stacked V.++ (V.singleton x), label, pooled)
        | otherwise = (V.singleton x, x, pooled V.++ (V.singleton stacked))

getDivideLine ns
  | ns == [] = 0
  | length ns == 1 = head ns
  | length histo == 1 = snd $ V.head histo
  | length histo == 2 = ((snd $ V.head histo) + (snd $ V.last histo)) * 0.5
  | otherwise = ((snd $ histo V.! resIndex) + (snd $ histo V.! (resIndex + 1))) * 0.5
  where
    nsSorted = Lis.sort ns
    histo = V.fromList $ getHistogram nsSorted
    resIndex = fst $ Lis.minimumBy (\x -> \y -> compare (snd x) (snd y)) $ V.toList $ V.imap g $ V.init histo
      where
        g i (coun, posi) = (i, scorePrev + scorePost)
          where
            prevs = V.ifilter h histo
              where
                h j _ = j <= i
            posts = V.ifilter h histo
              where
                h j _ = not $ j <= i
            scorePrev = V.sum $ V.map (\x@(n, p) -> (fromIntegral n) * abs (p - posi)) prevs
            scorePost = V.sum $ V.map (\x@(n, p) -> (fromIntegral n) * abs (p - posi)) posts

capNumber f lb ub n
  | f n < lb = lb
  | ub < f n = ub
  | otherwise = f n

fstQ (a, b, c, d) = a
sndQ (a, b, c, d) = b
thdQ (a, b, c, d) = c
forQ (a, b, c, d) = d

takeFstQ = V.map fstQ
takeSndQ = V.map sndQ
takeThdQ = V.map thdQ
takeForQ = V.map forQ

countPrefix :: Eq a => [a] -> [a] -> [a]
countPrefix [] _ = []
countPrefix _ [] = []
countPrefix (x:xs) (y:ys)
  | x == y = [x] ++ (countPrefix xs ys)
  | otherwise = []


countSuffix :: Eq a => [a] -> [a] -> [a]
countSuffix xs ys = reverse $ countPrefix (reverse xs) (reverse ys)













