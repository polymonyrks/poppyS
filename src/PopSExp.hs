{-# LANGUAGE GADTs #-}

module PopSExp where

import Lib
import qualified Data.Vector as V
import System.Directory
import SExp
import ParserP
import Data.Char
import Data.String.Utils
import Data.List as Lis
import Lib (takeFst, takeSnd, cshow, cshowI, vSortBy, iVecFromFile, oVecToFile, vNub, delimitAtWO2, indexing, deMaybe)
import Control.Monad
import Control.Applicative
import Debug.Trace
import Control.DeepSeq
import Text.Show.Unicode


reconsSExp :: (Eq a) => (Eq b) => [(Posi, a, Maybe b)] -> SExp (Posi, a) b
reconsSExp forgotten = foldl' injectSExpI Nil forgotten

injectSExpI :: (Eq a) => (Eq b) => SExp (Posi, a) b -> (Posi, a, Maybe b) -> SExp (Posi, a) b
injectSExpI Nil (pos, tag, val)
  | val == Nothing = Opr ([], tag) []
  | otherwise = Nil
injectSExpI (Atom (pos0, tag0) val0) (pos, tag, val) = Nil
injectSExpI (Opr (pos0, tag0) []) (pos, tag, val)
  | isEven = Nil
  | isOver1 && isPosLast0 = case val of
      Nothing -> Opr (pos0, tag0) [Opr (pos, tag) []]
      Just v ->  Opr (pos0, tag0) [Atom (pos, tag) v]
  | otherwise = Nil
  where
    isEven = length pos0 == length pos
    isOver1 = length pos0 + 1 == length pos
    isPosLast0 = last pos == 0
injectSExpI (Opr (pos0, tag0) xs0) newcommer@(pos, tag, val)
  | isEven = Nil
  | isOver1 = case (last xs0) of
        Nil -> Nil
        Opr (po, ta) va -> result
          where
            result
              | isLastMatch = case val of
                  Nothing -> Opr (pos0, tag0) $ xs0 ++ [Opr (pos, tag) []]
                  Just v -> Opr (pos0, tag0) $ xs0 ++ [Atom (pos, tag) v]
              | otherwise = Nil
            isLastMatch = init po == init pos && last po + 1 == last pos
        Atom (po, ta) va -> result2
          where
            result2
             | isLastMatch = case val of
                 Nothing -> Opr (pos0, tag0) $ xs0 ++ [Opr (pos, tag) []]
                 Just v  -> Opr (pos0, tag0) $ xs0 ++ [Atom (pos, tag) v]
             | otherwise = Nil
            isLastMatch = init po == init pos && last po + 1 == last pos
  | isOver2More = Opr (pos0, tag0) ((init xs0) ++ [injectSExpI (last xs0) newcommer])
  | otherwise = Nil
  where
    isEven = length pos0 == length pos
    isOver1 = length pos0 + 1 == length pos
    isOver2More = 1 < length pos - length pos0

forgetSExpI :: SExp a b -> [(Posi, a, Maybe b)]
forgetSExpI sexp = reshaped
  where
    indexed = indexingSP sexp
    forgotten = forgetSExp indexed
    reshaped = map (\x@((i, tag), tok) ->(i, tag, tok)) forgotten

forgetSExp :: SExp a b -> [(a, Maybe b)]
forgetSExp Nil = []
forgetSExp (Atom tag1 tok1) = [(tag1, Just tok1)]
forgetSExp (Opr tag1 ts1) = (tag1, Nothing) : (concatMap forgetSExp ts1)

effectSExp :: (SExp a b -> SExp a b) -> Posi -> SExp a b -> SExp a b
effectSExp f [] sexp = f sexp
effectSExp f (i : is) Nil = Nil
effectSExp f (i : is) (Atom tag1 tok1) = (Atom tag1 tok1)
effectSExp f (i : is) (Opr tag1 ts1) = (Opr tag1 $ updateList ts1 (i, effectSExp f is))

replaceSExp :: SExp a b -> Posi -> SExp a b -> SExp a b
replaceSExp sexpReplnd pos sexpReplns = effectSExp (\x -> sexpReplns) pos sexpReplnd

zipSExp :: SExp a c -> SExp b d -> SExp (a, b) (c, d)
zipSExp Nil _ = Nil
zipSExp _ Nil = Nil
zipSExp (Atom tag1 tok1) (Atom tag2 tok2) = Atom (tag1, tag2) (tok1, tok2)
zipSExp (Atom tag1 tok1) _ = Nil
zipSExp _ (Atom tag1 tok1) = Nil
zipSExp (Opr tag1 ts1) (Opr tag2 ts2) = Opr (tag1, tag2) (zipWith zipSExp ts1 ts2)

isBottomBy :: (Eq c) => (a -> c) -> SExp a b -> Bool
isBottomBy f sexp = isBottom $ mapNode f id sexp

isBottom :: (Eq a) => SExp a b -> Bool
isBottom Nil = False
isBottom (Atom x y) = True
isBottom (Opr x ys)
  | isIncBs = False
  | otherwise = and $ map isBottom ys
  where
    bsTags = map f ys
      where
        f Nil = Nothing
        f (Atom x1 y) = Just x1
        f (Opr x1 y) = Just x1
    isIncBs = elem (Just x) bsTags

takeSpecTags :: (a -> Bool) -> SExp a b -> [SExp a b]
takeSpecTags f Nil = []
takeSpecTags f (Atom a b)
  | f a = [(Atom a b)]
  | otherwise = []
takeSpecTags f (Opr a bs)
  | f a = [Opr a bs] ++ childs
  | otherwise = childs
     where
       childs = (concatMap (takeSpecTags f) bs)
mapNode :: (a -> b) -> (c -> d) -> SExp a c -> SExp b d
mapNode f g Nil = Nil
mapNode f g (Atom tag tok) = Atom (f tag) (g tok)
mapNode f g (Opr tag1 ts) = Opr (f tag1) (map (mapNode f g) ts)

indexingSP :: SExp a b -> SExp ([Int], a) b
indexingSP Nil = Nil
indexingSP (Atom tag tok) = Atom ([], tag) tok
indexingSP (Opr tag1 ts) = res2
  where
    zipped = zip [0 ..] ts
    res2 = Opr ([], tag1) mapped
       where
         mapped = map (\x@(i, t) -> f i $ indexingSP t) zipped
         f i sexp2 = case sexp2 of
           Nil -> Nil
           (Atom iTagA@(is, tagA) tokA) -> (Atom (i : is, tagA) tokA)
           (Opr iTagO@(is, tag0) ts)    -> (Opr (i : is, tag0) $ map (mapNode (g i) id) ts)
         g j (js, tag2) = (j : js, tag2)


refref :: SExp a b -> [Posi]
refref Nil = [[]]
refref (Atom _ _) = [[]]
refref (Opr _ ts) = res -- [] : [ i:p | (i, t) <- zip [0 ..] ts, p <- refref t]
  where
    zipped = zip [0 ..] ts
    res = [] : concatMap (\x@(i, t) -> map (\js -> i : js) (refref t)) zipped

compareSExpIndex :: Posi -> Posi -> Ordering
compareSExpIndex [] [] = EQ
compareSExpIndex [] _ = LT
compareSExpIndex _ [] = GT
compareSExpIndex (i : is) (j : js)
  | i == j = compareSExpIndex is js
  | otherwise = compare i j

getSens res
  | res == V.empty = V.empty
  | res == V.singleton "" = V.empty
  | otherwise = filtered
  where
    breaklines = V.cons 0 $  takeFst $ V.filter (\x@(n, c) -> c == "") $ indexing res
    spans = takeSnd $ V.filter (\x@(i, x2) -> mod i 2 == 0) $ indexing $  V.zip (V.init breaklines) $ V.tail breaklines
    sens = V.map (\n@(n1, n2) -> V.map (\m -> res V.! m) $ V.fromList [n1 .. n2 - 1]) spans
    filtered = V.map ((V.filter (\x -> not $ x == ""))) sens

retrTags :: V.Vector String -> V.Vector Tag
retrTags raw = res2
  where
    res = getSens raw
    reshapeds = V.map reshapeSexp res
    parsed = V.concatMap (\sss -> takeFst $ V.fromList $ filter (\x@(ps, residue) -> residue == "") $ parse pSExp sss) reshapeds
    ns = V.fromList [0 .. (V.length parsed - 1)]
    retrOnce n = subTags
      where
        sexp = parsed V.! n
        tag1 = "NP"
        subTags =  map (deMaybe TAGVOID . rootTag) $ subTrees sexp
    res2 = vNub $ V.concatMap (V.fromList . retrOnce) ns

retrNounPatterns :: V.Vector String -> V.Vector (V.Vector Tag, String)
retrNounPatterns raw = res3
  where
    res = getSens raw
    reshapeds = V.map reshapeSexp res
    parsed = V.concatMap (\sss -> takeFst $ V.fromList $ filter (\x@(ps, residue) -> residue == "") $ parse pSExp sss) reshapeds
    ns = V.fromList [0 .. (V.length parsed - 1)]
    retrOnce n = subTags
      where
        sexp = parsed V.! n
        tag1 = "NP"
        subTags = map (\x -> map (\x -> (deMaybe TAGVOID $ rootTag x, childToken x)) $ subTree1 x) $ filter (\x -> (isAtomOpr x) && rootTag x == Just NP) $ subTrees sexp
    res2 = V.concatMap (V.fromList . retrOnce) ns
    res3 = V.map (\r -> (takeFst $ V.fromList r, h $ takeSnd $ V.fromList r)) res2
       where
         h vs
          | vs == V.empty = ""
          | otherwise = map toLower $ V.foldl' (\y -> \x -> y ++ " " ++ x) (V.head vs) $ V.tail vs

getHistogram :: Ord a => V.Vector a -> V.Vector (a, Int)
getHistogram xs = mergeHistogram $ V.map (\x -> (x, 1)) xs

mergeHistogram :: Ord a => V.Vector (a, Int) -> V.Vector (a, Int)
mergeHistogram vs = res
  where
    sorted = vSortBy (\x@(x1,_) -> \y@(y1,_) -> compare x1 y1) vs  -- :: V.Vector (a, Int)
    res
     | sorted == V.empty = V.empty
     | otherwise = V.snoc poled st
       where
         res2@(st, poled) = V.foldl' f (V.head sorted, V.empty) $ V.tail sorted
           where
             f y@(stacked@(chsymY, chsY), pooled) x@(chsymX, chsX)
              | chsymY == chsymX = ((chsymX, chsY + chsX), pooled)
              | otherwise = (x, V.snoc pooled stacked)

vSort ls = vSortBy f ls
  where
    f x y = compare x y

vBundleBy :: Ord a => (a -> a -> Ordering) -> V.Vector a -> V.Vector (V.Vector a)
vBundleBy fc xsPrim
  | xsPrim == V.empty = V.empty
  | otherwise = V.snoc resPooled resStacked
    where
      xs = vSortBy fc xsPrim
      res@(resStacked, resLabel, resPooled) = V.foldl' f2 (V.singleton $ V.head xs, V.head xs, V.empty) (V.tail xs)
      f2 y@(stacked, label, pooled) x
        | fc label x == EQ = (stacked V.++ (V.singleton x), label, pooled)
        | otherwise = (V.singleton x, x, pooled V.++ (V.singleton stacked))

cshowR :: Show a => V.Vector a -> IO (V.Vector ())
cshowR vs = cshow $ V.reverse $ indexing vs

cshowS :: Show a => V.Vector a -> Int -> Int-> IO (V.Vector ())
cshowS vs n m = cshow forShow
  where
    indexed = indexing vs
    forShow = V.map (\n -> indexed V.! n) $ V.fromList [n .. m]


showSP sexp = cshow $ V.map (\str -> replace "\"" "" str) $ V.fromList $  delimitAtWO2 '\n' id $ printSExp' (0, sexp)


updateList :: [a] -> (Int, a -> a) -> [a]
updateList as (i, f) = res
  where
    vAS = V.fromList as
    updans = f $ vAS V.! i
    res = V.toList $ V.update vAS (V.singleton (i, updans))

parsePart :: (Eq b) => ParserP a b -> [a] -> b
parsePart p cs = head res
  where
    res = (map fst) $ parse p cs
