{-# LANGUAGE GADTs #-}

module ParserP where

import Control.Monad
import Control.Applicative
import SExp
import Debug.Trace

data ParserP a b = CParserP ([a] -> [(b, [a])])

parse :: ParserP a b -> ([a] -> [(b, [a])])
parse (CParserP f) = f

instance Functor (ParserP a) where
  fmap f parser = CParserP resParser
    where
      resParser as = [(f a, as') | (a, as') <- parse parser $ as]

instance Applicative (ParserP a) where
  pure b = CParserP f
    where
      f str = [(b, str)]
  (<*>) pf p = CParserP resParser
    where
      resParser xs = [(f x, zs) | (f, ys) <- parse pf $ xs, (x, zs) <- parse p $ ys]

instance Monad (ParserP a) where
  return = pure
  (>>=) p q = CParserP resParser
     where
       resParser xs = [(y, zs) | (x, ys) <- parse p $ xs, (y, zs) <- parse (q x) $ ys]

instance Alternative (ParserP a) where
  empty = failp
    where
      failp = CParserP (\xs -> [])
  (<|>) p1  p2 = CParserP f
  -- (<|>) p1  p2 = CParserP (\xs -> parse p1 xs ++ parse p2 xs)
    where
      f xs =
        let
           parsed1 = parse p1 xs
           parsed2 = parse p2 xs
        in
         case parsed1 of
           [] -> parsed2
           _ -> parsed1
  many p = (p <:> many p) <|> (succeed [])
    where
      (<:>) p1 q1 = CParserP (\xs -> [(r : rs, zs) | (r, ys) <- parse p1 xs, (rs, zs) <- parse q1 ys])
  some p = (:) <$> p <*> many p


instance MonadPlus (ParserP a) where
  mzero = empty
  mplus = (<|>)


fail = CParserP (\xs -> [])

succeed :: b -> ParserP a b -- reterns r and xs not consumed.
succeed r = CParserP (\xs -> [(r, xs)])

single :: Eq a => a -> ParserP a a -- parse exact value.
single c = CParserP (\ys -> singlePrim c ys)
  where
   singlePrim c [] = []
   singlePrim c (x : xs)
    | c == x = [(x, xs)]
    | otherwise = []

satisfy :: (a -> Bool) -> ParserP a a -- parse predicate.
satisfy p = CParserP (\xs -> satisfyPrim p xs)
  where
   satisfyPrim p [] = []
   satisfyPrim p (x:xs)
    | p x = [(x,xs)]
    | otherwise = []

anySingle :: ParserP a a
anySingle = satisfy (const True)

tokens :: ([a] -> [a] -> Bool) -> [a] -> ParserP a [a]
tokens p cs = CParserP (\xs -> matchPrim cs xs)
  where
    matchPrim cs xs
     | p cs $ take n xs = [(cs, drop n xs)]
     | otherwise = []
       where
         n = length cs

chunk :: Eq a => [a] -> ParserP a [a]
chunk = tokens (==)

just :: ParserP a b -> ParserP a b
just p = CParserP (\xs -> filter (null . snd) $ parse p xs)

char :: Char -> ParserP Char Char
char c = satisfy (== c)

try :: ParserP a b -> ParserP a b
try p = CParserP f
  where
    f xs = do
      aa <- parse p xs
      let
        res = case aa of
          (b, []) -> (b, xs)
          cc -> cc
      return res

notSExpStructual = satisfy f
  where
    -- cs = ['[', '(', '"', ')', ']', ' ']
    cs = ['(', ')', ' ']
    f c = not $ elem c cs

pSExp :: ParserP Char (SExp Tag String)
pSExp = do
  res <- pAtom <|> do
    void (char '(')
    typeConstruct <- some notSExpStructual
    tSExp <- some pSExp
    void (char ')')
    return $ Opr (read typeConstruct :: Tag) tSExp
  return res

pAtom :: ParserP Char (SExp Tag String)
pAtom = do
  void (char '(')
  typeConstruct <- some notSExpStructual
  void (char ' ')
  tok <- some notSExpStructual
  void (char ')')
  return  $ Atom (read typeConstruct :: Tag) tok
