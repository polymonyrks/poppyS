{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module SExp where

import GHC.Generics (Generic, Generic1)
import Prelude as P
import qualified Data.Vector as V
import Data.String.Utils
import Control.DeepSeq
import Text.Show.Unicode
-- import qualified Data.Text as T

data SExp a b where
  Nil :: SExp a b
  Atom :: a -> b -> SExp a b
  Opr :: a -> [SExp a b] -> SExp a b
  deriving (Eq, Ord, Generic, Generic1, NFData)

instance (Show a, Show b) => Show (SExp a b) where
  show Nil = "[]"
  show (Atom t s)  = show t ++ " (" ++ (show s) ++ ")"
  show (Opr l ts) = "[." ++ show l ++ " " ++ show ts ++ "]"

type Posi = [Int]

foldSExp
  :: (tg -> a -> a)
     -> (a -> a -> a) -> (tg -> tk -> a) -> a -> SExp tg tk -> a
foldSExp f0 f1 f2 x0 Nil = x0
foldSExp f0 f1 f2 x0 (Atom a b) = f2 a b
foldSExp f0 f1 f2 x0 (Opr a bs) = f0 a $ foldl f1 x0 $ map (foldSExp f0 f1 f2 x0) bs

posi :: SExp a b -> [Posi]
posi Nil = [[]]
posi (Atom _ _) = [[]]
posi (Opr _ ts) = [] : [ i:p | (i, t) <- zip [0 ..] ts, p <- posi t]

subTree1 :: SExp a b -> [SExp a b]
subTree1 Nil = []
subTree1 (Atom _ _) = []
subTree1 (Opr _ ts) = ts

subTree :: SExp a b -> Posi -> SExp a b
subTree t [] = t
subTree (Opr _ ts) (i : is) = subTree (ts !! i) is

subTrees sexp = map (subTree sexp) $ posi sexp

subTokens :: SExp a String -> String
subTokens Nil = ""
subTokens (Atom tag tok) = tok
subTokens (Opr tag sexps) = V.foldl' g "" $ V.fromList sexps
  where
    g y x
      | y == "" = subTokens x
      | otherwise = y ++ " " ++ (subTokens x)

listSubTags :: SExp a b -> [a]
listSubTags Nil = []
listSubTags (Atom tag tok) = [tag]
listSubTags (Opr tag sexps) = tag : (V.foldl' g [] $ V.fromList sexps)
  where
    g y x = y ++ (listSubTags x)

listSubTokens :: SExp a b -> [b]
listSubTokens Nil = []
listSubTokens (Atom tag tok) = [tok]
listSubTokens (Opr tag sexps) = (V.foldl' g [] $ V.fromList sexps)
  where
    g y x = y ++ (listSubTokens x)

mapAtom :: (b -> c) -> SExp a b -> SExp a c
mapAtom f Nil = Nil
mapAtom f (Atom tag tok) = (Atom tag (f tok))
mapAtom f (Opr tag sexps) = (Opr tag (map (mapAtom f) sexps))

reshapeSexp :: V.Vector String -> String
reshapeSexp bb = dd
  where
    f y
     | res == y = y
     | otherwise = f res
      where
        res = replace " (" "("  y
    g = f
    cc = V.map g bb
    dd = replace "\n" "" $ V.foldl' (\y -> \x -> y ++ "\n" ++ x) "" cc

rootTag :: SExp a b -> Maybe a
rootTag Nil = Nothing
rootTag (Atom tag _) = Just tag
rootTag (Opr tag _)  = Just tag

childToken :: SExp a String -> String
childToken Nil = ""
childToken (Atom _ tok) = tok
childToken (Opr tag []) = ""
childToken (Opr tag (sexp : ps)) = V.foldl' g (childToken sexp) $ V.fromList ps
  where
    g y x = y ++ " " ++ (childToken x)

isAtom :: SExp a b -> Bool
isAtom Nil = False
isAtom (Atom _ _) = True
isAtom ((Opr _ _)) = False

isAtomTag :: (Eq a, Eq b) => a -> SExp a b -> Bool
isAtomTag tag sexp
  | ans == [] = True
  | subtrs == [] = True
  | otherwise = False
  where
    subtrs = subTrees sexp
    ans = filter (\x -> rootTag x == Just tag) $ tail subtrs

isAtomOpr :: (Eq a, Eq b) => SExp a b -> Bool
isAtomOpr Nil = False
isAtomOpr (Atom _ _) = False
isAtomOpr (Opr _ subtrs) = isAtoms
  where
    isAtoms = [] == filter (\x -> not $ isAtom x) subtrs

{-
data SExp where
  Nil :: SExp
  Atom :: Tag -> String -> SExp
  Opr :: Tag -> [SExp] -> SExp
  deriving (Eq, Ord)
-}

printSExp :: (Show a, Eq a, Show b, Eq b) => (Int, SExp a b) -> String
printSExp (n,  Nil) = ""
printSExp (n, (Atom tag tok)) = concat [(replicate n ' ') , (show tag) , " (" , (show tok) , ")"]
printSExp (n, (Opr tag ss)) = concat [(replicate n ' ') , "[. " , (show tag) , " [\n" , (g ss) , "]"]
  where
    g ss
      | ss == [] = ""
      | P.length ss == 1 = concat [printSExp (n + 1, V.head vss) , "]"]
      | otherwise = concat [(V.foldl' h ((printSExp ((n + 1),  vssHead)) ++ ",\n") $ vssTail), printSExp (n + 1, vssLast),  "]"]
        where
          vss = V.fromList ss
          vssHead = V.head vss
          vssTail0 = V.tail vss
          vssTail = V.init vssTail0
          vssLast = V.last vssTail0
          h y x = concat [y, (printSExp (n + 1, x)), ",\n"]

printSExp' :: (Show a, Eq a, Show b, Eq b) => (Int, SExp a b) -> String
printSExp' (n,  Nil) = ""
printSExp' (n, (Atom tag tok)) = concat [(replicate n ' '), (ushow tag), " (", (ushow tok), ")"]
printSExp' (n, (Opr tag ss)) = concat [(replicate n ' '), "[", (ushow tag), " [\n", (g ss), "]"]
  where
    g ss
      | ss == [] = ""
      | P.length ss == 1 = concat [printSExp' (n + 1, V.head vss), "]"]
      | otherwise = concat [(V.foldl' h (concat [(printSExp' ((n + 1),  vssHead)), ",\n"]) $ vssTail), printSExp' (n + 1, vssLast), "]"]
        where
          vss = V.fromList ss
          vssHead = V.head vss
          vssTail0 = V.tail vss
          vssTail = V.init vssTail0
          vssLast = V.last vssTail0
          h y x = concat [y, (printSExp' (n + 1, x)), ",\n"]

instance Show Tag where
   show ROOT = "ROOT"
   show NP = "NP"
   show ADJP = "ADJP"
   show JJ = "JJ"
   show PP = "PP"
   show TO = "TO"
   show NNP = "NNP"
   show NNPS = "NNPS"
   show CC = "CC"
   show CD = "CD"
   show COMMA = ","
   show NN = "NN"
   show S = "S"
   show NNS = "NNS"
   show VP = "VP"
   show MD = "MD"
   show RB = "RB"
   show VB = "VB"
   show VBN = "VBN"
   show IN = "IN"
   show SBAR = "SBAR"
   show PRP = "PRP"
   show ADVP = "ADVP"
   show PRPDOLL = "PRP$"
   show DT = "DT"
   show PERIOD = "."
   show VBZ = "VBZ"
   show VBP = "VBP"
   show VBG = "VBG"
   show RBS = "RBS"
   show EX = "EX"
   show RBR = "RBR"
   show COLON = ":"
   show WHNP = "WHNP"
   show WP = "WP"
   show VBD = "VBD"
   show PRT = "PRT"
   show RP = "RP"
   show FRAG = "FRAG"
   show QP = "QP"
   show X = "X"
   show SYM = "SYM"
   show INTJ = "INTJ"
   show UH = "UH"
   show SINV = "SINV"
   show FW = "FW"
   show JJR = "JJR"
   show PRN = "PRN"
   show WDT = "WDT"
   show BQUOTE = "``"
   show QUOTE = "''"
   show WHADVP = "WHADVP"
   show WRB = "WRB"
   show WHPP = "WHPP"
   show LRB = "-LRB-"
   show RRB = "-RRB-"
   show LST = "LST"
   show LS = "LS"
   show WPDOLL = "WP$"
   show CONJP = "CONJP"
   show POS = "POS"
   show UCP = "UCP"
   show PDT = "PDT"
   show NX = "NX"
   show JJS = "JJS"
   show DOLL = "$"
   show SHARP = "#"
   show SQ = "SQ"
   show SBARQ = "SBARQ"
   show RRC = "RRC"
   show WHADJP = "WHADJP"
   show NAC = "NAC"
   show TAGVOID = "TAGVOID"



instance Read Tag where
  readsPrec _ str
    | isEqToken "WHADJP" str = cRes "WHADJP" WHADJP str
    | isEqToken "WHADVP" str = cRes "WHADVP" WHADVP  str
    | isEqToken "CONJP" str = cRes "CONJP" CONJP str
    | isEqToken "-LRB-" str = cRes "-LRB-" LRB str
    | isEqToken "-RRB-" str = cRes "-RRB-" RRB str
    | isEqToken "SBARQ" str = cRes "SBARQ" SBARQ str
    | isEqToken "WHPP" str = cRes "WHPP" WHPP str
    | isEqToken "ROOT" str = cRes "ROOT" ROOT str
    | isEqToken "ADJP" str = cRes "ADJP" ADJP str
    | isEqToken "NNPS" str = cRes "NNPS" NNPS str
    | isEqToken "SBAR" str = cRes "SBAR" SBAR str
    | isEqToken "ADVP" str = cRes "ADVP" ADVP str
    | isEqToken "PRP$" str = cRes "PRP$" PRPDOLL str
    | isEqToken "WHNP" str = cRes "WHNP" WHNP str
    | isEqToken "FRAG" str = cRes "FRAG" FRAG str
    | isEqToken "INTJ" str = cRes "INTJ" INTJ str
    | isEqToken "SINV" str = cRes "SINV" SINV str
    | isEqToken "NNP" str = cRes "NNP" NNP str
    | isEqToken "NNS" str = cRes "NNS" NNS str
    | isEqToken "VBN" str = cRes "VBN" VBN str
    | isEqToken "PRP" str = cRes "PRP" PRP str
    | isEqToken "VBZ" str = cRes "VBZ" VBZ str
    | isEqToken "VBP" str = cRes "VBP" VBP str
    | isEqToken "VBG" str = cRes "VBG" VBG str
    | isEqToken "RBS" str = cRes "RBS" RBS str
    | isEqToken "RBR" str = cRes "RBR" RBR str
    | isEqToken "VBD" str = cRes "VBD" VBD str
    | isEqToken "PRT" str = cRes "PRT" PRT str
    | isEqToken "SYM" str = cRes "SYM" SYM str
    | isEqToken "JJR" str = cRes "JJR" JJR str
    | isEqToken "WRB" str = cRes "WRB" WRB str
    | isEqToken "LST" str = cRes "LST" LST str
    | isEqToken "WP$" str = cRes "WP$" WPDOLL str
    | isEqToken "POS" str = cRes "POS" POS str
    | isEqToken "UCP" str = cRes "UCP" UCP str
    | isEqToken "PDT" str = cRes "PDT" PDT str
    | isEqToken "JJS" str = cRes "JJS" JJS str
    | isEqToken "RRC" str = cRes "RRC" RRC str
    | isEqToken "NAC" str = cRes "NAC" NAC str
    | isEqToken "``" str = cRes "``" BQUOTE str
    | isEqToken "''" str = cRes "''" QUOTE str
    | isEqToken "NX" str = cRes "NX" NX str
    | isEqToken "NP" str = cRes "NP" NP str
    | isEqToken "JJ" str = cRes "JJ" JJ str
    | isEqToken "PP" str = cRes "PP" PP str
    | isEqToken "TO" str = cRes "TO" TO str
    | isEqToken "CC" str = cRes "CC" CC str
    | isEqToken "CD" str = cRes "CD" CD str
    | isEqToken "NN" str = cRes "NN" NN str
    | isEqToken "VP" str = cRes "VP" VP str
    | isEqToken "MD" str = cRes "MD" MD str
    | isEqToken "RB" str = cRes "RB" RB str
    | isEqToken "VB" str = cRes "VB" VB str
    | isEqToken "IN" str = cRes "IN" IN str
    | isEqToken "DT" str = cRes "DT" DT str
    | isEqToken "EX" str = cRes "EX" EX str
    | isEqToken "WP" str = cRes "WP" WP str
    | isEqToken "RP" str = cRes "RP" RP str
    | isEqToken "QP" str = cRes "QP" QP str
    | isEqToken "UH" str = cRes "UH" UH str
    | isEqToken "FW" str = cRes "FW" FW str
    | isEqToken "LS" str = cRes "LS" LS str
    | isEqToken "SQ" str = cRes "SQ" SQ str
    | isEqToken "," str = cRes "," COMMA str
    | isEqToken "S" str = cRes "S" S str
    | isEqToken "." str = cRes "." PERIOD str
    | isEqToken ":" str = cRes ":" COLON str
    | isEqToken "X" str = cRes "X" X str
    | isEqToken "$" str = cRes "$" DOLL str
    | isEqToken "WDT" str = cRes "WDT" WDT str
    | isEqToken "PRN" str = cRes "PRN" NAC str
    | isEqToken "#" str = cRes "#" SHARP str
    | otherwise = error $ "Unknown Tag String " ++ (show str) ++ " (Maybe This Must be Registered.)"
      where
        cRes :: String -> Tag -> String -> [(Tag, String)]
        cRes tok resTag str = [(resTag, P.drop (P.length tok) str)]

isEqToken query target
 | lenOfTarget < lenOfQuery = False
 | isQuery = True
 | otherwise = False
   where
     lenOfQuery = length query
     lenOfTarget = length target
     isQuery = query == (take lenOfQuery target)

data Tag =
   ROOT
 | NP
 | ADJP
 | JJ
 | PP
 | TO
 | NNP
 | NNPS
 | CC
 | CD
 | COMMA --,
 | NN
 | S
 | NNS
 | VP
 | MD
 | RB
 | VB
 | VBN
 | IN
 | SBAR
 | PRP
 | ADVP
 | PRPDOLL --PRP$
 | DT
 | PERIOD -- .
 | VBZ
 | VBP
 | VBG
 | RBS
 | EX
 | RBR
 | COLON -- :
 | WHNP
 | WP
 | VBD
 | PRT
 | RP
 | FRAG
 | QP
 | X
 | SYM
 | INTJ
 | UH
 | SINV
 | FW
 | JJR
 | PRN
 | WDT
 | BQUOTE -- ``
 | QUOTE -- ''
 | WHADVP
 | WRB
 | WHPP
 | LRB -- -LRB-
 | RRB -- -RRB-
 | LST
 | LS
 | WPDOLL -- WP$
 | CONJP
 | POS
 | UCP
 | PDT
 | NX
 | JJS
 | DOLL -- $
 | SHARP -- #
 | SQ
 | SBARQ
 | RRC
 | WHADJP
 | NAC
 | TAGVOID -- for Nil SExp
  deriving (Eq, Ord, Generic, NFData)
