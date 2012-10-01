module Language.Lojban.Read (
	Lojban(..),
	Selbri(..),
	Sumti(..),
	Tag(..),
	Mex(..),
	RelativeClause(..),
	readLojban
) where

import Language.Lojban.Parser hiding (
	Tag, Sumti, Selbri, KOhA, FA, Brivla, BAI, Number, LI, RelativeClause,
	Time, KU, Linkargs, FIhO)
import qualified Language.Lojban.Parser as P
import Data.Maybe
import Data.Char

data Lojban
	= Bridi Selbri [(Tag, Sumti)]
	| Vocative String
	| Free [Free]
	| ParseError String
	deriving (Show, Eq)

data Selbri
	= Brivla String
	| Linkargs Selbri Sumti
	deriving (Show, Eq)

data Sumti
	= KOhA String
	| LA (Either String Selbri)
	| LE Selbri
	| LO Selbri (Maybe RelativeClause)
	| LI Mex
	| KU
	| SFIhO Selbri Sumti
	deriving (Show, Eq)

data RelativeClause
	= POI Lojban
	| Debug String
	deriving (Show, Eq)

data Mex
	= Number Double
	deriving (Show, Eq)

data Tag
	= FA Int
	| FIhO Selbri
	| BAI String
	| Time [String]
	deriving (Show, Eq)

readLojban :: String -> Lojban
readLojban = either (ParseError . show) process . parse

process :: Text -> Lojban
process (TopText _ _ [VocativeSumti [(_, v, _)] _ _] Nothing Nothing Nothing) =
	Vocative v
process (IText_1 _ _ _ [VocativeSumti [(_, v, _)] _ _] Nothing) = Vocative v
process (IText_1 _ _ _ fs@(_ : _) Nothing) = Free fs
process (IText_1 _ _ _ _ (Just t)) = process t
process (TermsBridiTail ss _ _ (SelbriTailTerms selbri ts _ _)) =
	Bridi (readSelbri selbri) $ processTagSumti ss ts
process (TermsBridiTail ss _ _ (P.Selbri selbri)) =
	Bridi (readSelbri selbri) $ processTagSumti ss []
process (P.Selbri selbri) = Bridi (readSelbri selbri) []
process t = error $ "process: " ++ show t

processTagSumti :: [P.Sumti] -> [P.Sumti] -> [(Tag, Sumti)]
processTagSumti s t = let
	(n, e, r) = readTagSumti 1 [] [] s
	(_, _, r') = readTagSumti (if n > 1 then n else 2) e [] t in r ++ r'

readTagSumti :: Int -> [Int] -> [(Tag, Sumti)] -> [P.Sumti]
	-> (Int, [Int], [(Tag, Sumti)])
readTagSumti n e r [] = (n, e, reverse r)
readTagSumti n e r (TagSumti t s : rest) = case readTag t of
	(Just i, tag) -> readTagSumti (next i e) (i : e) ((tag, sumti) : r) rest
	(_, tag) -> readTagSumti n e ((tag, sumti) : r) rest
	where
	sumti = readSumti s
readTagSumti n e r (s : rest) =
	readTagSumti (next n e) (n : e) ((FA n, readSumti s) : r) rest

readTag :: P.Tag -> (Maybe Int, Tag)
readTag (P.FA (_, f, _) _) = let n = fromJust $ lookup f faList in (Just n, FA n)
readTag (P.BAI _ _ (_, b, _) _ _) = (Nothing, BAI b)
readTag (P.Time _ _ _ ts) = (Nothing, Time $ map readTime ts)
readTag t = error $ "readTag: " ++ show t

readTime ::  IntervalProperty -> String
readTime (ZAhO (_, z, _) _) = z
readTime ip = error $ "readTime: " ++ show ip

faList :: [(String, Int)]
faList = [
	("fa", 1),
	("fe", 2),
	("fi", 3),
	("fo", 4),
	("fu", 5)
 ]

readSumti :: P.Sumti -> Sumti
readSumti (P.KOhA (_, k, _) _) = KOhA k
readSumti (LALE (_, "lo", _) _ st _ _) = LO selbri relative
	where (selbri, relative) = readSumtiTail st
readSumti (P.LI _ _ m _ _) = LI $ readMex m
readSumti (P.KU _ _) = KU
readSumti (TagSumti (P.FIhO (_, "fi'o", _) _ selbri _ _) sumti) =
	SFIhO (readSelbri selbri) (readSumti sumti)
readSumti s = error $ "readSumti: " ++ show s

readMex :: Operand -> Mex
readMex (P.Number n _ _) = readNumber n
readMex o = error $ "readMex: " ++ show o

readNumber :: [Clause] -> Mex
readNumber = Number . paToInt . map (\(_, p, _) -> p)

processKIhO :: [String] -> [String]
processKIhO = reverse . pk 3 . reverse
	where
	pk _ [] = []
	pk n ("ki'o" : rest) = replicate n "no" ++ pk 3 rest
	pk n (pa : rest)
		| n > 0 = pa : pk (n - 1) rest
		| otherwise = pa : pk 2 rest

paToInt :: [String] -> Double
paToInt = pti . reverse . processKIhO
	where
	pti [] = 0
	pti (p : rest) = fromJust (lookup p paList) + 10 * pti rest

paList :: [(String, Double)]
paList = [
	("no", 0),
	("pa", 1),
	("re", 2),
	("ci", 3),
	("vo", 4),
	("mu", 5),
	("xa", 6),
	("ze", 7),
	("bi", 8),
	("so", 9)
 ]

readSumtiTail :: SumtiTail -> (Selbri, Maybe RelativeClause)
readSumtiTail (SelbriRelativeClauses s Nothing) =
	(readSelbri s, Nothing)
readSumtiTail (SelbriRelativeClauses s (Just r)) =
	(readSelbri s, Just $ readRelativeClauses r)
readSumtiTail st = error $ "readSumtiTail: " ++ show st

readRelativeClauses :: P.RelativeClause -> RelativeClause
readRelativeClauses (NOI (_, "poi", _) _ bridi _ _) =
	POI $ process bridi
readRelativeClauses r = Debug $  show r

readSelbri :: P.Selbri -> Selbri
readSelbri (P.Brivla (_, b, _) _) = Brivla $ map toLower b
readSelbri (P.Linkargs selbri (BE (_, "be", _) _ sumti _ _ _)) =
	Linkargs (readSelbri selbri) (readSumti sumti)
readSelbri s = error $ "readSelbri: " ++ show s
	
next :: Int -> [Int] -> Int
next n e
	| (n + 1) `elem` e = next (n + 1) e
	| otherwise = n + 1
