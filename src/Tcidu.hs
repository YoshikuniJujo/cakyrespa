module Tcidu(parse) where

import Control.Arrow(first)
import Data.Maybe(fromJust)
import Data.Char(isSpace, toLower)
import qualified Language.Lojban.Parser as P(
	parse, Text, Clause,
	Sentence(..), Gek(..), Free(..), IntervalProperty(..),
	Selbri(..), Linkargs(..),
	Tag(..), Sumti(..), SumtiTail(..), RelativeClause(..), Operand(..))
import Klesi(
	Text(..), Selbri(..), Tag(..), Sumti(..), Mex(..), RelativeClause(..))
import Liste(mezofaliste, mezoseliste, mezopaliste)

--------------------------------------------------------------------------------

parse :: String -> Text
parse = either (ParseError . show) (process 1) . P.parse

process :: Int -> P.Text -> Text
process n s = processSE $ case process' n s of
	Bridi selbri terms -> Bridi selbri (processCEhU n terms)
	r -> r

processSE :: Text -> Text
processSE (Bridi (NA (SE n selbri)) ss) = processSE $
	Bridi (NA selbri) $ map (first $ flipTag n) ss
processSE (Bridi (SE n selbri) ss) = processSE $
	Bridi selbri $ map (first $ flipTag n) ss
processSE l = l

pCEhU :: Sumti -> (Int, Int -> Sumti)
pCEhU CEhUPre = (1, CEhU)
pCEhU (LO s (Just (POI (Bridi (ME sumti) ss)))) = let (b, ce'u) = pCEhU sumti in
	(b, \n -> LO s $ Just $ POI $ Bridi (ME $ ce'u n) ss)
pCEhU (LO (BE selbri sumti) Nothing) = let (b, ce'u) = pCEhU sumti in
	(b, \n -> LO (BE selbri $ ce'u n) Nothing)
pCEhU (SFIhO modal sumti) = let (b, ce'u) = pCEhU sumti in
	(b, SFIhO modal . ce'u)
pCEhU (GOI sumti1 sumti2) = let
	(b1, ce'u1) = pCEhU sumti1
	(b2, ce'u2) = pCEhU sumti2 in
	(b1 + b2, \n -> GOI (ce'u1 n) (ce'u2 $ b1 + n))
pCEhU (LA (Right (ME sumti))) = let (b, ce'u) = pCEhU sumti in
	(b, LA . Right . ME . ce'u)
pCEhU (Relative sumti r) = let (b, ce'u) = pCEhU sumti in
	(b, \n -> Relative (ce'u n) r)
pCEhU (LAhE sumti) = let (b, ce'u) = pCEhU sumti in
	(b, LAhE . ce'u)
pCEhU sumti = (0, const sumti)

processCEhU :: Int -> [(Tag, Sumti)] -> [(Tag, Sumti)]
processCEhU _ [] = []
processCEhU n ((t, sumti) : rest) = let (b, ce'u) = pCEhU sumti in
	(t, ce'u n) : processCEhU (n + b) rest

countc :: Int -> P.Text -> Int
countc n = countCEhU 0 . (\(Bridi _ s) -> s) . process' n

countCEhU :: Int -> [(Tag, Sumti)] -> Int
countCEhU n [] = n
countCEhU n ((_, sumti) : rest) = let (b, _) = pCEhU sumti in
	countCEhU (n + b) rest

flipTag :: Int -> Tag -> Tag
flipTag n (FA f)
	| f == 1 = FA n
	| f == n = FA 1
flipTag _ t = t

process' :: Int -> P.Text -> Text
process' _ (P.TopText _ _ [P.VocativeSumti [(_, v, _)] _ _] Nothing Nothing Nothing) =
	Vocative v
process' _ (P.IText_1 _ _ _ [P.VocativeSumti [(_, v, _)] _ _] Nothing) = Vocative v
process' _ (P.IText_1 _ _ _ fs@(_ : _) Nothing) = Free fs
process' n (P.IText_1 _ _ _ _ (Just t)) = process n t
process' n (P.Prenex ss (_, "zo'u", _) _ t) = Prenex (map readSumti ss) $ process n t
process' n (P.PrenexSentence ss (_, "zo'u", _) _ t) =
	Prenex (map readSumti ss) $ process n t
process' _ (P.TermsBridiTail ss _ _ (P.SelbriTailTerms selbri ts _ _)) =
	Bridi (readSelbri selbri) $ processTagSumti ss ts
process' _ (P.TermsBridiTail ss _ _ (P.Selbri selbri)) =
	Bridi (readSelbri selbri) $ processTagSumti ss []
process' n (P.TermsBridiTail ss _ _
	(P.GekSentence
		(P.STagGik
			(P.Time _ [((_, tense, _), _, _)] _ _)
			((_, "gi", _), _, _))
		t ((_, "gi", _), _, _) u _ _ _)) =
	TagGI tense
		(processSE $ process n $ P.TermsBridiTail ss undefined undefined t)
		(processSE $ process (n +
			countc n (P.TermsBridiTail ss undefined undefined t)) $
			P.TermsBridiTail ss undefined undefined u)
process' n (P.GekSentence
	(P.STagGik
		(P.Time _ [((_, tense, _), _, _)] _ _)
		((_, "gi", _), _, _))
	t ((_, "gi", _), _, _) u _ _ _) =
	TagGI tense
		(processSE $ process n t)
		(processSE $ process n u)
process' _ (P.Selbri selbri) = Bridi (readSelbri selbri) []
process' _ (P.SelbriTailTerms selbri ts _ _) =
	Bridi (readSelbri selbri) $ processTagSumti [] ts
process' _ t = UnknownText $ "process: " ++ show t

processTagSumti :: [P.Sumti] -> [P.Sumti] -> [(Tag, Sumti)]
processTagSumti s t = let
	(n, e, r) = readTagSumti 1 [] [] s
	(_, _, r') = readTagSumti (if n > 1 then n else 2) e [] t in r ++ r'

readTagSumti :: Int -> [Int] -> [(Tag, Sumti)] -> [P.Sumti]
	-> (Int, [Int], [(Tag, Sumti)])
readTagSumti n e r [] = (n, e, reverse r)
readTagSumti n e r (P.TagSumti t s : rest) = case readTag t of
	(Just i, tag) -> readTagSumti (next i e) (i : e) ((tag, sumti) : r) rest
	(_, tag) -> readTagSumti n e ((tag, sumti) : r) rest
	where
	sumti = readSumti s
readTagSumti n e r (s : rest) =
	readTagSumti (next n e) (n : e) ((FA n, readSumti s) : r) rest

readTag :: P.Tag -> (Maybe Int, Tag)
readTag (P.FA (_, f, _) _) = let n = fromJust $ lookup f mezofaliste in (Just n, FA n)
readTag (P.BAI _ Nothing (_, b, _) _ _) = (Nothing, BAI 1 b)
readTag (P.BAI _ (Just (_, s, _)) (_, b, _) _ _) =
	(Nothing, BAI (fromJust $ lookup s mezoseliste) b)
readTag (P.Time _ [((_, t, _), Nothing, Nothing)] _ _) = (Nothing, Time [t])
readTag (P.Time _ _ _ ts) = (Nothing, Time $ map readTime ts)
readTag t = (Nothing, UnknownTag $ "readTag: " ++ show t)

readTime :: P.IntervalProperty -> String
readTime (P.ZAhO (_, z, _) _) = z
readTime ip = "readTime: " ++ show ip

readSumti :: P.Sumti -> Sumti
readSumti (P.KOhA (_, "ce'u", _) [P.XINumber (_, "xi", _) _ [([], pa, [])] _]) =
	CEhU $ round $ paToInt [pa]
readSumti (P.KOhA (_, "ce'u", _) []) = CEhUPre
readSumti s@(P.KOhA (_, "ce'u", _) f) =
	UnknownSumti $ show s ++ " " ++ show f
readSumti (P.KOhA (_, k, _) _) = KOhA k
readSumti (P.LALE (_, "lo", _) _ st _ _) = LO selbri relative
	where (selbri, relative) = readSumtiTail st
readSumti (P.LALE (_, "la", _) _ st _ _) = LA $ Right selbri
	where (selbri, _relative) = readSumtiTail st
readSumti (P.LA (_, "la", _) _ _ [(_, n, _)] _) = LA $ Left n
readSumti (P.LI _ _ m _ _) = LI $ readMex m
readSumti (P.KU _ _) = KU
readSumti (P.TagSumti (P.FIhO (_, "fi'o", _) _ selbri _ _) sumti) =
	SFIhO (readSelbri selbri) (readSumti sumti)
readSumti (P.ZOI _ "zoi" ws _ _) = ZOI $ processZOI $ concat ws
readSumti (P.LAhE_NAhE (_, "tu'a", _) _ _ _ s _ _) = TUhA $ readSumti s
readSumti (P.OuterQuantifier _ s (Just (P.GOI (_, "goi", _) _ t _ _))) =
	GOI (readSumti s) (readSumti t)
readSumti (P.OuterQuantifier _ s (Just r)) =
	Relative (readSumti s) (readRelativeClauses r)
readSumti (P.LerfuString ls _ _) = LerfuString $ concatMap (\(_, s, _) -> s) ls
readSumti (P.GekSumti
	(P.STagGik (P.Time _ [((_, pu, _), _, _)] _ _) ((_, "gi", _), _, _))
	s ((_, "gi", _), _, _) t) = STense pu (readSumti s) (readSumti t)
readSumti (P.LAhE_NAhE (_, "la'e", _) _ _ _ s _ _) = LAhE $ readSumti s
readSumti s = UnknownSumti $ "readSumti: " ++ show s

processZOI :: String -> String
processZOI ('.' : str) = let ('.' : s) = reverse str in
	dropWhile isSpace $ reverse $ dropWhile isSpace s
processZOI str = let ('.' : s) = reverse str in
	dropWhile isSpace $ reverse $ dropWhile isSpace s

readMex :: P.Operand -> Mex
readMex (P.Number n _ _) = readNumber n
readMex (P.JOhI (_, "jo'i", _) _ ns _ _) = JOhI $ map readMex ns
readMex o = UnknownMex $ "readMex: " ++ show o

readNumber :: [P.Clause] -> Mex
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
paToInt ("ni'u" : pas) = - (pti $ reverse $ processKIhO pas)
paToInt pas = pti $ reverse $ processKIhO pas

pti :: [String] -> Double
pti [] = 0
pti (p : rest) = fromJust (lookup' p mezopaliste) + 10 * pti rest

lookup' :: Eq a => a -> [([a], b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((xs, y) : ys)
	| x `elem` xs = Just y
	| otherwise = lookup' x ys

readSumtiTail :: P.SumtiTail -> (Selbri, Maybe RelativeClause)
readSumtiTail (P.SelbriRelativeClauses s Nothing) =
	(readSelbri s, Nothing)
readSumtiTail (P.SelbriRelativeClauses s (Just r)) =
	(readSelbri s, Just $ readRelativeClauses r)
readSumtiTail st = (UnknownSelbri $ "readSumtiTail: " ++ show st, Nothing)

readRelativeClauses :: P.RelativeClause -> RelativeClause
readRelativeClauses (P.NOI (_, "poi", _) _ bridi _ _) =
	POI $ process 1 bridi
readRelativeClauses r = UnknownRelativeClause $  show r

readSelbri :: P.Selbri -> Selbri
readSelbri (P.Brivla (_, b, _) _) = Brivla $ map toLower b
readSelbri (P.Linkargs selbri (P.BE (_, "be", _) _ sumti _ _ _)) =
	BE (readSelbri selbri) (readSumti sumti)
readSelbri (P.NU (_, "nu", _) _ _ _ bridi _ _) = NU $ process 1 bridi
readSelbri (P.NU (_, "du'u", _) _ _ _ bridi _ _) = DUhU $ process 1 bridi
readSelbri (P.NA (_, "na", _) _ s) = NA $ readSelbri s
readSelbri (P.ME (_, "me", _) _ s _ _ _ _) = ME $ readSumti s
readSelbri (P.SE (_, se, _) _ s) = SE (fromJust $ lookup se mezoseliste) $ readSelbri s
readSelbri s = UnknownSelbri $ "readSelbri: " ++ show s
	
next :: Int -> [Int] -> Int
next n e
	| (n + 1) `elem` e = next (n + 1) e
	| otherwise = n + 1
