module Tcidu(parse) where

import Control.Arrow(first, second)
import Data.Maybe(catMaybes)
import Data.Char(isSpace, toLower)
import qualified Language.Lojban.Parser as P(
	parse, Text, Clause,
	Sentence(..), Gek(..), Free(..), IntervalProperty(..),
	Selbri(..), Linkargs(..),
	Tag(..), Sumti(..), SumtiTail(..), RelativeClause(..), Operand(..))
import Klesi(
	Text(..), Selbri(..), Tag(..), Sumti(..), Mex(..), RelativeClause(..))
import Liste(mezofatcidu, mezosetcidu, mezopatcidu)

--------------------------------------------------------------------------------

parse :: String -> Text
parse = either (ParseError . show) (snd . process 1) . P.parse

process :: Int -> P.Text -> (Int, Text)
process n ptext = second flipText $ proc n ptext
	where
	flipText (Bridi (NA (SE i selbri)) terms) =
		flipText $ Bridi (NA selbri) $ flipTerms i terms
	flipText (Bridi (SE i selbri) terms) =
		flipText $ Bridi selbri $ flipTerms i terms
	flipText text = text
	flipTerms i = map $ first flipTag
		where
		flipTag (FA f)
			| f == 1 = FA i
			| f == i = FA 1
		flipTag t = t

some :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
some _ x0 [] = (x0, [])
some f x0 (y : ys) = let
	(x1, z) = f x0 y
	(x', zs) = some f x1 ys in
	(x', z : zs)

proc :: Int -> P.Text -> (Int, Text)
proc n (P.TopText [] (Right Nothing) [] Nothing Nothing (Just ([], "fa'o"))) =
	(0, FAhO)
proc n (P.TopText _ _ _ _ (Just t) _) = proc n t
proc n (P.TopText _ _ [P.VocativeSumti [(_, v, _)] _ _] _ _ _) = (n, Vocative v)
proc n (P.TopText _ _ fs@(_ : _) _ _ _) = (n, Free fs)
proc n (P.IText_1 _ _ _ _ (Just t)) = proc n t
proc n (P.IText_1 _ _ _ [P.VocativeSumti [(_, v, _)] _ _] _) = (n, Vocative v)
proc n (P.IText_1 _ _ _ fs@(_ : _) _) = (n, Free fs)
proc n (P.StatementI ptext iptexts) =
	(0, MultiText $ map snd $ map (proc 1) $ ptext : ptexts)
	where
	ptexts = catMaybes $ map (\(_, _, t) -> t) iptexts
proc n (P.Selbri selbri) = (n, Bridi (readSelbri selbri) [])
proc n (P.SelbriTailTerms pselbri pterms _ _) = (n', Bridi selbri terms)
	where	selbri = readSelbri pselbri
		(n', terms) = processTagSumti n [] pterms
proc n (P.TermsBridiTail pterms _ _ (P.Selbri pselbri)) = (n', Bridi selbri terms)
	where	selbri = readSelbri pselbri
		(n', terms) = processTagSumti n pterms []
proc n (P.TermsBridiTail pterms1 _ _ (P.SelbriTailTerms pselbri pterms2 _ _)) =
	(n', Bridi selbri terms)
	where	selbri = readSelbri pselbri
		(n', terms) = processTagSumti n pterms1 pterms2
proc n (P.Prenex psumti (_, "zo'u", _) _ t) = (n', Prenex sumti ret)
	where	sumti = map readSumti psumti
		(n', ret) = process n t
proc n (P.PrenexSentence psumti (_, "zo'u", _) _ t) = (n', Prenex sumti ret)
	where	sumti = map readSumti psumti
		(n', ret) = process n t
proc n (P.TermsBridiTail ss _ _
	(P.GekSentence
		(P.STagGik
			(P.Time _ [((_, tense, _), _, _)] _ _)
			((_, "gi", _), _, _))
		t ((_, "gi", _), _, _) u _ _ _)) =
	(n2, TagGI tense ret1 ret2)
	where
	(n1, ret1) = process n $ P.TermsBridiTail ss undefined undefined t
	(n2, ret2) = process n1 $ P.TermsBridiTail ss undefined undefined u
proc n (P.GekSentence
	(P.STagGik
		(P.Time _ [((_, tense, _), _, _)] _ _)
		((_, "gi", _), _, _))
	t ((_, "gi", _), _, _) u _ _ _) =
	(n, TagGI tense (snd $ process n t) (snd $ process n u))
proc n t = (n, UnknownText $ "proc: " ++ show t)

processTagSumti :: Int -> [P.Sumti] -> [P.Sumti] -> (Int, [(Tag, Sumti)])
processTagSumti n = curry $ processCEhU n . uncurry pTagSumti
	where
	pTagSumti s t = let
		(n', e, r) = readTagSumti 1 [] [] s
		(_, _, r') = readTagSumti (if n' > 1 then n' else 2) e [] t in
		r ++ r'

processCEhU :: Int -> [(Tag, Sumti)] -> (Int, [(Tag, Sumti)])
processCEhU n [] = (n, [])
processCEhU n ((t, sumti) : rest) = (n', (t, ce'u n) : terms')
	where
	(b, ce'u) = pCEhU sumti
	(n', terms') = processCEhU (n + b) rest

pCEhU :: Sumti -> (Int, Int -> Sumti)
pCEhU CEhUPre = (1, CEhU)
pCEhU (LO selbri (Just (POI (Bridi (ME sumti) ss)))) = let (b, s) = pCEhU sumti in
	(b, \n -> LO selbri $ Just $ POI $ Bridi (ME $ s n) ss)
pCEhU (LO (BE selbri sumti) Nothing) = let (b, s) = pCEhU sumti in
	(b, \n -> LO (BE selbri $ s n) Nothing)
pCEhU (SFIhO modal sumti) =
	let (b, s) = pCEhU sumti in (b, SFIhO modal . s)
pCEhU (GOI sumti1 sumti2) =
	let (b1, s1) = pCEhU sumti1; (b2, s2) = pCEhU sumti2 in
		(b1 + b2, \n -> GOI (s1 n) (s2 $ b1 + n))
pCEhU (LA (Right (ME sumti))) =
	let (b, s) = pCEhU sumti in (b, LA . Right . ME . s)
pCEhU (Relative sumti r) = let (b, s) = pCEhU sumti in (b, flip Relative r . s)
pCEhU (LAhE sumti) = let (b, s) = pCEhU sumti in (b, LAhE . s)
pCEhU sumti = (0, const sumti)

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
readTag (P.FA (_, f, _) _) = (Just $ mezofatcidu f, FA $ mezofatcidu f)
readTag (P.BAI _ Nothing (_, b, _) _ _) = (Nothing, BAI 1 b)
readTag (P.BAI _ (Just (_, s, _)) (_, b, _) _ _) =
	(Nothing, BAI (mezosetcidu s) b)
readTag (P.Time _ [((_, t, _), Nothing, Nothing)] _ _) = (Nothing, Time [t])
readTag (P.Time _ _ _ ts) = (Nothing, Time $ map readTime ts)
readTag t = (Nothing, UnknownTag $ "readTag: " ++ show t)

readTime :: P.IntervalProperty -> String
readTime (P.ZAhO (_, z, _) _) = z
readTime ip = "readTime: " ++ show ip

readSumti :: P.Sumti -> Sumti
readSumti (P.KOhA (_, "ce'u", _) [P.XINumber (_, "xi", _) _ [([], pa, [])] _]) =
	CEhU $ round $ mezopatcidu [pa]
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
readNumber = Number . mezopatcidu . map (\(_, p, _) -> p)

readSumtiTail :: P.SumtiTail -> (Selbri, Maybe RelativeClause)
readSumtiTail (P.SelbriRelativeClauses s Nothing) =
	(readSelbri s, Nothing)
readSumtiTail (P.SelbriRelativeClauses s (Just r)) =
	(readSelbri s, Just $ readRelativeClauses r)
readSumtiTail st = (UnknownSelbri $ "readSumtiTail: " ++ show st, Nothing)

readRelativeClauses :: P.RelativeClause -> RelativeClause
readRelativeClauses (P.NOI (_, "poi", _) _ bridi _ _) =
	POI $ snd $ process 1 bridi
readRelativeClauses r = UnknownRelativeClause $  show r

readSelbri :: P.Selbri -> Selbri
readSelbri (P.Brivla (_, b, _) _) = Brivla $ map toLower b
readSelbri (P.Linkargs selbri (P.BE (_, "be", _) _ sumti _ _ _)) =
	BE (readSelbri selbri) (readSumti sumti)
readSelbri (P.NU (_, "nu", _) _ _ _ bridi _ _) = NU $ snd $ process 1 bridi
readSelbri (P.NU (_, "du'u", _) _ _ _ bridi _ _) = DUhU $ snd $ process 1 bridi
readSelbri (P.NA (_, "na", _) _ s) = NA $ readSelbri s
readSelbri (P.ME (_, "me", _) _ s _ _ _ _) = ME $ readSumti s
readSelbri (P.SE (_, se, _) _ s) = SE (mezosetcidu se) $ readSelbri s
readSelbri s = UnknownSelbri $ "readSelbri: " ++ show s
	
next :: Int -> [Int] -> Int
next n e
	| (n + 1) `elem` e = next (n + 1) e
	| otherwise = n + 1
