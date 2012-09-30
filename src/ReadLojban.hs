module ReadLojban (
	readLojban
) where

import Language.Lojban.Parser hiding
	(Tag, Sumti, Selbri, KOhA, FA, Brivla, BAI, Number, LI)
import qualified Language.Lojban.Parser as P
import Data.Maybe

main = do
	let	ret = readLojban ".i ko carna fi lo zunle la'u li panono"
	print ret

data Lojban
	= Bridi Selbri [(Tag, Sumti)]
	| ParseError String
	deriving Show

data Selbri
	= Brivla String
	deriving Show

data Sumti
	= KOhA String
	| LA (Either String Selbri)
	| LE Selbri
	| LO Selbri
	| LI Mex
	deriving Show

data Mex
	= Number Int
	deriving Show

data Tag
	= FA Int
	| FIhO Selbri
	| BAI String
	deriving Show

readLojban :: String -> Lojban
readLojban = either (ParseError . show) process . parse

process :: Text -> Lojban
process (IText_1 _ _ _ _ (Just t)) = process t
process (TermsBridiTail ss _ _ (SelbriTailTerms selbri ts _ _)) =
	Bridi (readSelbri selbri) $ processTagSumti ss ts
process t = error $ show t

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
readTag _ = error "yet"

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
readSumti (LALE (_, "lo", _) _ st _ _) = LO $ readSumtiTail st
readSumti (P.LI _ _ m _ _) = LI $ readMex m

readMex :: Operand -> Mex
readMex (P.Number n _ _) = readNumber n

-- readNumber :: P.Number -> Mex
readNumber = Number . paToInt . map (\(_, p, _) -> p)

paToInt :: [String] -> Int
paToInt = pti . reverse
	where
	pti [] = 0
	pti (p : rest) = fromJust (lookup p paList) + 10 * pti rest

paList = [
	("no", 0),
	("pa", 1),
	("re", 2)
 ]

readSumtiTail :: SumtiTail -> Selbri
readSumtiTail (SelbriRelativeClauses s _) = readSelbri s

readSelbri :: P.Selbri -> Selbri
readSelbri (P.Brivla (_, b, _) _) = Brivla b
	
next :: Int -> [Int] -> Int
next n e
	| (n + 1) `elem` e = next (n + 1) e
	| otherwise = n + 1
