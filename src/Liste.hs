module Liste (skaste, mezofaliste, mezoseliste, mezopatcidu) where

import Data.Maybe

skaste :: [(String, (Int, Int, Int))]
skaste = [
	("xekri", (0, 0, 0)),
	("blabi", (255, 255, 255)),

	("xunre", (255, 0, 0)),
	("labri'o", (0, 255, 0)),	-- Lime
	("crinyblabi", (0, 255, 0)),	-- Lime
	("blanu", (0, 0, 255)),

	("pelxu", (255, 255, 0)),
	("cicna", (0, 255, 255)),
	("nukni", (255, 0, 255)),

	("rijyska", (192, 192, 192)),	-- Silver
	("grusi", (128, 128, 128)),

--	("?", (128, 0, 0)),		-- Maroon
	("alzaityska", (128, 128, 0)),	-- Olive
	("crino", (0, 128, 0)),
	("zirpu", (128, 0, 128)),	-- Purple
--	("?", (0, 128, 128)),		-- Teal
--	("?", (0, 0, 128)),		-- Navy

	("bunre", (165, 42, 42)),	-- Brown
	("narju", (255, 165, 0)),	-- Orange
	("sloska", (255, 215, 0)),	-- Gold
	("xunblabi", (255, 192, 203))	-- Pink
 ]

mezofaliste :: [(String, Int)]
mezofaliste = [
	("fa", 1),
	("fe", 2),
	("fi", 3),
	("fo", 4),
	("fu", 5)
 ]

mezopatcidu :: [String] -> Double
mezopatcidu ("ni'u" : pas) = - (pti $ reverse $ processKIhO pas)
mezopatcidu pas = pti $ reverse $ processKIhO pas

pti :: [String] -> Double
pti [] = 0
pti (p : rest) = fromJust (lookup' p mezopaliste) + 10 * pti rest

lookup' :: Eq a => a -> [([a], b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((xs, y) : ys)
	| x `elem` xs = Just y
	| otherwise = lookup' x ys

processKIhO :: [String] -> [String]
processKIhO = reverse . pk 3 . reverse
	where
	pk _ [] = []
	pk n ("ki'o" : rest) = replicate n "no" ++ pk 3 rest
	pk n (pa : rest)
		| n > 0 = pa : pk (n - 1) rest
		| otherwise = pa : pk 2 rest

mezopaliste :: [([String], Double)]
mezopaliste = [
	(["no", "0"], 0),
	(["pa", "1"], 1),
	(["re", "2"], 2),
	(["ci", "3"], 3),
	(["vo", "4"], 4),
	(["mu", "5"], 5),
	(["xa", "6"], 6),
	(["ze", "7"], 7),
	(["bi", "8"], 8),
	(["so", "9"], 9)
 ]

mezoseliste :: [(String, Int)]
mezoseliste = [
	("se", 2),
	("te", 3),
	("ve", 4),
	("xe", 5)
 ]
