module Main where

import Graphics.UI.GLUT hiding (R)
import Graphics.UI.GLUT.Turtle
import Language.Lojban.Parser

import System.Environment

main :: IO ()
main = do
	prgName <- getProgName
	rawArgs <- getArgs
	_args <- initialize prgName rawArgs
	f <- openField
	t <- newTurtle f
	pencolor t ((255, 255, 255) :: (Int, Int, Int))
	fillcolor t ((255, 255, 255) :: (Int, Int, Int))
	shape t "turtle"
	shapesize t 3 3
	oninputtext f (processInput f t . readCommand . parse)
	mainLoop

data Command
	= CRAKLA Double | RIXYKLA Double
	| ZUNLE Double | PRITU Double
	| COhO | Unknown Text | ParseError
	deriving Show

readCommand :: Either ParseError Text -> Command
readCommand (Left _) = ParseError
readCommand (Right (IText_1 _ _ _ _ (Just t))) = readCommand $ Right t
readCommand (Right (IText_1 _ _ _ [VocativeSumti [(_, "co'o", _)] _ _] _)) =
	COhO
readCommand (Right (TopText _ _ [VocativeSumti [(_, "co'o", _)] _ _] _ _ _)) =
	COhO
readCommand (Right (TermsBridiTail [KOhA (_, "ko", _) _] _ _
	(Selbri (Brivla (_, "crakla", _) _)))) = CRAKLA 100
readCommand (Right (TermsBridiTail [KOhA (_, "ko", _) _] _ _
	(Selbri (Brivla (_, "rixykla", _) _)))) = RIXYKLA 100
readCommand (Right (TermsBridiTail [KOhA (_, "ko", _) _] _ _
	(SelbriTailTerms (Brivla (_, "crakla", _) _) [t] _ _))) =
	CRAKLA $ readLAhU t
readCommand (Right (TermsBridiTail [KOhA (_, "ko", _) _] _ _
	(SelbriTailTerms (Brivla (_, "rixykla", _) _) [t] _ _))) =
	RIXYKLA $ readLAhU t
readCommand (Right t@(TermsBridiTail [KOhA (_, "ko", _) _] _ _
	(SelbriTailTerms (Brivla (_, "carna", _) _) [f] _ _))) = case readLR f of
	L -> ZUNLE 90
	R -> PRITU 90
	_ -> Unknown t
readCommand (Right t@(TermsBridiTail [KOhA (_, "ko", _) _] _ _
	(SelbriTailTerms (Brivla (_, "carna", _) _) [f, l] _ _))) = case readLR f of
	L -> ZUNLE $ readLAhU l
	R -> PRITU $ readLAhU l
	_ -> Unknown t
readCommand (Right t) = Unknown t

data LR = L | R | BadLR deriving Show

readLR :: Sumti -> LR
readLR (TagSumti (FA (_, "fi", _) _) (LALE (_, "lo", _) _
	(SelbriRelativeClauses (Brivla (_, "zunle", _) _) _) _ _)) = L
readLR (TagSumti (FA (_, "fi", _) _) (LALE (_, "lo", _) _
	(SelbriRelativeClauses (Brivla (_, "pritu", _) _) _) _ _)) = R
readLR _ = BadLR

readLAhU :: Sumti -> Double
readLAhU (TagSumti (BAI _ _ (_, "la'u", _) _ _) (LI (_, "li", _) _
	(Number ns _ _) _ _)) = readNum ns

-- readNum :: [( -> Double
readNum = rn . reverse
	where
	rn [] = 0
	rn ((_, pa, _) : rest) = case lookup pa paNum of
		Just n -> n + 10 * rn rest
		Nothing -> 0

paNum = [
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

processInput :: Field -> Turtle -> Command -> IO Bool
processInput _ t (CRAKLA d) = forward t d >> return True
processInput _ t (RIXYKLA d) = backward t d >> return True
processInput _ t (ZUNLE d) = left t d >> return True
processInput _ t (PRITU d) = right t d >> return True
processInput _ _ COhO = return False
processInput f _ (Unknown t) = do
	outputString f ".i mi na jimpe"
	print $ show t
	return True
processInput f _ ParseError = do
	outputString f ".i di'u na drani fo lo gerna"
	return True
