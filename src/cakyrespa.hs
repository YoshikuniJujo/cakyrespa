module Main where

import Graphics.UI.GLUT hiding (R)
import Graphics.UI.GLUT.Turtle

import Language.Lojban.Read

import System.Environment
import Data.Maybe

main :: IO ()
main = do
	prgName <- getProgName
	rawArgs <- getArgs
	_args <- initialize prgName rawArgs
	f <- openField "cakyrespa" 640 480
	t <- newTurtle f
	pencolor t ((255, 255, 255) :: (Int, Int, Int))
	fillcolor t ((255, 255, 255) :: (Int, Int, Int))
	shape t "turtle"
	shapesize t 3 3
	oninputtext f (processInput f t . cmd)
	mainLoop

cmd :: String -> Command
cmd str = case readLojban str of
	(Vocative "co'o") -> COhO
	b@(Bridi (Brivla "carna") s) -> fromMaybe (Unknown b) $ readCarna s
	b@(Bridi (Brivla "crakla") s) -> fromMaybe (Unknown b) $ readCrakla s
	b@(Bridi (Brivla "rixykla") s) -> fromMaybe (Unknown b) $ readRixykla s
	b@(Bridi (Brivla "pilno") s) -> fromMaybe (Unknown b) $ readPilno s
	r -> Unknown r

readPilno :: [(Tag, Sumti)] -> Maybe Command
readPilno s = do
	KOhA "ko" <- lookup (FA 1) s
	LO (Brivla "penbi") (Just (POI (Brivla skari))) <- lookup (FA 2) s
	(r, g, b) <- lookup skari skaste
	return $ PEBYSKA r g b

skaste :: [(String, (Int, Int, Int))]
skaste = [
	("xunre", (255, 0, 0))
 ]

readLAhU2 :: [(Tag, Sumti)] -> Maybe Double
readLAhU2 s = do
	LI (Number d) <- lookup (BAI "la'u") s
	return d

readCrakla :: [(Tag, Sumti)] -> Maybe Command
readCrakla s = do
	KOhA "ko" <- lookup (FA 1) s
	return $ CRAKLA $ fromMaybe 100 $ readLAhU2 s

readRixykla :: [(Tag, Sumti)] -> Maybe Command
readRixykla s = do
	KOhA "ko" <- lookup (FA 1) s
	return $ RIXYKLA $ fromMaybe 100 $ readLAhU2 s

readCarna :: [(Tag, Sumti)] -> Maybe Command
readCarna s = do
	KOhA "ko" <- lookup (FA 1) s
	let	LO (Brivla lr) _ =
			fromMaybe (LO (Brivla "zunle") Nothing) $ lookup (FA 3) s
	 	d = fromMaybe 90 $ readLAhU2 s
	case lr of
		"zunle" -> return $ ZUNLE d
		"pritu" -> return $ PRITU d
		_ -> fail "bad"

data Command
	= CRAKLA Double | RIXYKLA Double
	| ZUNLE Double | PRITU Double
	| PEBYSKA Int Int Int
	| COhO | Unknown Lojban | ParseErrorC
	deriving Show

data LR = L | R | BadLR deriving Show

processInput :: Field -> Turtle -> Command -> IO Bool
processInput _ t (CRAKLA d) = forward t d >> return True
processInput _ t (RIXYKLA d) = backward t d >> return True
processInput _ t (ZUNLE d) = left t d >> return True
processInput _ t (PRITU d) = right t d >> return True
processInput _ t (PEBYSKA r g b) = pencolor t (r, g, b) >> return True
processInput _ _ COhO = return False
processInput f _ u@(Unknown _) = do
	outputString f ".i mi na jimpe"
	putStrLn $ show u
	return True
processInput f _ ParseErrorC = do
	outputString f ".i di'u na drani fo lo gerna"
	return True
