module Main where

import Graphics.UI.GLUT hiding (R, Repeat)
import Graphics.UI.GLUT.Turtle
import Text.XML.YJSVG

import Language.Lojban.Read

import System.Environment
import Data.Maybe

main :: IO ()
main = do
	prgName <- getProgName
	rawArgs <- getArgs
	_args <- initialize prgName rawArgs
	f <- openField "cakyrespa" 640 480
	prompt f ".i "
	t <- newTurtle f
--	pencolor t ((255, 255, 255) :: (Int, Int, Int))
--	fillcolor t ((255, 255, 255) :: (Int, Int, Int))
	shape t "turtle"
	shapesize t 3 3
	oninputtext f (processInput f t . cmd)
	mainLoop

cmd :: String -> Command
cmd = pcmd . readLojban

pcmd p = case p of
	(Vocative "co'o") -> COhO
	(TenseGI "ba" b c) -> Commands (pcmd b) (pcmd c)
	b@(Bridi (Brivla "galfi") s) -> fromMaybe (Unknown b) $ readGalfi s
	b@(Bridi (Brivla "tcidu") s) -> fromMaybe (Unknown b) $ readTcidu s
	b@(Bridi (Brivla "rejgau") s) -> fromMaybe (Unknown b) $ readRejgau s
	b@(Bridi (Brivla "viska") s) -> fromMaybe (Unknown b) $ readViska s
	b@(Bridi (NA (Brivla "viska")) s) -> fromMaybe (Unknown b) $ readNAViska s
	b@(Bridi (Brivla "cisni") s) -> fromMaybe (Unknown b) $ readCisni s
	b@(Bridi (Brivla "xruti") s) -> fromMaybe (Unknown b) $ readXruti s
	b@(Bridi (Brivla "rapli") s) -> fromMaybe (Unknown b) $ readRapli s
	b@(Bridi (Brivla "carna") s) -> fromMaybe (Unknown b) $ readCarna s
	b@(Bridi (Brivla "crakla") s) -> fromMaybe (Unknown b) $ readCrakla s
	b@(Bridi (Brivla "rixykla") s) -> fromMaybe (Unknown b) $ readRixykla s
	b@(Bridi (Brivla "pilno") s) ->
		fromMaybe (Unknown b) $ readBAPilno s
	b@(Bridi (NA (Brivla "pilno")) s) -> fromMaybe (Unknown b) $ readNAPilno s
	b@(Bridi (Brivla "clugau") s) -> fromMaybe (Unknown b) $ readClugau s
	r -> Unknown r

readGalfi s = do
	TUhA (KOhA "ko") <- lookup (FA 1) s
	LO (Brivla "foldi") Nothing <- lookup (FA 2) s
	LO (Brivla skari) Nothing <- lookup (FA 3) s
	clr <- lookup skari skaste
	return $ let (r, g, b) = clr in FLOSKA r g b

readTcidu s = do
	KOhA "ko" <- lookup (FA 1) s
	LA (Right (ME (ZOI fp))) <- lookup (FA 3) s
	return $ READFILE fp

readRejgau s = do
	KOhA "ko" <- lookup (FA 1) s
	ZOI fp <- lookup (BAI "me'e") s
	tai <- lookup (BAI "tai") s
	case tai of
		LA (Right (Brivla "cakyrespa")) -> return $ SAVEASCAK fp
		LA (Left "syvygyd") -> return $ SAVEASSVG fp
		s -> error $ show s

readViska :: [(Tag, Sumti)] -> Maybe Command
readViska s = do
	KOhA "ko" <- lookup (FA 2) s
	return VISKA

readNAViska :: [(Tag, Sumti)] -> Maybe Command
readNAViska s = do
	KOhA "ko" <- lookup (FA 2) s
	return NAVISKA

readNAPilno :: [(Tag, Sumti)] -> Maybe Command
readNAPilno s = do
	KOhA "ko" <- lookup (FA 1) s
	return NAPILNOLOPENBI

readCisni :: [(Tag, Sumti)] -> Maybe Command
readCisni s = do
	LI (Number n) <- lookup (FA 1) s
	KOhA "ko" <- lookup (FA 2) s
	return $ CISNI n

readXruti :: [(Tag, Sumti)] -> Maybe Command
readXruti s = do
	KOhA "ko" <- lookup (FA 1) s
	return $ XRUTI

readRapli :: [(Tag, Sumti)] -> Maybe Command
readRapli s = do
	LO (NU p) _ <- lookup (FA 1) s
	LI (Number n) <- lookup (FA 2) s
	return $ Repeat (round n) (pcmd p)

readClugau :: [(Tag, Sumti)] -> Maybe Command
readClugau s = do
	KOhA "ko" <- lookup (FA 1) s
	case ((Time ["co'a"], KU) `elem` s, (Time ["co'u"], KU) `elem` s) of
		(True, False) -> return COhACLUGAU
		(False, True) -> return COhUCLUGAU
		_ -> fail "bad"

readBAPilno :: [(Tag, Sumti)] -> Maybe Command
readBAPilno s = do
	cm <- readPilno s
	if (Time ["ba"], KU) `elem` s then return cm
		else return $ Commands cm PILNOLOPENBI

readPilno :: [(Tag, Sumti)] -> Maybe Command
readPilno s = do
	KOhA "ko" <- lookup (FA 1) s
	selpli <- lookup (FA 2) s
	case selpli of
		LO (Brivla "penbi") (Just (POI (Bridi (Brivla "cisni") [
			(FA 1, LI (Number size))]))) -> return $ PEBYCISNI size
		LO (Linkargs (Brivla "penbi") (SFIhO (Brivla "cisni") (LI
			(Number size)))) _ -> return $ PEBYCISNI size
		LO (Brivla "penbi") (Just (POI (Bridi (Brivla skari) []))) -> do
			(r, g, b) <- lookup skari skaste
			return $ PEBYSKA r g b
		LO (Linkargs (Brivla "penbi") (LO (Brivla skari) _)) _ -> do
			(r, g, b) <- lookup skari skaste
			return $ PEBYSKA r g b
		LO (Brivla "burcu") (Just (POI (Bridi (Brivla skari) []))) -> do
			(r, g, b) <- lookup skari skaste
			return $ BURSKA r g b
		LO (Linkargs (Brivla "burcu") (SFIhO (Brivla "skari") (LO
			(Brivla skari) _))) _ -> do
			(r, g, b) <- lookup skari skaste
			return $ BURSKA r g b
		LO (Brivla "penbi") Nothing -> return PILNOLOPENBI
		_ -> return $ UnknownSelpli selpli

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
	| PEBYCISNI Double
	| PEBYSKA Int Int Int
	| BURSKA Int Int Int
	| FLOSKA Int Int Int
	| COhACLUGAU
	| COhUCLUGAU
	| Commands Command Command
	| Repeat Int Command
	| XRUTI
	| CISNI Double
	| NAPILNOLOPENBI
	| PILNOLOPENBI
	| NAVISKA
	| VISKA
	| SAVEASSVG FilePath
	| SAVEASCAK FilePath
	| READFILE FilePath
	| COhO | Unknown Lojban | ParseErrorC
	| UnknownSelpli Sumti
	deriving Show

data LR = L | R | BadLR deriving Show

processInput :: Field -> Turtle -> Command -> IO Bool
processInput _ t (CRAKLA d) = forward t d >> return True
processInput _ t (RIXYKLA d) = backward t d >> return True
processInput _ t (ZUNLE d) = left t d >> return True
processInput _ t (PRITU d) = right t d >> return True
processInput _ t (PEBYCISNI s) = pensize t s >> return True
processInput _ t (PEBYSKA r g b) = pencolor t (r, g, b) >> return True
processInput _ t (BURSKA r g b) = fillcolor t (r, g, b) >> return True
processInput _ t (FLOSKA r g b) = bgcolor t (r, g, b) >> return True
processInput _ t COhACLUGAU = beginfill t >> return True
processInput _ t COhUCLUGAU = endfill t >> return True
processInput _ t XRUTI = undo t >> return True
processInput _ t (CISNI s) = shapesize t s s >> return True
processInput _ t NAPILNOLOPENBI = penup t >> return True
processInput _ t PILNOLOPENBI = pendown t >> return True
processInput _ t NAVISKA = hideturtle t >> return True
processInput _ t VISKA = showturtle t >> return True
processInput _ _ COhO = return False
processInput f t (Commands c d) = processInput f t c >> processInput f t d
processInput f t (Repeat n c) = sequence_ (replicate n (processInput f t c)) >>
	return True
processInput _ t (SAVEASSVG fp) = do
	w <- windowWidth t
	h <- windowHeight t
	svg <- getSVG t
	writeFile fp $ showSVG w h svg
	return True
processInput _ t (SAVEASCAK fp) = inputs t >>= writeFile fp . show >> return True
processInput _ t (READFILE fp) = readFile fp >>= runInputs t . read >> return True
processInput f _ u@(Unknown _) = do
	outputString f ".i mi na jimpe"
	print u
	return True
processInput f _ u@(UnknownSelpli _) = do
	outputString f ".i mi na djuno lo bi'unai selpli"
	print u
	return True
processInput f _ ParseErrorC = do
	outputString f ".i di'u na drani fo lo gerna"
	return True
