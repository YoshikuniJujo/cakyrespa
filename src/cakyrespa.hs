{-# LANGUAGE PatternGuards #-}

module Main where

import Text.XML.YJSVG(showSVG)
import Graphics.UI.GLUT(initialize, mainLoop)
import Graphics.UI.GLUT.Turtle(
	Field, openField, prompt, outputString, oninputtext,
	Turtle, newTurtle, runInputs, inputs, getSVG,
	goto, forward, backward, left, right, beginfill, endfill, undo, notundo,
	penup, pendown, hideturtle, showturtle, shape, shapesize,
	pensize, pencolor, fillcolor, bgcolor, windowWidth, windowHeight)
import Language.Lojban.Read(
	readLojban,
	Lojban(..), Selbri(..), Tag(..), Sumti(..), RelativeClause(..), Mex(..))

import System.Environment(getProgName, getArgs)
import System.IO.Unsafe(unsafePerformIO)
import Control.Applicative((<$>))
import Control.Monad(when, replicateM_)
import Data.Maybe(fromMaybe)
import Data.IORef(IORef, newIORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)

--------------------------------------------------------------------------------

main :: IO ()
main = do
	prgName <- getProgName
	rawArgs <- getArgs
	_args <- initialize prgName rawArgs
	f <- openField "cakyrespa" 640 480
	t <- newTurtle f
	prompt f ".i "
	shape t "turtle"
	shapesize t 3 3
	notundo t
	oninputtext f $ run f t . flip command [] . readLojban
	mainLoop

command :: Lojban -> [Argument] -> Command
command (Vocative "co'o") _ = COhO
command (TenseGI "ba" b c) args = Commands (command b args) (command c args)
command (Prenex ss b) _ = CommandList $ command b <$> mapM sumtiToArgument ss
command b@(Bridi (Brivla brivla) s) args = fromMaybe (Unknown b) $ case brivla of
	"gasnu" -> readGasnu s args
	"morji" -> readMorji s
	"klama" -> readKlama s
	"galfi" -> readGalfi s
	"tcidu" -> readTcidu s
	"rejgau" -> readRejgau s
	"viska" -> readViska s
	"cisni" -> readCisni s
	"xruti" -> readXruti s
	"rapli" -> readRapli s
	"carna" -> carna s args
	"crakla" -> crakla s args
	"rixykla" -> readRixykla s
	"pilno" -> pilno s args
	"clugau" -> readClugau s
	_ -> Nothing
command b@(Bridi (NA (Brivla brivla)) s) _args = fromMaybe (Unknown b) $ case brivla of
	"viska" -> readNAViska s
	"pilno" -> readNAPilno s
	_ -> Nothing
command l _ = Unknown l

updateReaders :: (Lojban -> Maybe Command) -> Lojban -> Maybe ([Argument] -> Command)
updateReaders reader text = do
	c <- reader text
	return $ const c

readGasnu :: [(Tag, Sumti)] -> [Argument] -> Maybe Command
readGasnu s a = do
	KOhA "ko" <- lookup (FA 1) s
	LerfuString cmene <- lookup (FA 2) s
	return $ GASNU cmene a

readMorji, readKlama, readGalfi, readTcidu, readRejgau ::
	[(Tag, Sumti)] -> Maybe Command
readMorji s = do
	KOhA "ko" <- lookup (FA 1) s
	GOI (LerfuString cmene) (LO (DUhU fasnu) _) <- lookup (FA 2) s
	return $ MORJI cmene $ command fasnu

readKlama s = do
	KOhA "ko" <- lookup (FA 1) s
	LI (JOhI [Number x, Number y]) <- lookup (FA 2 ) s
	return $ KLAMA x y

readGalfi s = do
	TUhA (KOhA "ko") <- lookup (FA 1) s
	LO (Brivla "foldi") Nothing <- lookup (FA 2) s
	LO (Brivla skari) Nothing <- lookup (FA 3) s
	clr <- lookup skari skaste
	return $ let (r, g, b) = clr in FLOSKA r g b

readTcidu s = do
	KOhA "ko" <- lookup (FA 1) s
	LA (Right (ME (ZOI fp))) <- lookup (FA 2) s
	return $ READFILE fp

readRejgau s = do
	KOhA "ko" <- lookup (FA 1) s
	ZOI fp <- lookup (BAI Nothing "me'e") s
	tai <- lookup (BAI (Just "se") "tai") s
	case tai of
		LA (Right (Brivla "cakyrespa")) -> return $ SAVEASCAK fp
		LA (Left "syvygyd") -> return $ SAVEASSVG fp
		_ -> error $ show tai

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
	return XRUTI

readRapli :: [(Tag, Sumti)] -> Maybe Command
readRapli s = do
	LO (NU p) _ <- lookup (FA 1) s
	LI (Number n) <- lookup (FA 2) s
	return $ Repeat (round n) (command p [])

readClugau :: [(Tag, Sumti)] -> Maybe Command
readClugau s = do
	KOhA "ko" <- lookup (FA 1) s
	case ((Time ["co'a"], KU) `elem` s, (Time ["co'u"], KU) `elem` s) of
		(True, False) -> return COhACLUGAU
		(False, True) -> return COhUCLUGAU
		_ -> fail "bad"

pilno :: ReadCommand
pilno terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	binxo <- selpli args =<< lookup (FA 2) terms
	return $ if (Time ["ba"], KU) `elem` terms
		then binxo
		else Commands binxo PILNOLOPENBI

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

selpli :: [Argument] -> Sumti -> Maybe Command
selpli _ (LO (Brivla "penbi") (Just (POI (Bridi (Brivla "cisni") [
	(FA 1, LI (Number size))])))) = return $ PEBYCISNI size
selpli _ (LO (Linkargs (Brivla "penbi") (SFIhO (Brivla "cisni") (LI
	(Number size)))) _) = return $ PEBYCISNI size
selpli _ (LO (Brivla "penbi") (Just (POI (Bridi (Brivla skari) [])))) =
	uncurry3 PEBYSKA <$> lookup skari skaste
selpli args (LO (Brivla "penbi") (Just (POI (Bridi (Brivla "cisni") [
	(FA 1, CEhU i)])))) = return $ applyDouble PEBYCISNI args i
selpli _ (LO (Linkargs (Brivla "penbi") (LO (Brivla skari) _)) _) =
	uncurry3 PEBYSKA <$> lookup skari skaste
selpli args (LO (Brivla "penbi") (Just (POI (Bridi (ME me) [])))) = case me of
	LO (Brivla skari) _ -> uncurry3 PEBYSKA <$> lookup skari skaste
	CEhU i -> return $ applyLO
		(maybe (ErrorC "selpli: no such skari")
			(uncurry3 PEBYSKA) . flip lookup skaste) args i
	_ -> return $ ErrorC $ "bad penbi: " ++ show me
selpli _ (LO (Brivla "penbi") Nothing) = return KUNTI

selpli _ (LO (Brivla "burcu") (Just (POI (Bridi (Brivla skari) [])))) =
	uncurry3 PEBYSKA <$> lookup skari skaste
selpli _ (LO (Linkargs (Brivla "burcu") (SFIhO (Brivla "skari") (LO
	(Brivla skari) _))) _) = uncurry3 PEBYSKA <$> lookup skari skaste

selpli _ p = return $ UnknownSelpli p

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

lahu :: [(Tag, Sumti)] -> Maybe (Either Double Int)
lahu s = do
	l <- lookup (BAI Nothing "la'u") s
	case l of
		LI (Number d) -> return $ Left d
		CEhU i -> return $ Right i
		_ -> fail "bad"

type ReadCommand = [(Tag, Sumti)] -> [Argument] -> Maybe Command

crakla :: ReadCommand
crakla terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	return $ maybe (CRAKLA 100)
		(either CRAKLA $ applyDouble CRAKLA args) $ lahu terms

readRixykla :: [(Tag, Sumti)] -> Maybe Command
readRixykla s = do
	KOhA "ko" <- lookup (FA 1) s
	return $ RIXYKLA $ maybe 100 (\(Left d) -> d) $ lahu s

carna :: ReadCommand
carna terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	farna <- case lookup (FA 3) terms of
		Just (LO (Brivla zp) _) -> Just zp
		Nothing -> Just "zunle"
		_ -> Nothing
	let	jganu = fromMaybe (Left 90) $ lahu terms
	case farna of
		"zunle" -> return $ either ZUNLE (applyDouble ZUNLE args) jganu
		"pritu" -> return $ either PRITU (applyDouble PRITU args) jganu
		_ -> fail "bad"

getDouble :: [Argument] -> Int -> Maybe Double
getDouble args i = do
	when (length args < i) $ fail "bad"
	ADouble d <- return $ args !! (i - 1)
	return d

applyDouble :: (Double -> Command) -> [Argument] -> Int -> Command
applyDouble cmd args i
	| length args >= i, ADouble d <- args !! (i - 1) = cmd d
	| otherwise = ErrorC "applyDouble: bad arguments"

applyLO :: (String -> Command) -> [Argument] -> Int -> Command
applyLO cmd args i
	| length args >= i, ALO s <- args !! (i - 1) = cmd s
	| otherwise = ErrorC "applyLO: bad arguments"

getALO :: [Argument] -> Int -> Maybe [String]
getALO args i = do
	when (length args < i) $ fail "bad"
	ALO s <- return $ args !! (i - 1)
	return [s]

data Argument
	= ADouble Double
	| AInt Int
	| ACommand Command
	| ALO String
	| ACons Argument Argument
--	deriving Show

sumtiToArgument :: Sumti -> [Argument]
sumtiToArgument (LI (Number n)) = [ADouble n]
sumtiToArgument (LO (Brivla s) _) = [ALO s]
sumtiToArgument (STense "ba" s_ t_) =
	sumtiToArgument s_ ++
	sumtiToArgument t_
sumtiToArgument s = error $ "sumtiToArgument :" ++ show s

data Command
	= KLAMA Double Double
	| CRAKLA Double | RIXYKLA Double
	| ZUNLE Double | PRITU Double
	| PEBYCISNI Double
	| PEBYSKA Int Int Int
	| BURSKA Int Int Int
	| FLOSKA Int Int Int
	| COhACLUGAU
	| COhUCLUGAU
	| Commands Command Command
	| CommandList [Command]
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
	| MORJI String ([Argument] -> Command)
	| GASNU String [Argument]
	| KUNTI
	| COhO | Unknown Lojban | ParseErrorC | UnknownSelpli Sumti
	| ErrorC String
--	deriving Show

type Table = [(String, [Argument] -> Command)]
theTable :: IORef Table
theTable = unsafePerformIO $ newIORef []
morji :: String -> ([Argument] -> Command) -> IO ()
morji cmene fasnu = atomicModifyIORef_ theTable ((cmene, fasnu) :)
tcidu :: String -> IO (Maybe ([Argument] -> Command))
tcidu cmene = lookup cmene <$> readIORef theTable

data LR = L | R | BadLR deriving Show

run :: Field -> Turtle -> Command -> IO Bool
run _ t (KLAMA x y) = goto t x y >> return True
run _ t (CRAKLA d) = forward t d >> return True
run _ t (RIXYKLA d) = backward t d >> return True
run _ t (ZUNLE d) = left t d >> return True
run _ t (PRITU d) = right t d >> return True
run _ t (PEBYCISNI s) = pensize t s >> return True
run _ t (PEBYSKA r g b) = pencolor t (r, g, b) >> return True
run _ t (BURSKA r g b) = fillcolor t (r, g, b) >> return True
run _ t (FLOSKA r g b) = bgcolor t (r, g, b) >> return True
run _ t COhACLUGAU = beginfill t >> return True
run _ t COhUCLUGAU = endfill t >> return True
run _ t XRUTI = undo t >> return True
run _ t (CISNI s) = shapesize t s s >> return True
run _ t NAPILNOLOPENBI = penup t >> return True
run _ t PILNOLOPENBI = pendown t >> return True
run _ t NAVISKA = hideturtle t >> return True
run _ t VISKA = showturtle t >> return True
run _ _ COhO = return False
run f t (Commands c d) = run f t c >> run f t d
run f t (CommandList cl) =
	mapM_ (run f t) cl >> return True
run f t (Repeat n c) = replicateM_ n (run f t c) >>
	return True
run _ _ (MORJI cmene fasnu) = morji cmene fasnu >> return True
run f t (GASNU cmene sumti) = do
	mfasnu <- tcidu cmene
	flip (maybe $ run f t $ ErrorC $ "not defined: " ++ cmene) mfasnu $
		\fasnu -> run f t $ fasnu sumti
run _ t (SAVEASSVG fp) = do
	w <- windowWidth t
	h <- windowHeight t
	svg <- getSVG t
	writeFile fp $ showSVG w h svg
	return True
run _ t (SAVEASCAK fp) = inputs t >>= writeFile fp . show >> return True
run _ t (READFILE fp) = readFile fp >>= runInputs t . read >> return True
run _ _ KUNTI = return True
run f _ (Unknown u) = do
	outputString f ".i mi na jimpe"
	putStr "Unknown " >> print u
	return True
run f _ (UnknownSelpli us) = do
	outputString f ".i mi na djuno lo bi'unai selpli"
	putStr "Unknown " >> print us
	return True
run f _ ParseErrorC = do
	outputString f ".i di'u na drani fo lo gerna"
	return True
run _ _ (ErrorC str) = do
	putStrLn $ "error: " ++ str
	return True
