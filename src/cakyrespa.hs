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

command :: Lojban -> [Sumti] -> Command
command (Vocative "co'o") _ = COhO
command (TenseGI "ba" b c) args = Commands (command b args) (command c args)
command (Prenex ss b) _ = CommandList $ command b <$> mapM sumtiToArgument ss
command b@(Bridi (Brivla brivla) s) args = fromMaybe (Unknown b) $ case brivla of
	"gasnu" -> gasnu s args
	"morji" -> morji s args
	"klama" -> klama s
	"galfi" -> galfi s
	"tcidu" -> tcidu s
	"rejgau" -> rejgau s
	"viska" -> viska s
	"cisni" -> cisni s
	"xruti" -> xruti s
	"rapli" -> rapli s
	"carna" -> carna s args
	"crakla" -> crakla s args
	"rixykla" -> rixykla s args
	"pilno" -> pilno s args
	"clugau" -> clugau s
	_ -> Nothing
command b@(Bridi (NA (Brivla brivla)) s) _args = fromMaybe (Unknown b) $ case brivla of
	"viska" -> naViska s
	"pilno" -> naPilno s
	_ -> Nothing
command l _ = Unknown l

updateReaders :: (Lojban -> Maybe Command) -> Lojban -> Maybe ([Argument] -> Command)
updateReaders reader text = do
	c <- reader text
	return $ const c

gasnu :: ReadCommand
gasnu s a = do
	KOhA "ko" <- lookup (FA 1) s
	LerfuString cmene <- lookup (FA 2) s
	return $ GASNU cmene a

klama, galfi, tcidu, rejgau ::
	[(Tag, Sumti)] -> Maybe Command

morji :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
morji s _ = do
	KOhA "ko" <- lookup (FA 1) s
	GOI (LerfuString cmene) (LO (DUhU fasnu) _) <- lookup (FA 2) s
	return $ MORJI cmene $ command fasnu

klama s = do
	KOhA "ko" <- lookup (FA 1) s
	LI (JOhI [Number x, Number y]) <- lookup (FA 2 ) s
	return $ KLAMA x y

galfi s = do
	TUhA (KOhA "ko") <- lookup (FA 1) s
	LO (Brivla "foldi") Nothing <- lookup (FA 2) s
	LO (Brivla skari) Nothing <- lookup (FA 3) s
	clr <- lookup skari skaste
	return $ let (r, g, b) = clr in FLOSKA r g b

tcidu s = do
	KOhA "ko" <- lookup (FA 1) s
	LA (Right (ME (ZOI fp))) <- lookup (FA 2) s
	return $ READFILE fp

rejgau s = do
	KOhA "ko" <- lookup (FA 1) s
	ZOI fp <- lookup (BAI Nothing "me'e") s
	tai <- lookup (BAI (Just "se") "tai") s
	case tai of
		LA (Right (Brivla "cakyrespa")) -> return $ SAVEASCAK fp
		LA (Left "syvygyd") -> return $ SAVEASSVG fp
		_ -> error $ show tai

viska :: [(Tag, Sumti)] -> Maybe Command
viska s = do
	KOhA "ko" <- lookup (FA 2) s
	return VISKA

naViska :: [(Tag, Sumti)] -> Maybe Command
naViska s = do
	KOhA "ko" <- lookup (FA 2) s
	return NAVISKA

naPilno :: [(Tag, Sumti)] -> Maybe Command
naPilno s = do
	KOhA "ko" <- lookup (FA 1) s
	return NAPILNOLOPENBI

cisni :: [(Tag, Sumti)] -> Maybe Command
cisni s = do
	LI (Number n) <- lookup (FA 1) s
	KOhA "ko" <- lookup (FA 2) s
	return $ CISNI n

xruti :: [(Tag, Sumti)] -> Maybe Command
xruti s = do
	KOhA "ko" <- lookup (FA 1) s
	return XRUTI

rapli :: [(Tag, Sumti)] -> Maybe Command
rapli s = do
	LO (NU p) _ <- lookup (FA 1) s
	LI (Number n) <- lookup (FA 2) s
	return $ Repeat (round n) (command p [])

clugau :: [(Tag, Sumti)] -> Maybe Command
clugau s = do
	KOhA "ko" <- lookup (FA 1) s
	case ((Time ["co'a"], KU) `elem` s, (Time ["co'u"], KU) `elem` s) of
		(True, False) -> return COhACLUGAU
		(False, True) -> return COhUCLUGAU
		_ -> fail "bad"

pilno :: ReadCommand
pilno terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	binxo <- selpli args . linkargsToPOI =<< lookup (FA 2) terms
	return $ if (Time ["ba"], KU) `elem` terms
		then binxo
		else Commands binxo PILNOLOPENBI

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

linkargsToPOI :: Sumti -> Sumti
linkargsToPOI (LO (Linkargs selbri (SFIhO modal sumti)) Nothing) =
	LO selbri $ Just $ POI $ Bridi modal [(FA 1, sumti)]
linkargsToPOI (LO (Linkargs selbri sumti) Nothing) =
	LO selbri $ Just $ POI $ Bridi selbri [(FA 2, sumti)]
linkargsToPOI s = s

selpli :: [Argument] -> Sumti -> Maybe Command
selpli args (LO (Brivla "penbi") (Just (POI bridi))) = penbi args bridi
selpli args (LO (Brivla "burcu") (Just (POI bridi))) = burcu args bridi
selpli _ (LO (Brivla "penbi") Nothing) = return KUNTI
selpli _ p = return $ UnknownSelpli p

pebyska :: String -> Maybe Command
pebyska skari = uncurry3 PEBYSKA <$> lookup skari skaste

penbi :: [Argument] -> Lojban -> Maybe Command
penbi _ (Bridi (Brivla s) []) = pebyska s
penbi args (Bridi (Brivla "penbi") [(FA 2, s)]) = applyLO args s pebyska
penbi args (Bridi (ME s) []) = applyLO args s pebyska
penbi args (Bridi (Brivla "cisni") [(FA 1, s)]) =
	applyDouble args s $ return . PEBYCISNI
penbi _ p = return $ ErrorC $ "penbi: no such penbi" ++ show p

burska :: String -> Maybe Command
burska skari = uncurry3 BURSKA <$> lookup skari skaste

burcu :: [Argument] -> Lojban -> Maybe Command
burcu _ (Bridi (Brivla skari) []) = burska skari
burcu a (Bridi (ME s) []) = applyLO a s burska
burcu a (Bridi (SE 2 (Brivla "skari")) [(FA 1, s)]) = applyLO a s burska
burcu a (Bridi (Brivla "skari") [(FA 2, s)]) = applyLO a s burska
burcu _ _ = return $ ErrorC "burcu: no such burcu"

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

lahu :: [Sumti] -> [(Tag, Sumti)] -> Maybe Double
lahu args s = do
	l <- lookup (BAI Nothing "la'u") s
	applyDouble args l return

type ReadCommand = [(Tag, Sumti)] -> [Argument] -> Maybe Command

crakla :: ReadCommand
crakla terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	return $ maybe (CRAKLA 100)  CRAKLA $ lahu args terms

rixykla :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
rixykla s args = do
	KOhA "ko" <- lookup (FA 1) s
	return $ RIXYKLA $ fromMaybe 100 $ lahu args s
--	return $ RIXYKLA $ maybe 100 (\(Left d) -> d) $ lahu s

carna :: ReadCommand
carna terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	farna <- case lookup (FA 3) terms of
		Just (LO (Brivla zp) _) -> Just zp
		Nothing -> Just "zunle"
		_ -> Nothing
	let	jganu = fromMaybe 90 $ lahu args terms
	case farna of
		"zunle" -> return $ ZUNLE jganu
		"pritu" -> return $ PRITU jganu
		_ -> fail "bad"

getDouble :: [Argument] -> Int -> Maybe Double
getDouble args i = do
	when (length args < i) $ fail "bad"
	LI (Number d) <- return $ args !! (i - 1)
	return d

applyDouble :: Monad m => [Sumti] -> Sumti -> (Double -> m a) -> m a
applyDouble args (CEhU i) cmd
	| length args >= i, LI (Number d) <- args !! (i - 1) = cmd d
applyDouble _ (LI (Number d)) cmd = cmd d
applyDouble _ s _ = fail $ "applyDouble: bad sumti" ++ show s

applyLO :: Monad m => [Sumti] -> Sumti -> (String -> m a) -> m a
applyLO args (CEhU i) cmd
	| length args >= i, LO (Brivla s) Nothing <- args !! (i - 1) = cmd s
applyLO _ (LO (Brivla s) Nothing) cmd = cmd s
applyLO _ s _ = fail $ "applyLO: bad sumti" ++ show s

sumtiToArgument :: Sumti -> [Sumti]
sumtiToArgument (STense "ba" s t) = sumtiToArgument s ++ sumtiToArgument t
sumtiToArgument s = [s]

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

type Argument = Sumti

type Table = [(String, [Argument] -> Command)]
theTable :: IORef Table
theTable = unsafePerformIO $ newIORef []
writeTable :: String -> ([Argument] -> Command) -> IO ()
writeTable cmene fasnu = atomicModifyIORef_ theTable ((cmene, fasnu) :)
readTable :: String -> IO (Maybe ([Argument] -> Command))
readTable cmene = lookup cmene <$> readIORef theTable

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
run _ _ (MORJI cmene fasnu) = writeTable cmene fasnu >> return True
run f t (GASNU cmene sumti) = do
	mfasnu <- readTable cmene
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
