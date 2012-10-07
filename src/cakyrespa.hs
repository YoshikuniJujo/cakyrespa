module Main where

import Functions(pair, notPair, Lojban(..), Selbri(..), Sumti(..), readLojban,
	Command(..))

import Text.XML.YJSVG(showSVG)
import Graphics.UI.GLUT(initialize, mainLoop)
import Graphics.UI.GLUT.Turtle(
	Field, openField, prompt, outputString, oninputtext,
	Turtle, newTurtle, runInputs, inputs, getSVG,
	goto, forward, backward, left, right, beginfill, endfill, undo, notundo,
	penup, pendown, hideturtle, showturtle, shape, shapesize,
	pensize, pencolor, fillcolor, bgcolor, windowWidth, windowHeight)

import System.Environment(getProgName, getArgs)
import System.IO.Unsafe(unsafePerformIO)
import Control.Applicative((<$>))
import Control.Monad(replicateM_)
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
command (Prenex ss b) _ = CommandList $ command b <$> mapM bagi ss
	where
	bagi (STense "ba" s t) = bagi s ++ bagi t
	bagi s = [s]
command b@(Bridi (Brivla brivla) s) args =
	fromMaybe (Unknown b) $ maybe Nothing (($ args) . ($ s))  $ lookup brivla pair
command b@(Bridi (NA (Brivla brivla)) s) args =
	fromMaybe (Unknown b) $ maybe Nothing (($ args) . ($ s)) $ lookup brivla notPair
command l _ = Unknown l

type Table = [(String, [Sumti] -> Command)]
theTable :: IORef Table
theTable = unsafePerformIO $ newIORef []
writeTable :: String -> ([Sumti] -> Command) -> IO ()
writeTable cmene fasnu = atomicModifyIORef_ theTable ((cmene, fasnu) :)
readTable :: String -> IO (Maybe ([Sumti] -> Command))
readTable cmene = lookup cmene <$> readIORef theTable

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
