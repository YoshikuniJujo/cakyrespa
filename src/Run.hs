module Run (run) where

import Types

import Text.XML.YJSVG(showSVG)
import Graphics.UI.GLUT.Turtle(
	Field, outputString,
	Turtle, runInputs, inputs, getSVG,
	goto, forward, backward, left, right, beginfill, endfill, undo,
	penup, pendown, hideturtle, showturtle, shapesize,
	pensize, pencolor, fillcolor, bgcolor, windowWidth, windowHeight)

import Control.Applicative((<$>))
import Control.Monad(replicateM_)
import Data.IORef(IORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)

--------------------------------------------------------------------------------

writeT :: a -> b -> IORef [(a, b)] -> IO ()
writeT k v t = atomicModifyIORef_ t ((k, v) :)
readT :: Eq a => a -> IORef [(a, b)] -> IO (Maybe b)
readT k t = lookup k <$> readIORef t

run :: Field -> Turtle -> IORef [(String, [Sumti] -> Command)] -> Command -> IO Bool
run _ t _ (KLAMA x y) = goto t x y >> return True
run _ t _ (CRAKLA d) = forward t d >> return True
run _ t _ (RIXYKLA d) = backward t d >> return True
run _ t _ (ZUNLE d) = left t d >> return True
run _ t _ (PRITU d) = right t d >> return True
run _ t _ (PEBYCISNI s) = pensize t s >> return True
run _ t _ (PEBYSKA r g b) = pencolor t (r, g, b) >> return True
run _ t _ (BURSKA r g b) = fillcolor t (r, g, b) >> return True
run _ t _ (FLOSKA r g b) = bgcolor t (r, g, b) >> return True
run _ t _ COhACLUGAU = beginfill t >> return True
run _ t _ COhUCLUGAU = endfill t >> return True
run _ t _ XRUTI = undo t >> return True
run _ t _ (CISNI s) = shapesize t s s >> return True
run _ t _ NAPILNOLOPENBI = penup t >> return True
run _ t _ PILNOLOPENBI = pendown t >> return True
run _ t _ NAVISKA = hideturtle t >> return True
run _ t _ VISKA = showturtle t >> return True
run _ _ _ COhO = return False
run f t tbl (Commands c d) = run f t tbl c >> run f t tbl d
run f t tbl (CommandList cl) =
	mapM_ (run f t tbl) cl >> return True
run f t tbl (Repeat n c) = replicateM_ n (run f t tbl c) >>
	return True
run _ _ tbl (MORJI cmene fasnu) = writeT cmene fasnu tbl >> return True
run f t tbl (GASNU cmene sumti) = do
	mfasnu <- readT cmene tbl
	flip (maybe $ run f t tbl $ ErrorC $ "not defined: " ++ cmene) mfasnu $
		\fasnu -> run f t tbl $ fasnu sumti
run _ t _ (SAVEASSVG fp) = do
	w <- windowWidth t
	h <- windowHeight t
	svg <- getSVG t
	writeFile fp $ showSVG w h svg
	return True
run _ t _ (SAVEASCAK fp) = inputs t >>= writeFile fp . show >> return True
run _ t _ (READFILE fp) = readFile fp >>= runInputs t . read >> return True
run _ _ _ KUNTI = return True
run f _ _ (Unknown u) = do
	outputString f ".i mi na jimpe"
	putStr "Unknown " >> print u
	return True
run f _ _ (UnknownSelpli us) = do
	outputString f ".i mi na djuno lo bi'unai selpli"
	putStr "Unknown " >> print us
	return True
run f _ _ ParseErrorC = do
	outputString f ".i di'u na drani fo lo gerna"
	return True
run _ _ _ (ErrorC str) = do
	putStrLn $ "error: " ++ str
	return True
