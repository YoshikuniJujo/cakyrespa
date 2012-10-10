module Minde (run) where

import Klesi(Minde(..), Sumti)

import Text.XML.YJSVG(showSVG)
import Graphics.UI.GLUT.Turtle(
	Field, outputString,
	Turtle, runInputs, inputs, getSVG,
	goto, forward, backward, left, right, beginfill, endfill, undo,
	penup, pendown, hideturtle, showturtle, shapesize,
	pensize, pencolor, fillcolor, bgcolor, windowWidth, windowHeight)

import Control.Applicative((<$>))
import Data.IORef(IORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)

--------------------------------------------------------------------------------

readT :: Eq a => a -> IORef [(a, b)] -> IO (Maybe b)
readT k t = lookup k <$> readIORef t

writeT :: a -> b -> IORef [(a, b)] -> IO ()
writeT k v t = atomicModifyIORef_ t ((k, v) :)

run :: Field -> Turtle -> IORef [(String, [Sumti] -> Minde)] -> Minde -> IO Bool
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
run f t tbl (MIDSTE cl) = mapM_ (run f t tbl) cl >> return True
run _ _ tbl (MORJI cmene fasnu) = writeT cmene fasnu tbl >> return True
run f t tbl (GASNU cmene sumti) = do
	mfasnu <- readT cmene tbl
	flip (maybe $ run f t tbl $ SRERA $ "not defined: " ++ cmene) mfasnu $
		\fasnu -> run f t tbl $ fasnu sumti
run _ t _ (REJGAUSETAISVG fp) = do
	w <- windowWidth t
	h <- windowHeight t
	svg <- getSVG t
	writeFile fp $ showSVG w h svg
	return True
run _ t _ (REJGAUSETAICAK fp) = inputs t >>= writeFile fp . show >> return True
run _ t _ (TCIDU fp) = readFile fp >>= runInputs t . read >> return True
run f _ _ (SRERA str) = do
	outputString f ".i mi na jimpe"
	putStrLn $ "error: " ++ str
	return True
