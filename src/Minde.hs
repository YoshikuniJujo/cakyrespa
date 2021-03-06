module Minde (minde) where

import Klesi(Minde(..), Sumti)

import Text.XML.YJSVG(showSVG)
import Graphics.UI.GLUT.Turtle(setFieldSize, Console, home,
	Field, consoleOutput, clear,
	Turtle, runInputs, inputs, getSVG,
	goto, forward, backward, left, right, beginfill, endfill, undo,
	penup, pendown, hideturtle, showturtle, shapesize,
	pensize, pencolor, fillcolor, bgcolor, windowWidth, windowHeight)

import Control.Applicative((<$>))
import Data.IORef(IORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)

--------------------------------------------------------------------------------

vabytcidu :: Eq a => a -> IORef [(a, b)] -> IO (Maybe b)
vabytcidu cmene vanbi = lookup cmene <$> readIORef vanbi

vabyciha :: a -> b -> IORef [(a, b)] -> IO ()
vabyciha cmene fasnu vanbi = atomicModifyIORef_ vanbi ((cmene, fasnu) :)

minde :: Field -> Console -> Turtle -> IORef [(String, [Sumti] -> Minde)] -> Minde -> IO Bool
minde _ _ t _ (KLAMA x y) = goto t x y >> return True
minde _ _ t _ (CRAKLA d) = forward t d >> return True
minde _ _ t _ (RIXYKLA d) = backward t d >> return True
minde _ _ t _ (ZUNLE d) = left t d >> return True
minde _ _ t _ (PRITU d) = right t d >> return True
minde _ _ t _ XRUKLALOKRASI = home t >> return True
minde _ _ t _ COhACLUGAU = beginfill t >> return True
minde _ _ t _ COhUCLUGAU = endfill t >> return True
minde _ _ t _ (PEBYCISNI s) = pensize t s >> return True
minde _ _ t _ (PEBYSKA r g b) = pencolor t (r, g, b) >> return True
minde _ _ t _ (BURSKA r g b) = fillcolor t (r, g, b) >> return True
minde _ _ t _ (FLOSKA r g b) = bgcolor t (r, g, b) >> return True
minde _ _ t _ NAPILNOLOPENBI = penup t >> return True
minde _ _ t _ PILNOLOPENBI = pendown t >> return True
minde _ _ t _ NAVISKA = hideturtle t >> return True
minde _ _ t _ VISKA = showturtle t >> return True
minde _ _ t _ (CISNI s) = shapesize t s s >> return True
minde f _ _ _ (CISNYGAUFOLDI w h) = setFieldSize f w h >> return True
minde _ _ t _ VIMCULOPIXRA = clear t >> return True
minde _ _ t _ XRUTI = undo t >> return True
minde _ _ _ vanbi (MORJI cmene fasnu) = vabyciha cmene fasnu vanbi >> return True
minde f c t vanbi (GASNU cmene sumti) = vabytcidu cmene vanbi >>=
	minde f c t vanbi . maybe (SRERA $ "not defined: " ++ cmene) ($ sumti)
minde _ _ t _ (REJGAUSETAICAK fp) = inputs t >>= writeFile fp . show >> return True
minde _ _ t _ (TCIDU fp) = readFile fp >>= runInputs t . read >> return True
minde _ _ t _ (REJGAUSETAISVG fp) = do
	w <- windowWidth t
	h <- windowHeight t
	svg <- getSVG t
	writeFile fp $ showSVG w h svg
	return True
minde _ _ _ _ (REJGAUSETAIJBO _) = return True
minde _ _ _ _ (TCIDUSETAIJBO _) = return True
minde _ _ _ _ COhO = return False
minde f c t vanbi (MIDYSTE cl) = mapWhile (minde f c t vanbi) cl
minde _ c _ _ (SRERA str) = do
	consoleOutput c ".i mi na jimpe"
	putStrLn $ "error: " ++ str
	return True

mapWhile :: Monad m => (a -> m Bool) -> [a] -> m Bool
mapWhile _ [] = return True
mapWhile f (x : xs) = do
	b <- f x
	if b then mapWhile f xs else return False
