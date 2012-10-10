module Main where

import Minde(minde)
import Jimpe(jimpe)
import Tcidu(parse)

import Graphics.UI.GLUT(mainLoop)
import Graphics.UI.GLUT.Turtle(
	initialize, openField, prompt, oninputtext,
	newTurtle, notundo, shape, shapesize)	
import Data.IORef(newIORef)


--------------------------------------------------------------------------------

main :: IO ()
main = do
	_args <- initialize
	f <- openField "cakyrespa" 640 480
	t <- newTurtle f
	env <- newIORef []
	prompt f ".i "
	shape t "turtle"
	shapesize t 3 3
	notundo t
	oninputtext f $ minde f t env . jimpe . parse
	mainLoop
