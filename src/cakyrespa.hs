module Main where

import Minde(run)
import Jimpe(command)
import Tcidu(readLojban)

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
	oninputtext f $ run f t env . command . readLojban
	mainLoop
