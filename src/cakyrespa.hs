module Main where

import Run(run)
import Command(command)
import Read(readLojban)

import Graphics.UI.GLUT(mainLoop)
import Graphics.UI.GLUT.Turtle(
	initialize, openField, newTurtle, prompt, notundo, oninputtext,
	shape, shapesize)	


--------------------------------------------------------------------------------

main :: IO ()
main = do
	_args <- initialize
	f <- openField "cakyrespa" 640 480
	t <- newTurtle f
	prompt f ".i "
	shape t "turtle"
	shapesize t 3 3
	notundo t
	oninputtext f $ run f t . command . readLojban
	mainLoop
