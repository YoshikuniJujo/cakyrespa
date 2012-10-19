module Main where

import Minde(minde)
import Jimpe(jimpe)
import Tcidu(parse)
import Klesi(Text(ParseError), Minde(REJGAUSETAIJBO, TCIDUSETAIJBO))

import Graphics.UI.GLUT(mainLoop)
import Graphics.UI.GLUT.Turtle(
	initialize, openField, openConsole, setConsole, consolePrompt,
	oncommand,
	newTurtle, notundo, shape, shapesize)	
import Data.IORef(newIORef, readIORef)
import Data.IORef.Tools(atomicModifyIORef_)


--------------------------------------------------------------------------------

main :: IO ()
main = do
	_args <- initialize
	f <- openField "cakyrespa" 640 480
	c <- openConsole "console" 640 480
	setConsole f c
	t <- newTurtle f
	env <- newIORef []
	cmds <- newIORef []
	consolePrompt c ".i "
	shape t "turtle"
	shapesize t 3 3
	notundo t
	oncommand f $ \cmd -> do
		let	p = parse cmd
			j = jimpe p
		case (p, j) of
			(_, REJGAUSETAIJBO fp) ->
				writeFile fp . unlines . reverse =<< readIORef cmds
			(_, TCIDUSETAIJBO fp) ->
				readFile fp >>= minde f c t env . jimpe . parse
					>> return ()
			(ParseError _, _) -> return ()
			_ -> atomicModifyIORef_ cmds ((".i" ++ cmd) :)
		minde f c t env j
	mainLoop
