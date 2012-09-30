module Main where

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Turtle
import Language.Lojban.Parser

import System.Environment

main :: IO ()
main = do
	prgName <- getProgName
	rawArgs <- getArgs
	_args <- initialize prgName rawArgs
	f <- openField
	t <- newTurtle f
	pencolor t ((255, 255, 255) :: (Int, Int, Int))
	fillcolor t ((255, 255, 255) :: (Int, Int, Int))
	oninputtext f (processInput f t . readCommand . parse)
	mainLoop

data Command = COhO | Unknown Text | ParseError
	deriving Show

readCommand :: Either ParseError Text -> Command
readCommand (Left _) = ParseError
readCommand (Right (TopText _ _ [VocativeSumti [(_, "co'o", _)] _ _] _ _ _)) =
	COhO
readCommand (Right t) = Unknown t

processInput :: Field -> Turtle -> Command -> IO Bool
processInput _ _ COhO = return False
processInput f _ (Unknown t) = do
	outputString f ".i mi na jimpe"
	print $ show t
	return True
processInput f _ ParseError = do
	outputString f ".i di'u na drani fo lo gerna"
	return True
