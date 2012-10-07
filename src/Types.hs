module Types (
	Command(..),
	Lojban(..), Selbri(..), Tag(..), Sumti(..), Mex(..), RelativeClause(..)
) where

import Language.Lojban.Parser(Free(..))

--------------------------------------------------------------------------------

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
	| MORJI String ([Sumti] -> Command)
	| GASNU String [Sumti]
	| KUNTI
	| COhO | Unknown Lojban | ParseErrorC | UnknownSelpli Sumti
	| ErrorC String

data Lojban
	= Bridi Selbri [(Tag, Sumti)]
	| TenseGI String Lojban Lojban
	| Vocative String
	| Free [Free]
	| Prenex [Sumti] Lojban
	| ParseError String
	| NotImplemented String
	deriving (Show, Eq)

data Selbri
	= Brivla String
	| Linkargs Selbri Sumti
	| NU Lojban
	| DUhU Lojban
	| NotImplementedSelbri String
	| NA Selbri
	| ME Sumti
	| SE Int Selbri
	deriving (Show, Eq)

data Sumti
	= KOhA String
	| CEhU Int
	| CEhUPre
	| LA (Either String Selbri)
	| LE Selbri
	| LO Selbri (Maybe RelativeClause)
	| LI Mex
	| KU
	| SFIhO Selbri Sumti
	| ZOI String
	| TUhA Sumti
	| GOI Sumti Sumti
	| LerfuString String
	| STense String Sumti Sumti
	| LAhE Sumti
	| NotImplementedSumti String
	deriving (Show, Eq)

data RelativeClause
	= POI Lojban
	| Debug String
	| NotImplementedRelativeClause String
	deriving (Show, Eq)

data Mex
	= Number Double
	| JOhI [Mex]
	| NotImplementedMex String
	deriving (Show, Eq)

data Tag
	= FA Int
	| FIhO Selbri
	| BAI (Maybe String) String
	| Time [String]
	| NotImplementedTag String
	deriving (Show, Eq)
