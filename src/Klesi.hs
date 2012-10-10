module Klesi (
	Minde(..),
	Text(..), Selbri(..), Tag(..), Sumti(..), Mex(..), RelativeClause(..)
) where

import Language.Lojban.Parser(Free(..))

--------------------------------------------------------------------------------

data Minde
	= KLAMA Double Double
	| CRAKLA Double				| RIXYKLA Double
	| ZUNLE Double				| PRITU Double
	| COhACLUGAU				| COhUCLUGAU
	| PEBYCISNI Double			| PEBYSKA Int Int Int
	| BURSKA Int Int Int			| FLOSKA Int Int Int
	| NAPILNOLOPENBI			| PILNOLOPENBI
	| NAVISKA				| VISKA
	| CISNI Double
	| XRUTI
	| MORJI String ([Sumti] -> Minde)	| GASNU String [Sumti]
	| REJGAUSETAICAK FilePath		| TCIDU FilePath
	| REJGAUSETAISVG FilePath
	| COhO
	| MIDSTE [Minde]
	| SRERA String

data Text
	= Bridi Selbri [(Tag, Sumti)]
	| Prenex [Sumti] Text
	| TagGI String Text Text
	| Vocative String
	| Free [Free]
	| UnknownText String
	| ParseError String
	deriving (Show, Eq)

data Selbri
	= Brivla String
	| NA Selbri
	| SE Int Selbri
	| BE Selbri Sumti
	| ME Sumti
	| NU Text
	| DUhU Text
	| UnknownSelbri String
	deriving (Show, Eq)

data Tag
	= FA Int
	| BAI Int String
	| FIhO Selbri
	| Time [String]
	| UnknownTag String
	deriving (Show, Eq)

data Sumti
	= KU
	| CEhUPre
	| CEhU Int
	| KOhA String
	| LA (Either String Selbri)
	| LE Selbri
	| LO Selbri (Maybe RelativeClause)
	| LI Mex
	| LerfuString String
	| ZOI String
	| TUhA Sumti
	| LAhE Sumti
	| GOI Sumti Sumti
	| STense String Sumti Sumti
	| SFIhO Selbri Sumti
	| UnknownSumti String
	deriving (Show, Eq)

data Mex
	= Number Double
	| JOhI [Mex]
	| UnknownMex String
	deriving (Show, Eq)

data RelativeClause
	= POI Text
	| UnknownRelativeClause String
	deriving (Show, Eq)