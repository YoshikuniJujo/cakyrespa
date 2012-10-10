module Klesi (
	Minde(..),
	Lojban(..), Selbri(..), Tag(..), Sumti(..), Mex(..), RelativeClause(..)
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
