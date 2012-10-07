module Functions (command, Command(..), Sumti, readLojban) where

import Read( readLojban,
	Lojban(..), Selbri(..), Tag(..), Sumti(..), RelativeClause(..), Mex(..))

import Control.Applicative((<$>))
import Data.Maybe(fromMaybe)

--------------------------------------------------------------------------------

command :: Lojban -> Command
command = flip cmd []

cmd :: Lojban -> [Sumti] -> Command
cmd (Vocative "co'o") _ = COhO
cmd (TenseGI "ba" b c) args = Commands (cmd b args) (cmd c args)
cmd (Prenex ss b) _ = CommandList $ cmd b <$> mapM bagi ss
	where
	bagi (STense "ba" s t) = bagi s ++ bagi t
	bagi s = [s]
cmd b@(Bridi (Brivla brivla) s) args =
	fromMaybe (Unknown b) $ maybe Nothing (($ args) . ($ s))  $ lookup brivla pair
cmd b@(Bridi (NA (Brivla brivla)) s) args =
	fromMaybe (Unknown b) $ maybe Nothing (($ args) . ($ s)) $ lookup brivla notPair
cmd l _ = Unknown l

notPair :: [(String, [(Tag, Sumti)] -> [Sumti] -> Maybe Command)]
notPair = [
	("viska", naViska),
	("pilno", naPilno)
 ]

pair :: [(String, [(Tag, Sumti)] -> [Sumti] -> Maybe Command)]
pair = [
	("gasnu", gasnu),
	("morji",  morji),
	("klama", klama),
	("galfi", galfi),
	("tcidu", tcidu),
	("rejgau", rejgau),
	("viska", viska),
	("cisni", cisni),
	("xruti", xruti),
	("rapli", rapli),
	("carna", carna),
	("crakla", crakla),
	("rixykla", rixykla),
	("pilno", pilno),
	("clugau", clugau)
 ]

gasnu :: ReadCommand
gasnu s a = do
	KOhA "ko" <- lookup (FA 1) s
	LerfuString cmene <- lookup (FA 2) s
	return $ GASNU cmene a

morji :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
morji s args = do
	KOhA "ko" <- lookup (FA 1) s
	GOI lerfu duhu <- lookup (FA 2) s
	apply2 args lerfu duhu $ \l d -> case (l, d) of
		(LerfuString cmene, LO (DUhU fasnu) _) ->
			return $ MORJI cmene $ cmd fasnu
		a -> return $ ErrorC $ show a

klama :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
klama s args = do
	KOhA "ko" <- lookup (FA 1) s
	sumti <- lookup (FA 2 ) s
	apply args sumti $ \smt -> case smt of
		LI (JOhI [Number x, Number y]) -> return $ KLAMA x y
		_ -> return $ ErrorC $ show s

galfi :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
galfi s args = do
	TUhA (KOhA "ko") <- lookup (FA 1) s
	LO (Brivla "foldi") Nothing <- lookup (FA 2) s
	sumti <- lookup (FA 3) s
	apply args sumti $ \smt -> case smt of
		LO (Brivla skari) Nothing -> do
			clr <- lookup skari skaste
			return $ let (r, g, b) = clr in FLOSKA r g b
		_ -> return $ ErrorC $ show s

tcidu :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
tcidu s args = do
	KOhA "ko" <- lookup (FA 1) s
	sumti <- lookup (FA 2) s
	apply args sumti $ \smt -> case smt of
		LA (Right (ME (ZOI fp))) -> return $ READFILE fp
		LAhE (ZOI fp) -> return $ READFILE fp
		_ -> return $ ErrorC $ show s

rejgau :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
rejgau s args = do
	KOhA "ko" <- lookup (FA 1) s
	zfp <- lookup (BAI Nothing "me'e") s
	tai <- lookup (BAI (Just "se") "tai") s
	apply2 args zfp tai $ \z t -> case (z, t) of
		(ZOI fp, LA (Right (Brivla "cakyrespa"))) -> return $ SAVEASCAK fp
		(ZOI fp, LA (Left "syvygyd")) -> return $ SAVEASSVG fp
		_ -> error $ show tai

viska :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
viska s _ = do
	KOhA "ko" <- lookup (FA 2) s
	return VISKA

naViska :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
naViska s _ = do
	KOhA "ko" <- lookup (FA 2) s
	return NAVISKA

naPilno :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
naPilno s _ = do
	KOhA "ko" <- lookup (FA 1) s
	return NAPILNOLOPENBI

cisni :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
cisni s args = do
	sumti <- lookup (FA 1) s
	KOhA "ko" <- lookup (FA 2) s
	apply args sumti $ \smt -> case smt of
		LI (Number n) -> return $ CISNI n
		_ -> return $ ErrorC $ show s

xruti :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
xruti s _ = do
	KOhA "ko" <- lookup (FA 1) s
	return XRUTI

rapli :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
rapli s args = do
	sumti1 <- lookup (FA 1) s
	sumti2 <- lookup (FA 2) s
	apply2 args sumti1 sumti2 $ \nu num -> case (nu, num) of
		(LO (NU p) _, LI (Number n)) ->
			return $ Repeat (round n) (cmd p [])
		_ -> return $ ErrorC $ show s

clugau :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
clugau s _ = do
	KOhA "ko" <- lookup (FA 1) s
	case ((Time ["co'a"], KU) `elem` s, (Time ["co'u"], KU) `elem` s) of
		(True, False) -> return COhACLUGAU
		(False, True) -> return COhUCLUGAU
		_ -> fail "bad"

pilno :: ReadCommand
pilno terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	binxo <- selpli args . linkargsToPOI =<< lookup (FA 2) terms
	return $ if (Time ["ba"], KU) `elem` terms
		then binxo
		else Commands binxo PILNOLOPENBI

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

linkargsToPOI :: Sumti -> Sumti
linkargsToPOI (LO (Linkargs selbri (SFIhO modal sumti)) Nothing) =
	LO selbri $ Just $ POI $ Bridi modal [(FA 1, sumti)]
linkargsToPOI (LO (Linkargs selbri sumti) Nothing) =
	LO selbri $ Just $ POI $ Bridi selbri [(FA 2, sumti)]
linkargsToPOI s = s

selpli :: [Sumti] -> Sumti -> Maybe Command
selpli args (LO (Brivla "penbi") (Just (POI bridi))) = penbi args bridi
selpli args (LO (Brivla "burcu") (Just (POI bridi))) = burcu args bridi
selpli _ (LO (Brivla "penbi") Nothing) = return KUNTI
selpli _ p = return $ UnknownSelpli p

pebyska :: String -> Maybe Command
pebyska skari = uncurry3 PEBYSKA <$> lookup skari skaste

penbi :: [Sumti] -> Lojban -> Maybe Command
penbi _ (Bridi (Brivla s) []) = pebyska s
penbi args (Bridi (Brivla "penbi") [(FA 2, s)]) = applyLO args s pebyska
penbi args (Bridi (ME s) []) = applyLO args s pebyska
penbi args (Bridi (Brivla "cisni") [(FA 1, s)]) =
	applyDouble args s $ return . PEBYCISNI
penbi _ p = return $ ErrorC $ "penbi: no such penbi" ++ show p

burska :: String -> Maybe Command
burska skari = uncurry3 BURSKA <$> lookup skari skaste

burcu :: [Sumti] -> Lojban -> Maybe Command
burcu _ (Bridi (Brivla skari) []) = burska skari
burcu a (Bridi (ME s) []) = applyLO a s burska
burcu a (Bridi (SE 2 (Brivla "skari")) [(FA 1, s)]) = applyLO a s burska
burcu a (Bridi (Brivla "skari") [(FA 2, s)]) = applyLO a s burska
burcu _ _ = return $ ErrorC "burcu: no such burcu"

skaste :: [(String, (Int, Int, Int))]
skaste = [
	("xekri", (0, 0, 0)),
	("blabi", (255, 255, 255)),

	("xunre", (255, 0, 0)),
	("labri'o", (0, 255, 0)),	-- Lime
	("crinyblabi", (0, 255, 0)),	-- Lime
	("blanu", (0, 0, 255)),

	("pelxu", (255, 255, 0)),
	("cicna", (0, 255, 255)),
	("nukni", (255, 0, 255)),

	("rijyska", (192, 192, 192)),	-- Silver
	("grusi", (128, 128, 128)),

--	("?", (128, 0, 0)),		-- Maroon
	("alzaityska", (128, 128, 0)),	-- Olive
	("crino", (0, 128, 0)),
	("zirpu", (128, 0, 128)),	-- Purple
--	("?", (0, 128, 128)),		-- Teal
--	("?", (0, 0, 128)),		-- Navy

	("bunre", (165, 42, 42)),	-- Brown
	("narju", (255, 165, 0)),	-- Orange
	("sloska", (255, 215, 0)),	-- Gold
	("xunblabi", (255, 192, 203))	-- Pink
 ]

lahu :: [Sumti] -> [(Tag, Sumti)] -> Maybe Double
lahu args s = do
	l <- lookup (BAI Nothing "la'u") s
	applyDouble args l return

type ReadCommand = [(Tag, Sumti)] -> [Sumti] -> Maybe Command

crakla :: ReadCommand
crakla terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	return $ maybe (CRAKLA 100)  CRAKLA $ lahu args terms

rixykla :: [(Tag, Sumti)] -> [Sumti] -> Maybe Command
rixykla s args = do
	KOhA "ko" <- lookup (FA 1) s
	return $ RIXYKLA $ fromMaybe 100 $ lahu args s

carna :: ReadCommand
carna terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	farna <- case lookup (FA 3) terms of
		Just (LO (Brivla zp) _) -> Just zp
		Nothing -> Just "zunle"
		_ -> Nothing
	let	jganu = fromMaybe 90 $ lahu args terms
	case farna of
		"zunle" -> return $ ZUNLE jganu
		"pritu" -> return $ PRITU jganu
		_ -> fail "bad"

apply2 :: Monad m => [Sumti] -> Sumti -> Sumti -> (Sumti -> Sumti -> m a) -> m a
apply2 args s1 s2 cm = apply args s2 =<< apply args s1 (return . cm)

apply :: Monad m => [Sumti] -> Sumti -> (Sumti -> m a) -> m a
apply args (CEhU i) cm
	| length args >= i = cm $ args !! (i - 1)
	| otherwise = fail "apply: too few args"
apply _ s cm = cm s

applyDouble :: Monad m => [Sumti] -> Sumti -> (Double -> m a) -> m a
applyDouble args sumti cm = apply args sumti $ \s -> case s of
	LI (Number d) -> cm d
	_ -> fail $ "applyDouble: bad sumti " ++ show s

applyLO :: Monad m => [Sumti] -> Sumti -> (String -> m a) -> m a
applyLO args sumti cm = apply args sumti $ \s -> case s of
	LO (Brivla d) Nothing -> cm d
	_ -> fail $ "applyLO: bad sumti " ++ show s

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
