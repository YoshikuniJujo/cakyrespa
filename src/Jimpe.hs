module Jimpe (jimpe) where

import Klesi(
	Minde(..),
	Text(..), Selbri(..), Tag(..), Sumti(..), Mex(..), RelativeClause(..))

import Control.Applicative((<$>))
import Data.Maybe(fromMaybe)
import Data.Tuple.Tools(uncurry3)

--------------------------------------------------------------------------------

jimpe :: Text -> Minde
jimpe = flip jmi []

jmi :: Text -> [Sumti] -> Minde
jmi t@(Bridi (Brivla brivla) terms) args = fromMaybe (SRERA $ show t) $
	lookup brivla midste >>= \mid -> mid terms args
jmi t@(Bridi (NA (Brivla brivla)) terms) args = fromMaybe (SRERA $ show t) $
	lookup brivla narmidste >>= \mid -> mid terms args
jmi (Prenex sumste bridi) _ = MIDSTE $ jmi bridi <$> mapM bagi sumste
	where
	bagi (STense "ba" pavsu'i relsu'i) = bagi pavsu'i ++ bagi relsu'i
	bagi sumti = [sumti]
jmi (TagGI "ba" pavbri relbri) args =
	MIDSTE $ [jmi pavbri args, jmi relbri args]
jmi (Vocative "co'o") _ = COhO
jmi l _ = SRERA $ show l

type Midytcidu = [(Tag, Sumti)] -> [Sumti] -> Maybe Minde

midste, narmidste :: [(String, Midytcidu)]
midste = [
	("klama", klama),
	("crakla", crakla),
	("rixykla", rixykla),
	("carna", carna),
	("clugau", clugau),
	("galfi", galfi),
	("pilno", pilno),
	("cisni", cisni),
	("viska", viska),
	("rapli", rapli),
	("xruti", xruti),
	("morji", morji),
	("gasnu", gasnu),
	("rejgau", rejgau),
	("tcidu", tcidu)]
narmidste = [
	("pilno", napilno),
	("viska", naviska)]

apply :: Monad m => [Sumti] -> Sumti -> (Sumti -> m a) -> m a
apply args (CEhU i) cmd
	| length args >= i = cmd $ args !! (i - 1)
	| otherwise = fail "apply: too few args"
apply _ sumti cmd = cmd sumti

apply2 :: Monad m => [Sumti] -> Sumti -> Sumti -> (Sumti -> Sumti -> m a) -> m a
apply2 args s1 s2 cmd = apply args s2 =<< apply args s1 (return . cmd)

klama, crakla, rixykla, carna, clugau, galfi, pilno, cisni, viska, rapli, xruti,
	morji, gasnu, rejgau, tcidu, napilno, naviska :: Midytcidu

klama terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	selkla <- lookup (FA 2) terms
	apply args selkla $ \sk -> case sk of
		LI (JOhI [Number x, Number y]) -> return $ KLAMA x y
		_ -> return $ SRERA $ show terms ++ " " ++ show args

crakla terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	return . CRAKLA . fromMaybe 100 $ la'u terms args

rixykla terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	return . RIXYKLA . fromMaybe 100 $ la'u terms args

carna terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	farna <- case lookup (FA 3) terms of
		Nothing -> return "zunle"
		Just far -> apply args far $ \f -> case f of
			LO (Brivla zp) _ -> return zp
			_ -> fail "carna: bad direction"
	($ fromMaybe 90 $ la'u terms args) <$> case farna of
		"zunle" -> return ZUNLE
		"pritu" -> return PRITU
		_ -> fail "carna: bad direction"

la'u :: [(Tag, Sumti)] -> [Sumti] -> Maybe Double
la'u terms args = do
	klani <- lookup (BAI 1 "la'u") terms
	apply args klani $ \k -> case k of
		LI (Number n) -> return n
		_ -> fail "la'u: not number"

clugau terms _ = do
	KOhA "ko" <- lookup (FA 1) terms
	case ((Time ["co'a"], KU) `elem` terms,
		(Time ["co'u"], KU) `elem` terms) of
		(True, False) -> return COhACLUGAU
		(False, True) -> return COhUCLUGAU
		_ -> fail "clugau: bad tense"

galfi terms args = do
	TUhA (KOhA "ko") <- lookup (FA 1) terms
	LO (Brivla "foldi") Nothing <- lookup (FA 2) terms
	terga'i <- lookup (FA 3) terms
	apply args terga'i $ \skasu'i -> case skasu'i of
		LO (Brivla skale'u) Nothing -> do
			skari <- lookup skale'u skaste
			return $ let (r, g, b) = skari in FLOSKA r g b
		_ -> return $ SRERA $ "galfi: " ++ show terms ++ " " ++ show args

pilno terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	binxo <- selpli args . linkargsToPOI =<< lookup (FA 2) terms
	return $ if (Time ["ba"], KU) `elem` terms
		then MIDSTE binxo
		else MIDSTE $ binxo ++ [PILNOLOPENBI]

selpli :: [Sumti] -> Sumti -> Maybe [Minde]
selpli args (LO (Brivla "penbi") (Just (POI bridi))) = (: []) <$> penbi args bridi
selpli args (LO (Brivla "burcu") (Just (POI bridi))) = (: []) <$> burcu args bridi
selpli _ (LO (Brivla "penbi") Nothing) = return [] -- return KUNTI
selpli _ p = return [SRERA $ show p]

pebyska :: String -> Maybe Minde
pebyska skari = uncurry3 PEBYSKA <$> lookup skari skaste

penbi :: [Sumti] -> Text -> Maybe Minde
penbi _ (Bridi (Brivla s) []) = pebyska s
penbi args (Bridi (Brivla "penbi") [(FA 2, s)]) = applyLO args s pebyska
penbi args (Bridi (ME s) []) = applyLO args s pebyska
penbi args (Bridi (Brivla "cisni") [(FA 1, s)]) = apply args s $ \smt -> case smt of
	LI (Number d) -> return $ PEBYCISNI d
	_ -> fail "bad"
penbi _ p = return $ SRERA $ "penbi: no such penbi" ++ show p

burska :: String -> Maybe Minde
burska skari = uncurry3 BURSKA <$> lookup skari skaste

burcu :: [Sumti] -> Text -> Maybe Minde
burcu _ (Bridi (Brivla skari) []) = burska skari
burcu a (Bridi (ME s) []) = applyLO a s burska
burcu a (Bridi (SE 2 (Brivla "skari")) [(FA 1, s)]) = applyLO a s burska
burcu a (Bridi (Brivla "skari") [(FA 2, s)]) = applyLO a s burska
burcu _ _ = return $ SRERA "burcu: no such burcu"

applyLO :: Monad m => [Sumti] -> Sumti -> (String -> m a) -> m a
applyLO args sumti cm = apply args sumti $ \s -> case s of
	LO (Brivla d) Nothing -> cm d
	_ -> fail $ "applyLO: bad sumti " ++ show s

linkargsToPOI :: Sumti -> Sumti
linkargsToPOI (LO (BE selbri (SFIhO modal sumti)) Nothing) =
	LO selbri $ Just $ POI $ Bridi modal [(FA 1, sumti)]
linkargsToPOI (LO (BE selbri sumti) Nothing) =
	LO selbri $ Just $ POI $ Bridi selbri [(FA 2, sumti)]
linkargsToPOI s = s

cisni s args = do
	sumti <- lookup (FA 1) s
	KOhA "ko" <- lookup (FA 2) s
	apply args sumti $ \smt -> case smt of
		LI (Number n) -> return $ CISNI n
		_ -> return $ SRERA $ show s

viska s _ = do
	KOhA "ko" <- lookup (FA 2) s
	return VISKA

rapli s args = do
	sumti1 <- lookup (FA 1) s
	sumti2 <- lookup (FA 2) s
	apply2 args sumti1 sumti2 $ \nu num -> case (nu, num) of
		(LO (NU p) _, LI (Number n)) ->
			return $ MIDSTE $ replicate (round n) (jmi p [])
		_ -> return $ SRERA $ show s

xruti s _ = do
	KOhA "ko" <- lookup (FA 1) s
	return XRUTI

morji s args = do
	KOhA "ko" <- lookup (FA 1) s
	GOI lerfu duhu <- lookup (FA 2) s
	apply2 args lerfu duhu $ \l d -> case (l, d) of
		(LerfuString cmene, LO (DUhU fasnu) _) ->
			return $ MORJI cmene $ jmi fasnu
		a -> return $ SRERA $ show a

gasnu s a = do
	KOhA "ko" <- lookup (FA 1) s
	LerfuString cmene <- lookup (FA 2) s
	return $ GASNU cmene a

rejgau s args = do
	KOhA "ko" <- lookup (FA 1) s
	zfp <- lookup (BAI 1 "me'e") s
	tai <- lookup (BAI 2 "tai") s
	apply2 args zfp tai $ \z t -> case (z, t) of
		(ZOI fp, LA (Right (Brivla "cakyrespa"))) -> return $ REJGAUSETAICAK fp
		(ZOI fp, LA (Left "syvygyd")) -> return $ REJGAUSETAISVG fp
		_ -> error $ show tai

tcidu s args = do
	KOhA "ko" <- lookup (FA 1) s
	sumti <- lookup (FA 2) s
	apply args sumti $ \smt -> case smt of
		LA (Right (ME (ZOI fp))) -> return $ TCIDU fp
		LAhE (ZOI fp) -> return $ TCIDU fp
		_ -> return $ SRERA $ show s

napilno s _ = do
	KOhA "ko" <- lookup (FA 1) s
	return NAPILNOLOPENBI

naviska s _ = do
	KOhA "ko" <- lookup (FA 2) s
	return NAVISKA

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
