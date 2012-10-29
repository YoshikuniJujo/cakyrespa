module Jimpe (jimpe) where

import Klesi(
	Minde(..),
	Text(..), Selbri(..), Tag(..), Sumti(..), Mex(..), RelativeClause(..))
import Liste(skaste)

import Control.Applicative((<$>))
import Data.Maybe(fromMaybe)
import Data.Tuple.Tools(uncurry3)

--------------------------------------------------------------------------------

jimpe :: Text -> Minde
jimpe = flip jmi []

jmi :: Text -> [Sumti] -> Minde
jmi (Bridi (Brivla "sisti") [(FA 1, KOhA "ko")]) _ = COhO
jmi t@(Bridi (Brivla brivla) terms) args = fromMaybe (SRERA $ show t) $
	lookup brivla midste >>= \mid -> mid terms args
jmi t@(Bridi (NA (Brivla brivla)) terms) args = fromMaybe (SRERA $ show t) $
	lookup brivla narmidste >>= \mid -> mid terms args
jmi (Prenex sumste bridi) _ = MIDYSTE $ jmi bridi <$> mapM bagi sumste
	where
	bagi (STense "ba" pavsu'i relsu'i) = bagi pavsu'i ++ bagi relsu'i
	bagi sumti = [sumti]
jmi (TagGI "ba" pavbri relbri) args =
	MIDYSTE [jmi pavbri args, jmi relbri args]
jmi (MultiText texts) args = MIDYSTE $ map (($ args) . jmi) texts
jmi (Vocative "co'o") _ = COhO
jmi (Vocative "fe'o") _ = COhO
jmi FAhO _ = COhO
jmi l _ = SRERA $ show l

type Midytcidu = [(Tag, Sumti)] -> [Sumti] -> Maybe Minde

midste, narmidste :: [(String, Midytcidu)]
midste = [
	("klama", klama),
	("crakla", crakla),
	("rixykla", rixykla),
	("carna", carna),
	("xrukla", xrukla),
	("clugau", clugau),
	("galfi", galfi),
	("pilno", pilno),
	("cisni", cisni),
	("cisnygau", cisnygau),
	("viska", viska),
	("rapli", rapli),
	("vimcu", vimcu),
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

klama, crakla, rixykla, carna, clugau, galfi, pilno, cisni, cisnygau, viska,
	rapli, xruti, morji, gasnu, rejgau, tcidu, napilno, naviska, vimcu,
	xrukla :: Midytcidu

xrukla terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	selxrukla <- lookup (FA 2) terms
	apply args selxrukla $ \sxk -> case sxk of
		LO (Brivla "krasi") _ -> return XRUKLALOKRASI
		_ -> return $ SRERA $ show terms ++ " " ++ show args

vimcu terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	selgau <- lookup (FA 2) terms
	apply args selgau $ \sg -> case sg of
		LO (Brivla "pixra") _ -> return VIMCULOPIXRA
		_ -> return $ SRERA $ show terms ++ " " ++ show args

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
	sp <- lookup (FA 2) terms
	apply args sp $ selpli terms args . be2poi

selpli :: [(Tag, Sumti)] -> [Sumti] -> Sumti -> Maybe Minde
selpli terms args (LO (Brivla "penbi") (Just (POI bridi))) = do
	peb <- penbi args bridi
	return $ if (Time ["ba"], KU) `elem` terms
		then peb
		else MIDYSTE [peb, PILNOLOPENBI]
selpli terms _ (LO (Brivla "penbi") Nothing)
	| (Time ["ba"], KU) `elem` terms = return $ MIDYSTE []
	| otherwise = return PILNOLOPENBI
selpli _ args (LO (Brivla "burcu") (Just (POI bridi))) = burcu args bridi
selpli terms args (Relative sp r) = apply args sp $ \pb -> case pb of
	LO (Brivla "penbi") _ -> selpli terms args (LO (Brivla "penbi") (Just r))
	LO (Brivla "burcu") _ -> selpli terms args (LO (Brivla "burcu") (Just r))
	_ -> fail "selpli: bad selpli"
selpli _ _ sp = return $ SRERA $ "selpli: unknown selpli " ++ show sp

penbi :: [Sumti] -> Text -> Maybe Minde
penbi _ (Bridi (Brivla skari) []) = pebyska skari
penbi args (Bridi (Brivla "penbi") [(FA 2, skari)]) = applyLO args skari pebyska
penbi args (Bridi (ME skari) []) = applyLO args skari pebyska
penbi args (Bridi (Brivla "cisni") [(FA 2, cisnysu'i)]) =
	apply args cisnysu'i $ \cs -> case cs of
		LI (Number cisnyna'u) -> return $ PEBYCISNI cisnyna'u
		_ -> fail "bad"
penbi _ relative = return $ SRERA $ "penbi: no such penbi" ++ show relative

pebyska :: String -> Maybe Minde
pebyska skari = uncurry3 PEBYSKA <$> lookup skari skaste

burcu :: [Sumti] -> Text -> Maybe Minde
burcu _ (Bridi (Brivla skari) []) = burska skari
burcu args (Bridi (ME skari) []) = applyLO args skari burska
burcu args (Bridi (SE 2 (Brivla "skari")) [(FA 1, skari)]) =
	applyLO args skari burska
burcu args (Bridi (Brivla "skari") [(FA 2, skari)]) =
	applyLO args skari burska
burcu _ relative = return $ SRERA $ "burcu: no such burcu" ++ show relative

burska :: String -> Maybe Minde
burska skari = uncurry3 BURSKA <$> lookup skari skaste

applyLO :: Monad m => [Sumti] -> Sumti -> (String -> m a) -> m a
applyLO args sumti cmd = apply args sumti $ \su'i -> case su'i of
	LO (Brivla lerfu) Nothing -> cmd lerfu
	_ -> fail $ "applyLO: bad sumti " ++ show su'i

be2poi :: Sumti -> Sumti
be2poi (LO (BE selbri (SFIhO modal sumti)) Nothing) =
	LO selbri $ Just $ POI $ Bridi modal [(FA 1, sumti)]
be2poi (LO (BE selbri sumti) Nothing) =
	LO selbri $ Just $ POI $ Bridi selbri [(FA 2, sumti)]
be2poi sumti = sumti

cisni terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	csn <- lookup (FA 2) terms
	apply args csn $ \c -> case c of
		LI (Number n) -> return $ CISNI n
		_ -> return $ SRERA $ "cisni: " ++ show terms

cisnygau terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	LO (Brivla "foldi") _ <- lookup (FA 2) terms
	csn <- lookup (FA 3) terms
	apply args csn $ \sk -> case sk of
		LI (JOhI [Number x, Number y]) -> return $ CISNYGAUFOLDI x y
		_ -> return $ SRERA $ show terms ++ " " ++ show args

viska terms _ = do
	KOhA "ko" <- lookup (FA 2) terms
	return VISKA

rapli terms args = do
	fasnu <- lookup (FA 1) terms
	namcu <- lookup (FA 2) terms
	apply2 args fasnu namcu $ \fau nac -> case (fau, nac) of
		(LO (NU f) _, LI (Number n)) ->
			return $ MIDYSTE $ replicate (round n) (jmi f [])
		_ -> return $ SRERA $ show terms

xruti terms _ = do
	KOhA "ko" <- lookup (FA 1) terms
	return XRUTI

morji terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	GOI lerfu duhu <- lookup (FA 2) terms
	apply2 args lerfu duhu $ \ler d -> case (ler, d) of
		(LerfuString cmene, LO (NU fasnu) _) ->
			return $ MORJI cmene $ jmi fasnu
		_ -> return $ SRERA $ "morji: " ++ show terms

gasnu terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	LerfuString cmene <- lookup (FA 2) terms
	return $ GASNU cmene args

rejgau terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	cmene <- lookup (BAI 1 "me'e") terms
	setai <- lookup (BAI 2 "tai") terms
	apply2 args cmene setai $ \cme st -> case (cme, st) of
		(ZOI fp, LA (Right (Brivla "cakyrespa"))) ->
			return $ REJGAUSETAICAK fp
		(ZOI fp, LA (Left "syvygyd")) ->
			return $ REJGAUSETAISVG fp
		(ZOI fp, LA (Left "lojban")) ->
			return $ REJGAUSETAIJBO fp
		_ -> return $ SRERA $ "rejgau: " ++ show setai

tcidu terms args = do
	KOhA "ko" <- lookup (FA 1) terms
	setai <- lookup (BAI 2 "tai") terms
	apply args setai $ \st -> case st of
		(LA (Right (Brivla "cakyrespa"))) -> do
			sumti <- lookup (FA 2) terms
			fps <- apply args sumti $ \smt -> case smt of
				LA (Right (ME fps)) -> return fps
				LAhE fps -> return fps
				_ -> fail "tcidu: bad"
			apply args fps $ \smt -> case smt of
				ZOI fp -> return $ TCIDU fp
				_ -> fail "tcidu: bad"
		(LA (Left "lojban")) -> do
			sumti <- lookup (FA 2) terms
			fps <- apply args sumti $ \smt -> case smt of
				LA (Right (ME fps)) -> return fps
				LAhE fps -> return fps
				_ -> fail "tcidu: bad"
			apply args fps $ \smt -> case smt of
				ZOI fp -> return $ TCIDUSETAIJBO fp
				_ -> fail "tcidu: bad"
		_ -> fail "tcidu: bad"

napilno terms _ = do
	KOhA "ko" <- lookup (FA 1) terms
	return NAPILNOLOPENBI

naviska terms _ = do
	KOhA "ko" <- lookup (FA 2) terms
	return NAVISKA
