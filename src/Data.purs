module Latin.Data where

import Latin.Types
import Prelude

import Control.Comonad (extract)
import Control.MonadZero (empty, guard)
import Data.Foldable (any, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Tuple (Tuple(..))

initVS ::
  VerbSpec -> VerbStructure Handled
initVS vs =
  let
    emptyThemed :: forall a. a -> Themed (Handled a)
    emptyThemed = Just <<< Tuple false >>>
      { feat: _, head: "", theme: empty }
  in
    { root: vs.root
    , verb: vs.verb >>= emptyThemed
    , aspect: vs.aspect >>= emptyThemed
    , tense: vs.tense >>= emptyThemed
    , mood: vs.mood >>= emptyThemed
    , agreement: vs.agreement >>=
        Just <<< Tuple false >>>
          { feat: _, suffix: "" }
    }

laudavissemus :: VerbSpec
laudavissemus =
  { root: "laud"
  , verb: Just CI
  , aspect: Just ASPPerf
  , tense: Just TPast
  , mood: Just MSubj
  , agreement: Just (Agreement P1 Plural)
  }

laudaverunt :: VerbSpec
laudaverunt =
  { root: "laud"
  , verb: Just CI
  , aspect: Just ASPPerf
  , tense: Nothing
  , mood: Nothing
  , agreement: Just (Agreement P3 Plural)
  }

run ::
  { root :: String
  , verb :: Maybe Conjugation
  , aspect :: Maybe Aspect
  , tense :: Maybe Tense
  , mood :: Maybe Mood
  , agreement :: Maybe Agreement } ->
  { spelled :: String, final :: String }
run = { spelled: _, final: _ }
  <$> (initVS
    >>> spellout
    >>> renderVerbStructure)
  <*> (initVS
    >>> spellout
    >>> phRule
    >>> renderVerbStructure)

-- Spell out rules

type VSField a =
  { get :: VerbStructure Handled -> Themed (Handled a)
  , set :: VerbStructure Handled -> Themed (Handled a) -> VerbStructure Handled
  }
_verb :: VSField Conjugation
_verb = { get: _.verb, set: _ { verb = _ } }
isFront :: Conjugation -> Boolean
isFront = conjugationToTheme >>> vowelToFeatures >>> Set.member Front
_aspect :: VSField Aspect
_aspect = { get: _.aspect, set: _ { aspect = _ } }
isPerf :: Aspect -> Boolean
isPerf = case _ of
  ASPPerf -> true
_tense :: VSField Tense
_tense = { get: _.tense, set: _ { tense = _ } }
isPres :: Tense -> Boolean
isPres = case _ of
  TPres -> true
  _ -> false
isPast :: Tense -> Boolean
isPast = case _ of
  TPast -> true
  _ -> false
isFut :: Tense -> Boolean
isFut = case _ of
  TFut -> true
  _ -> false
_mood :: VSField Mood
_mood = { get: _.mood, set: _ { mood = _ } }
isSubj :: Mood -> Boolean
isSubj = case _ of
  MSubj -> true

getField :: forall a. VSField a -> VerbStructure Handled -> Maybe a
getField field = getField' field >>> map extract

getField' :: forall a. VSField a -> VerbStructure Handled -> Maybe (Handled a)
getField' field vs =
  _.feat <$> field.get vs

noField :: forall a. VSField a -> VerbStructure Handled -> Boolean
noField field = getField field >>> isNothing

matchField :: forall a. VSField a -> (a -> Boolean) -> VerbStructure Handled -> Boolean
matchField field pred = getField field >>> any pred

spellfeat :: forall a.
  VSField a ->
  (VerbStructure Handled -> a -> Maybe (Themed (Erased a))) ->
  SORule
spellfeat field spellthis vs = fromMaybe vs do
  Tuple handled feat <- getField' field vs
  guard $ not handled
  new <- spellthis vs feat
  pure $ field.set vs $ new <#> _ { feat = Tuple true feat }

soEnv :: (VerbStructure Handled -> Boolean) -> SORule -> SORule
soEnv match rule vs =
  if match vs then rule vs else vs

soEnv' :: forall a.
  (VerbStructure Handled -> Boolean) ->
  (VerbStructure Handled -> a -> Maybe (Themed (Erased a))) ->
  (VerbStructure Handled -> a -> Maybe (Themed (Erased a)))
soEnv' match rule vs a =
  if match vs then rule vs a else Nothing

spellouts :: Array SORule
spellouts =
  [ spellfeat _verb \_ conj ->
      Just $ Just
        { feat: unit
        , head: ""
        , theme: pure $ case conj of
            CI -> Vowel Va true
            CII -> Vowel Ve true
            CIII -> Vowel V_i false
            CIIIi -> Vowel Vi false
            CIV -> Vowel Vi true
        }
  -- 1. ASP[perf] ⟶ /v/ (/u/), /s/, /∅/ + /ɨ/
  , spellfeat _aspect \_ -> case _ of
      ASPPerf -> Just $ Just
        { feat: unit
        , head: "v"
        , theme: pure $ Vowel V_i false
        }
  -- 2. T[pres] ⟶ /∅/ (T node deleted)
  , spellfeat _tense \_ -> case _ of
      -- Delete T[pres] node
      TPres -> Just Nothing
      -- Do nothing
      _ -> Nothing
  -- 3. T[past] ⟶ /s/+/∅/ in env. ASP[perf]__M[subj]
  , soEnv (matchField _aspect isPerf) $
    soEnv (matchField _mood   isSubj) $
    spellfeat _tense \_ -> case _ of
      TPast -> Just $ Just
        { feat: unit
        , head: "s"
        , theme: empty
        }
      _ -> Nothing
  -- 4. T[past] ⟶ /∅/ in env. __M[subj]
  , soEnv (matchField _mood   isSubj) $
    spellfeat _tense \_ -> case _ of
      TPast -> Just $ Just
        { feat: unit
        , head: ""
        , theme: empty
        }
      _ -> Nothing
  -- 5. T[past] ⟶ /s/+/ā/ in env. ASP[perf]__
  , soEnv (matchField _aspect isPerf) $
    spellfeat _tense \_ -> case _ of
      TPast -> Just $ Just
        { feat: unit
        , head: "s"
        , theme: pure $ Vowel Va true
        }
      _ -> Nothing
  -- 6. T[past] ⟶ /ēb/+/ā/ in env. v+TH[+front]__
  , soEnv (matchField _verb isFront) $
    spellfeat _tense \_ -> case _ of
      TPast -> Just $ Just
        { feat: unit
        , head: "ēb"
        , theme: pure $ Vowel Va true
        }
      _ -> Nothing
  -- 7. T[past] ⟶ /b/+/ā/ in env. v+TH[-front]__
  , soEnv (matchField _verb isFront) $
    spellfeat _tense \_ -> case _ of
      TPast -> Just $ Just
        { feat: unit
        , head: "b"
        , theme: pure $ Vowel Va true
        }
      _ -> Nothing
  -- 8. T[fut] ⟶ /s/+/ɨ/ in env. ASP[perf]__
  , soEnv (matchField _aspect isPerf) $
    spellfeat _tense \_ -> case _ of
      TFut -> Just $ Just
        { feat: unit
        , head: "s"
        , theme: pure $ Vowel V_i false
        }
      _ -> Nothing
  -- 9. T[fut] ⟶ /∅/+/ē/ in env. v+TH[+front]__
  , soEnv (matchField _verb isFront) $
    spellfeat _tense \_ -> case _ of
      TFut -> Just $ Just
        { feat: unit
        , head: ""
        , theme: pure $ Vowel Ve true
        }
      _ -> Nothing
  -- 10. T[fut] ⟶ /b/+/ɨ/ in env. v+TH[-front]__
  , soEnv (matchField _verb isFront) $
    spellfeat _tense \_ -> case _ of
      TFut -> Just $ Just
        { feat: unit
        , head: "b"
        , theme: pure $ Vowel V_i false
        }
      _ -> Nothing
  -- 11. M[subj] ⟶ /s/ + /ē/ in env. T[past]__
  , soEnv (matchField _tense isPast) $
    spellfeat _mood \_ -> case _ of
      MSubj -> Just $ Just
        { feat: unit
        , head: "s"
        , theme: pure $ Vowel Ve true
        }
  -- 12. M[subj] ⟶ /s/ + /ī/ in env. ASP[perf]__
  , soEnv (matchField _aspect isPerf) $
    spellfeat _mood \_ -> case _ of
      MSubj -> Just $ Just
        { feat: unit
        , head: "s"
        , theme: pure $ Vowel Vi true
        }
  -- Agreement
  , \vs -> fromMaybe vs $ vs.agreement <#> \{ feat: Tuple _ agr } ->
      let
        suffix
          | (vs.aspect <#> _.feat >>> extract) == Just ASPPerf
          , vs.tense == Nothing
          , vs.mood == Nothing =
            case agr of
              Agreement P1 Singular -> "ī"
              Agreement P2 Singular -> "stī"
              Agreement P3 Singular -> "t"
              Agreement P1 Plural   -> "mus"
              Agreement P2 Plural   -> "stis"
              Agreement P3 Plural   -> "ērunt"
        suffix = case agr of
          Agreement P1 Singular ->
            if (vs.aspect == Nothing &&
                vs.tense == Nothing &&
                vs.mood == Nothing) ||
                (vs.tense <#> _.feat >>> extract) == Just TFut
              then "ō"
              else "m"
          Agreement P2 Singular -> "s"
          Agreement P3 Singular -> "t"
          Agreement P1 Plural   -> "mus"
          Agreement P2 Plural   -> "tis"
          Agreement P3 Plural   -> "nt"
      in vs { agreement = Just { feat: Tuple true agr, suffix }}
  ]

spellout :: SORule
spellout = unwrap $ unwrap $ foldMap (Dual <<< Endo) spellouts

-- Helpers for a DSL for phonetic rules

vocalic :: Phoneme -> Boolean
vocalic (PhVowel _) = true
vocalic _ = false

prevocalic :: Focused Focus Phoneme -> Boolean
prevocalic = _.after >>> any vocalic

postvocalic :: Focused Focus Phoneme -> Boolean
postvocalic = _.before >>> any vocalic

intervocalic :: Focused Focus Phoneme -> Boolean
intervocalic = prevocalic && postvocalic

inEnv ::
  (Focused Focus Phoneme -> Boolean) ->
  PhRule -> PhRule
inEnv pred trans foc | pred foc = trans foc
inEnv _ _ foc = Just foc.focused

onVowel :: (Vowel -> Maybe Vowel) -> PhRule
onVowel trans foc@{ focused: PhVowel v } =
  PhVowel <$> trans v
onVowel _ foc = Just foc.focused

onConsonant :: (Consonant -> Maybe Consonant) -> PhRule
onConsonant trans foc@{ focused: PhConsonant c } =
  PhConsonant <$> trans c
onConsonant _ foc = Just foc.focused

-- Phonetic rules

phRules :: Array PhRule
phRules =
  [ centralVowelDeletion
  , rhotacism
  , shortening
  , highVowelFronting
  ]

phRule :: SORule
phRule = unwrap $ unwrap $ foldMap (Dual <<< Endo <<< runRuleVerbStructure) phRules

-- Delete a central vowel before another vowel
centralVowelDeletion :: PhRule
centralVowelDeletion =
  inEnv prevocalic $
  onVowel $
  \v ->
    let
      target = Map.fromFoldable
        [ Tuple Back false, Tuple Front false ]
    in if matchVowel target v then Nothing else Just v

rhotacism :: PhRule
rhotacism =
  inEnv intervocalic $
  onConsonant $
    case _ of
      Consonant "s" -> Just (Consonant "r")
      c -> Just c

shortening :: PhRule
shortening =
  inEnv prevocalic $
  onVowel $
    Just <<< vowelOverFeatures (Set.delete Long)

highVowelFronting :: PhRule
highVowelFronting =
  {-
  onVowel $ \v ->
    let
      target = Map.fromFoldable
        [ Tuple Back false, Tuple High true ]
    in Just $ v #
      if matchVowel target v
        then vowelOverFeatures (Set.delete Long)
        else identity
  -}
  onVowel $ \(Vowel sound long) -> Just
    if sound == V_i
      then Vowel Vi long
      else Vowel sound long
