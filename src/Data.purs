module Latin.Data where

import Latin.Types
import Prelude

import Control.MonadZero (guard)
import Data.Foldable (all, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

-- Spell out rules

spellfeat :: forall a.
  (VerbStructure Handled -> Themed (Handled (Maybe a))) ->
  (VerbStructure Handled -> Themed (Handled (Maybe a)) -> VerbStructure Handled) ->
  (a -> Maybe (Themed (Erased a))) ->
  SORule
spellfeat get set spellthis vs = fromMaybe vs do
  foc <- get vs
  Tuple handled feat <- sequence foc.feat
  guard handled
  new <- spellthis feat
  pure $ set vs $ new <#> _ { feat = Tuple true (Just feat) }

spellouts :: Array SORule
spellouts =
  [ spellfeat _.aspect _ { aspect = _ }
    \ASPPerf -> Just $ Just
      { feat: unit
      , head: "v"
      , theme: [Vowel V_i false]
      }
  ]

spellout :: SORule
spellout = unwrap $ foldMap Endo spellouts

-- Helpers for a DSL for phonetic rules

vocalic :: Phoneme -> Boolean
vocalic (PhVowel _) = true
vocalic _ = false

prevocalic :: Focused Focus Phoneme -> Boolean
prevocalic = _.after >>> all vocalic

postvocalic :: Focused Focus Phoneme -> Boolean
postvocalic = _.after >>> all vocalic

intervocalic :: Focused Focus Phoneme -> Boolean
intervocalic = prevocalic && postvocalic

env ::
  (Focused Focus Phoneme -> Boolean) ->
  PhRule -> PhRule
env pred trans foc | pred foc = trans foc
env _ _ foc = Just foc.focused

onVowel :: (Vowel -> Maybe Vowel) -> PhRule
onVowel trans foc@{ focused: PhVowel v } =
  PhVowel <$> trans v
onVowel _ foc = Just foc.focused

onConsonant :: (Consonant -> Maybe Consonant) -> PhRule
onConsonant trans foc@{ focused: PhConsonant c } =
  PhConsonant <$> trans c
onConsonant _ foc = Just foc.focused

-- Phonetic rules

-- Delete a central vowel before another vowel
centralVowelDeletion :: PhRule
centralVowelDeletion =
  env prevocalic $
  onVowel $
  \v ->
    let
      target = Map.fromFoldable
        [ Tuple Back false, Tuple Front false ]
    in if matchVowel target v then Nothing else Just v

rhotacism :: PhRule
rhotacism =
  env intervocalic $
  onConsonant $
    case _ of
      Consonant "s" -> Just (Consonant "r")
      c -> Just c

shortening :: PhRule
shortening =
  env prevocalic $
  onVowel $
    Just <<< vowelOverFeatures (Set.delete Long)

highVowelFronting :: PhRule
highVowelFronting =
  onVowel $ \v ->
    let
      target = Map.fromFoldable
        [ Tuple Back false, Tuple High true ]
    in Just $ v #
      if matchVowel target v
        then vowelOverFeatures (Set.delete Long)
        else identity
