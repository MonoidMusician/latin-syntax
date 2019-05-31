module Latin.Types where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, oneOfMap)
import Data.FoldableWithIndex (allWithIndex)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (dimap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple)

-- Features used to describe vowels
data VowelFeature
  = Front
  -- N.B. Back = Round
  | Back
  | High
  | Low
  | Long

derive instance eqVowelFeature :: Eq VowelFeature
derive instance ordVowelFeature :: Ord VowelFeature

-- The vowel sounds, including barred i as V_i
data VowelSound
  = Va
  | Ve
  | Vi
  | V_i
  | Vo
  | Vu

derive instance eqVowelSound :: Eq VowelSound
derive instance ordVowelSound :: Ord VowelSound

-- A vowel is a combination of a vowel sound and a length
data Vowel
  = Vowel VowelSound Boolean

derive instance eqVowel :: Eq Vowel
derive instance ordVowel :: Ord Vowel

parseVowel :: String -> Maybe Vowel
parseVowel = case _ of
  "ā" -> Just $ Vowel Va true
  "ē" -> Just $ Vowel Ve true
  "ī" -> Just $ Vowel Vi true
  "ɨ̄" -> Just $ Vowel V_i true
  "ō" -> Just $ Vowel Vo true
  "ū" -> Just $ Vowel Vu true
  "a" -> Just $ Vowel Va false
  "e" -> Just $ Vowel Ve false
  "i" -> Just $ Vowel Vi false
  "ɨ" -> Just $ Vowel V_i false
  "o" -> Just $ Vowel Vo false
  "u" -> Just $ Vowel Vu false
  _ -> Nothing

renderVowel :: Vowel -> String
renderVowel (Vowel sound long) = case sound, long of
  Va, true -> "ā"
  Ve, true -> "ē"
  Vi, true -> "ī"
  V_i, true -> "ɨ̄"
  Vo, true -> "ō"
  Vu, true -> "ū"
  Va, false -> "a"
  Ve, false -> "e"
  Vi, false -> "i"
  V_i, false -> "ɨ"
  Vo, false -> "o"
  Vu, false -> "u"

-- If the container contains no vowels, return the null symbol
renderVowelsNull :: forall f. Foldable f => f Vowel -> String
renderVowelsNull = fromMaybe "∅" <<< foldMap (Just <<< renderVowel)

-- What features a vowel has
vowelToFeatures :: Vowel -> Set VowelFeature
vowelToFeatures (Vowel sound long) =
  (if long then Set.singleton Long else Set.empty) <>
  Set.fromFoldable
    case sound of
        Va -> [Low]
        Ve -> [Front]
        Vi -> [Front, High]
        V_i -> [High]
        Vo -> [Back]
        Vu -> [Back, High]

-- Guess the vowel sound from features
featuresToVowel :: Set VowelFeature -> Vowel
featuresToVowel feats = Vowel sound (Set.member Long feats)
  where
    sound | Set.member Front feats =
      if Set.member High feats then Vi else Ve
    sound | Set.member Back feats =
      if Set.member High feats then Vu else Vo
    sound | Set.member High feats = V_i
    sound = Va

vowelOverFeatures :: (Set VowelFeature -> Set VowelFeature) -> (Vowel -> Vowel)
vowelOverFeatures = dimap vowelToFeatures featuresToVowel

-- We do not care about consonant features, so just use a string
newtype Consonant
  = Consonant String

derive instance newtypeConsonant :: Newtype Consonant _
derive instance eqConsonant :: Eq Consonant
derive instance ordConsonant :: Ord Consonant

renderConsonant :: Consonant -> String
renderConsonant = unwrap

data Phoneme
  = PhVowel Vowel
  | PhConsonant Consonant

derive instance eqPhoneme :: Eq Phoneme
derive instance ordPhoneme :: Ord Phoneme

parsePhoneme :: String -> Phoneme
parsePhoneme s = case parseVowel s of
  Just v -> PhVowel v
  Nothing -> PhConsonant (Consonant s)

parsePhonemes :: String -> Array Phoneme
parsePhonemes = String.toCodePointArray >>>
  map (String.singleton >>> parsePhoneme)

renderPhoneme :: Phoneme -> String
renderPhoneme (PhVowel v) = renderVowel v
renderPhoneme (PhConsonant v) = renderConsonant v

-- For every item in the map, check that it is checked in the set
-- (if true in the map) or unechecked (if false).
match :: forall k. Ord k => Map k Boolean -> Set k -> Boolean
match = allWithIndex \k v -> eq v <<< Set.member k

matchVowel :: Map VowelFeature Boolean -> Vowel -> Boolean
matchVowel feats = match feats <<< vowelToFeatures

-- Tenses according to their conventional interpretation in Latin
-- grammar.
data Tempus
  = Present
  | Imperfect
  | Future
  | Perfect
  | Pluperfect
  | FuturePerfect

derive instance eqTempus :: Eq Tempus
derive instance ordTempus :: Ord Tempus

-- Moods according to their conventional interpretation
data Modus
  = Indicative
  | Subjunctive

derive instance eqModus :: Eq Modus
derive instance ordModus :: Ord Modus

-- Features for T
data Tense
  = TPres
  | TPast
  | TFut

derive instance eqTense :: Eq Tense
derive instance ordTense :: Ord Tense

-- Features for ASP
data Aspect
  = ASPPerf

derive instance eqAspect :: Eq Aspect
derive instance ordAspect :: Ord Aspect

-- Features for M
data Mood
  = MSubj

derive instance eqMood :: Eq Mood
derive instance ordMood :: Ord Mood

data Person = P1 | P2 | P3

derive instance eqPerson :: Eq Person
derive instance ordPerson :: Ord Person

data Numerus = Singular | Plural

derive instance eqNumerus :: Eq Numerus
derive instance ordNumerus :: Ord Numerus

data Agreement = Agreement Person Numerus

derive instance eqAgreement :: Eq Agreement
derive instance ordAgreement :: Ord Agreement

getPerson :: Agreement -> Person
getPerson (Agreement p _) = p

getNumber :: Agreement -> Numerus
getNumber (Agreement _ n) = n

setPerson :: Agreement -> Person -> Agreement
setPerson (Agreement _ n) p = Agreement p n

setNumber :: Agreement -> Numerus -> Agreement
setNumber (Agreement p _) n = Agreement p n

data Morphophoneme
  = MorphPh Phoneme
  | MorphT Tense
  | MorphASP Aspect
  | MorphM Mood
  | MorphAGR Agreement

derive instance eqMorphophoneme :: Eq Morphophoneme
derive instance ordMorphophoneme :: Ord Morphophoneme

data Conjugation
  = CI
  | CII
  | CIII
  | CIIIi
  | CIV

derive instance eqConjugation :: Eq Conjugation
derive instance ordConjugation :: Ord Conjugation

conjugationToTheme :: Conjugation -> Vowel
conjugationToTheme = case _ of
  CI -> Vowel Va true
  CII -> Vowel Ve true
  CIII -> Vowel V_i false
  CIIIi -> Vowel Vi false
  CIV -> Vowel Vi true

type Themed a =
    Maybe { feat :: a, head :: String, theme :: Array Vowel }

renderThemed :: forall a. Themed a -> String
renderThemed = foldMap renderThemed'

renderThemed_ :: forall a. Themed a -> String
renderThemed_ = maybe ""
  \t ->
    let
      sepIf _ "" = ""
      sepIf sep s = sep <> s
    in sepIf "-" t.head <> sepIf "+" (foldMap renderVowel t.theme)

renderThemed' :: forall a. { feat :: a, head :: String, theme :: Array Vowel } -> String
renderThemed' t = t.head <> foldMap renderVowel t.theme

type VerbSpec =
  { root :: String
  , verb :: Maybe Conjugation
  , aspect :: Maybe Aspect
  , tense :: Maybe Tense
  , mood :: Maybe Mood
  , agreement :: Maybe Agreement
  }

type Featured a = a
type Erased a = Unit
type Handled = Tuple Boolean
type VerbStructure f =
  { root :: String
  , verb :: Themed (f Conjugation)
  , aspect :: Themed (f Aspect)
  , tense :: Themed (f Tense)
  , mood :: Themed (f Mood)
  , agreement :: Maybe { feat :: f Agreement, suffix :: String }
  }

renderVerbStructure :: forall f. VerbStructure f -> String
renderVerbStructure vs = vs.root
  <> renderThemed_ vs.verb
  <> renderThemed_ vs.aspect
  <> renderThemed_ vs.tense
  <> renderThemed_ vs.mood
  <> case vs.agreement of
    Nothing -> ""
    Just { suffix } -> "-" <> suffix

type Focus a = a
type Focused f a =
  { before :: Maybe a
  , focused :: f a
  , after :: Maybe a
  }
-- Phonetic rules
type PhRule = Focused Focus Phoneme -> Maybe Phoneme
-- Spell-out rules
type SORule = VerbStructure Handled -> VerbStructure Handled

-- Apply a rule to each character in a context
runRule :: PhRule ->
  { before :: String
  , focused :: String
  , after :: String
  } -> String
runRule rule foc = ret where
    prior =
      String.codePointAt (String.length foc.before - 1) foc.before <#>
        String.singleton >>> parsePhoneme
    last =
      String.codePointAt 0 foc.after <#>
        String.singleton >>> parsePhoneme
    foci = parsePhonemes foc.focused
    init =
      { prev: prior
      , accum: []
      , remaining: foci
      }
    folding r =
      case Array.uncons r.remaining of
        Nothing -> r
        Just { head, tail } ->
          let
            next = rule
              { before: r.prev
              , focused: head
              , after: Array.head tail <|> last
              }
          in folding
            { prev: next <|> r.prev
            , accum: r.accum <|> oneOfMap pure next
            , remaining: tail
            }
    res = folding init
    ret = foldMap renderPhoneme res.accum

-- Apply the rule to a themed pair
runRuleThemed :: forall a. PhRule ->
  { before :: String
  , focused :: Themed a
  , after :: String
  } -> Themed a
runRuleThemed rule foc = foc.focused <#> \focused ->
  let
    stheme = foldMap renderVowel focused.theme
    head = runRule rule
      { before: foc.before
      , focused: focused.head
      , after: stheme <> foc.after
      }
    theme = runRule rule
      { before: foc.before <> head
      , focused: stheme
      , after: foc.after
      } # String.toCodePointArray
      # oneOfMap (String.singleton >>> parseVowel >>> oneOfMap pure)
  in
    { feat: focused.feat
    , head, theme
    }

-- Apply the rule to each part of the verb structure
runRuleVerbStructure :: forall f. PhRule -> VerbStructure f -> VerbStructure f
runRuleVerbStructure rule vs =
  let
    -- Assume root is unchanged by the transformations
    root = vs.root
    verb = runRuleThemed rule
      { before:
          root
      , focused: vs.verb
      , after:
          renderThemed vs.aspect <>
          renderThemed vs.tense <>
          renderThemed vs.mood <>
          foldMap _.suffix vs.agreement
      }
    aspect = runRuleThemed rule
      { before:
          root <>
          renderThemed verb
      , focused: vs.aspect
      , after:
          renderThemed vs.tense <>
          renderThemed vs.mood <>
          foldMap _.suffix vs.agreement
      }
    tense = runRuleThemed rule
      { before:
          root <>
          renderThemed verb <>
          renderThemed aspect
      , focused: vs.tense
      , after:
          renderThemed vs.mood <>
          foldMap _.suffix vs.agreement
      }
    mood = runRuleThemed rule
      { before:
          root <>
          renderThemed verb <>
          renderThemed aspect <>
          renderThemed tense
      , focused: vs.mood
      , after:
          foldMap _.suffix vs.agreement
      }
    agreement = vs.agreement <#> \r -> r
      { suffix = runRule rule
        { before:
            root <>
            renderThemed verb <>
            renderThemed aspect <>
            renderThemed tense
        , focused: r.suffix
        , after:
            ""
        }
      }
  in { root, verb, aspect, tense, mood, agreement }
