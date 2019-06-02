module Latin.Main where

import Prelude
import Latin.Types (Agreement(..), Aspect(..), Conjugation(..), Mood(..), Numerus(..), Person(..), Tense(..), VerbSpec, VerbStructure, Vowel, getNumber, getPerson, renderVerbStructure, renderVerb, renderVowel, setNumber, setPerson, runRuleVerbStructure, sepIf)
import Latin.Data (initVS, phRules', run, spellouts')

import Partial.Unsafe (unsafePartial)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldMap, foldl)
import Control.Plus (empty, (<|>))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver as HV
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.DOM.ParentNode (QuerySelector(..))

data Action
  = SetSpec VerbSpec

type State =
  { spec :: VerbSpec
  }

component :: H.Component HH.HTML (Const Void) Unit Void Aff
component = H.mkComponent
  { initialState: const
    { spec:
      { root: "laud"
      , verb: Just CI
      , aspect: Nothing
      , tense: Just TPres
      , mood: Nothing
      , agreement: Just (Agreement P1 Plural)
      }
    }
  , render, eval
  } where
    render { spec } =
      let
        setSpec f = pure $ SetSpec $ f spec
        hrenderVS :: forall f. Foldable f => VerbStructure f -> HH.HTML _ _
        hrenderVS vs =
          let
            nullify "" = "∅"
            nullify s = s

            hrenderThemed ::
              forall a. Show a =>
              Maybe { feat :: f a, head :: String, theme :: Array Vowel } ->
              Array { title :: String, value :: String }
            hrenderThemed = maybe empty
              \t ->
                let
                  title = nullify (foldMap show t.feat) <> "+TH"
                  value =
                    nullify t.head <> "+" <>
                    nullify (foldMap renderVowel t.theme)
                in
                  [ { title: "", value: "‑" } ] <|>
                  [ { title, value } ]

            cols = [ { title: "√Root", value: vs.root } ]
              <|> hrenderThemed vs.verb
              <|> hrenderThemed vs.aspect
              <|> hrenderThemed vs.tense
              <|> hrenderThemed vs.mood
              <|> flip (maybe empty) vs.agreement
                \t ->
                  let
                    title = nullify (foldMap show t.feat)
                    value = nullify t.suffix
                  in
                    [ { title: "", value: "‑" } ] <|>
                    [ { title, value } ]

            row f =
              HH.tr_ $ HH.td_ <<< pure <<< HH.text <<< f <$> cols
          in
            HH.table_
              [ HH.thead_ [ HH.text (renderVerbStructure vs) ]
              , row _.title
              , row _.value
              ]
        radio :: forall a. Eq a =>
          String ->
          (VerbSpec -> a) -> (VerbSpec -> a -> VerbSpec) ->
          Array { name :: String, val :: a, dis :: Boolean } -> HH.HTML _ _
        radio group get set vals =
          HH.ul [ HP.class_ (H.ClassName "opts") ] $
            vals <#> \{ name, val, dis } ->
              HH.li_ $ pure $ HH.label_
                [ HH.input
                  [ HP.type_ HP.InputRadio
                  , HP.name group
                  , HP.checked (get spec == val)
                  , HP.disabled dis
                  , HE.onClick \_ -> setSpec (flip set val)
                  ]
                , HH.text name
                ]
        example root conj =
          let new = spec { root = root, verb = Just conj }
          in HH.button
            [ HE.onClick \_ -> Just (SetSpec new) ]
            [ HH.text $ (run new).final ]

        spell (Tuple spelling spelled) (Tuple name rule) =
          let
            new = rule spelled
          in if spelled == new then Tuple spelling spelled
            else Tuple (spelling <|> [Tuple name new]) new
        Tuple spelling spelled = foldl spell (Tuple empty (initVS spec)) spellouts'
        pronounce (Tuple pronouncing pronounced) (Tuple name rule) =
          let
            new = runRuleVerbStructure rule pronounced
          in if pronounced == new then Tuple pronouncing pronounced
            else Tuple (pronouncing <|> [Tuple name new]) new
        Tuple pronouncing pronounced = foldl pronounce (Tuple empty spelled) phRules'
      in HH.div_
        [ HH.div_
          [ HH.text "Examples: "
          , example "laud" CI
          , example "mon" CII
          , example "dūc" CIII
          , example "cap" CIIIi
          , example "aud" CIV
          ]
        , HH.input
          [ HP.value spec.root
          , HE.onValueInput \root -> setSpec _ { root = root }
          ]
        , radio "conjugation" _.verb _ { verb = _ }
          [ { name: "I", val: Just CI, dis: false }
          , { name: "II", val: Just CII, dis: false }
          , { name: "III", val: Just CIII, dis: false }
          , { name: "III(i)", val: Just CIIIi, dis: false }
          , { name: "IV", val: Just CIV, dis: false }
          , { name: "∅ Athematic", val: Nothing, dis: false }
          ]
        , radio "tense" _.tense _ { tense = _ }
          [ { name: "∅ Infinitive", val: Nothing, dis: true }
          , { name: "T[pres] Present", val: Just TPres, dis: false }
          , { name: "T[past] Past", val: Just TPast, dis: false }
          , { name: "T[fut] Future", val: Just TFut, dis: spec.mood == Just MSubj }
          ]
        , radio "aspect" _.aspect _ { aspect = _ }
          [ { name: "∅ Non-perfect", val: Nothing, dis: false }
          , { name: "ASP[perf] Perfect", val: Just ASPPerf, dis: false }
          ]
        , radio "mood" _.mood _ { mood = _ }
          [ { name: "∅ Indicative", val: Nothing, dis: false }
          , { name: "M[subj] Subjunctive", val: Just MSubj, dis: spec.tense == Just TFut }
          ]
        , radio "person" (_.agreement >>> map getPerson) (\r p -> r { agreement = flip setPerson <$> p <*> r.agreement })
          [ { name: "1st", val: Just P1, dis: false }
          , { name: "2nd", val: Just P2, dis: false }
          , { name: "3rd", val: Just P3, dis: false }
          ]
        , radio "number" (_.agreement >>> map getNumber) (\r p -> r { agreement = flip setNumber <$> p <*> r.agreement })
          [ { name: "Singular", val: Just Singular, dis: false }
          , { name: "Plural", val: Just Plural, dis: false }
          ]
        , HH.p_
          [ HH.b_ [ HH.text "Derivation: " ]
          , HH.text $ spec.root
              <> sepIf "-" (foldMap show spec.verb)
              <> sepIf "-" (foldMap show spec.aspect)
              <> sepIf "-" (foldMap show spec.tense)
              <> sepIf "-" (foldMap show spec.mood)
              <> sepIf "-" (foldMap show spec.agreement)
          , HH.text " ⟶ "
          , HH.text $ renderVerbStructure spelled
          , HH.text " ⟶ "
          , HH.text $ renderVerbStructure pronounced
          , HH.text " ("
          , HH.b_ [ HH.text $ renderVerb pronounced ]
          , HH.text ")"
          ]
        , HH.div_ $ spelling >>= \(Tuple rule new) ->
          [ HH.p_ [ HH.text rule ]
          , hrenderVS new
          ]
        , HH.p_ [ HH.b_ [ HH.text "After spellout of morphemes:" ] ]
        , hrenderVS spelled
        , HH.div_ $ pronouncing >>= \(Tuple rule new) ->
          [ HH.p_ [ HH.text rule ]
          , hrenderVS new
          ]
        , HH.p_ [ HH.b_ [ HH.text "After phonological changes:" ] ]
        , hrenderVS pronounced
        ]
    eval :: H.HalogenQ (Const Void) Action Unit ~> H.HalogenM State Action () Void Aff
    eval = case _ of
      (H.Initialize a) -> pure a
      (H.Finalize a) -> pure a
      (H.Receive _ a) -> pure a
      (H.Query _ a) -> pure (a unit)
      (H.Action act a) -> a <$ case act of
        SetSpec spec -> H.modify_ _ { spec = spec }

main :: Effect Unit
main = launchAff_ $ unsafePartial do
  HA.awaitLoad
  Just e <- HA.selectElement (QuerySelector "#interactive")
  HV.runUI component unit e
