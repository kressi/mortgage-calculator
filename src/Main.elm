module Main exposing (main)

import Browser
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Html exposing (..)
import List exposing (any)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type alias Model = {
  propertyVal : Int,
  ownResources : Int,
  loanAmt : Int,
  loanRateNoAmo : Float,
  loanRate : Float,
  amoDurationYears : Int,
  intrstRate : Float,
  intrstAmt : Int,
  maintRate : Float,
  maintAmt : Int,
  income : Int,
  amoAmt : Int,
  sustainabilityRate : Float }

init : Model
init = {
  propertyVal = 1000000,
  ownResources = 800000,
  loanAmt = 200000,
  loanRateNoAmo = 0.65,
  loanRate = 0.8,
  amoDurationYears = 15,
  intrstRate = 0.05,
  intrstAmt = 0,
  maintRate = 0.01,
  maintAmt = 0,
  income = 80000,
  amoAmt = 0,
  sustainabilityRate = 0  }

type Msg
  = PropertyVal String
  | OwnResources String
  | LoanAmt String
  | LoanRateNoAmo String
  | LoanRate String
  | AmoDurationYears String
  | IntrstRate String
  | IntrstAmt String
  | MaintRate String
  | MaintAmt String
  | Income String
  | AmoAmt String
  | Calculate

update : Msg -> Model -> Model
update msg model =
    case msg of
      PropertyVal propertyVal -> {
        model | propertyVal = Maybe.withDefault 0 (String.toInt propertyVal) }
        |> recalculate
      OwnResources ownResources -> {
        model | ownResources = Maybe.withDefault 0 (String.toInt ownResources) }
        |> recalculate
      Calculate -> model |> recalculate
      _ -> model

recalculate : Model -> Model
recalculate model = {
  model | loanAmt = loanAmt model.propertyVal model.ownResources,
          loanRate = loanRate model.propertyVal model.ownResources }

loanAmt : Int -> Int -> Int
loanAmt property resources = property - resources

loanRate : Int -> Int -> Float
loanRate property resources = toFloat (loanAmt property resources) / toFloat property

view : Model -> Html Msg
view model =
  div [] [ 
    viewField
      "Kaufpreis "
      (viewInputInt init.propertyVal model.propertyVal False PropertyVal),
    viewField
      "Eigenmittel "
      (viewInputInt init.ownResources model.ownResources False OwnResources),
    viewField
      "Einkommen (p.a.) "
      (viewInputInt init.income model.income False Income),

    viewField
      "Belehnung (%) "
      (viewInputFloat init.loanRate model.loanRate True LoanRate),
    viewField
      "Hypothek "
      (viewInputInt init.loanAmt model.loanAmt True LoanAmt),

    viewField
      "Tragbarkeit (%) "
      (text (String.fromFloat model.sustainabilityRate)),
    viewField
      "Hypothekarzinsen (%) "
      (viewInputFloat init.intrstRate model.intrstRate True IntrstRate),
    viewField
      "Hypothekarzinsen "
      (viewInputInt init.intrstAmt model.intrstAmt True IntrstAmt),
    viewField
      "Unterhalts- und Nebenkosten (%) "
      (viewInputFloat init.maintRate model.maintRate False MaintRate),
    viewField
      "Unterhalts- und Nebenkosten "
      (viewInputInt init.maintAmt model.maintAmt False MaintAmt),
    viewField
      "Belehnungswert nach Amartisation (%) "
      (viewInputFloat init.loanRateNoAmo model.loanRateNoAmo True LoanRateNoAmo),
    viewField
      "Amortisationsdauer (Jahre) "
      (viewInputInt init.amoDurationYears model.amoDurationYears True AmoDurationYears),
    viewField
      "Amortisationsbetrag "
      (viewInputInt init.amoAmt model.amoAmt True AmoAmt),
    div [] [ button [ onClick Calculate ] [ text "Calculate" ] ]
  ]

viewField : String -> (Html msg) -> Html msg
viewField t m = div [] [ text t, m ]

viewInput : String -> String -> Bool -> Bool -> (String -> msg) -> Html msg
viewInput p v isFloat isDisabled toMsg =
  input ([
    type_ "number",
    placeholder p,
    value v,
    onInput toMsg
  ] |> consIf isDisabled (disabled True)
    |> consIf isFloat (step "any") ) []

viewInputInt : Int -> Int -> Bool -> (String -> msg) -> Html msg
viewInputInt p v =
  viewInput (String.fromInt p) (String.fromInt v) False

viewInputFloat : Float -> Float -> Bool -> (String -> msg) -> Html msg
viewInputFloat p v =
  viewInput (String.fromFloat p) (String.fromFloat v) True

consIf : Bool -> a -> List a -> List a
consIf cond x xs =
    if cond then
        x :: xs
    else
        xs