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
  intrstRateSust : Float,
  intrstRateActual : Float }

init : Model
init = {
  propertyVal = 1000000,
  ownResources = 800000,
  loanAmt = 200000,
  loanRateNoAmo = 0.65,
  loanRate = 0.8,
  amoDurationYears = 15,
  intrstRateSust = 0.05,
  intrstRateActual = 0.01  }

type Msg
  = PropertyVal String
  | OwnResources String
  | LoanAmt String
  | LoanRateNoAmo String
  | LoanRate String
  | AmoDurationYears String
  | IntrstRateSust String
  | IntrstRateActual String
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
  model | loanAmt = model.propertyVal - model.ownResources }

view : Model -> Html Msg
view model =
  div [] [ 
    div [] [
      text "Property Value ",
      viewInputInt init.propertyVal model.propertyVal False PropertyVal
    ],
    div [] [
      text "Own Resources ",
      viewInputInt init.ownResources model.ownResources False OwnResources
    ],
    div [] [
      text "Loan Amount ",
      viewInputInt init.loanAmt model.loanAmt True LoanAmt
    ],
    div [] [
      text "Loan Rate w/o Amortization (1st)",
      viewInputFloat init.loanRateNoAmo model.loanRateNoAmo True LoanRateNoAmo
    ],
    div [] [
      text "Loan Rate (1st & 2nd)",
      viewInputFloat init.loanRate model.loanRate True LoanRate
    ],
    div [] [
      text "Amortization Duration in Years (2nd)",
      viewInputInt init.amoDurationYears model.amoDurationYears True AmoDurationYears
    ],
    div [] [
      text "Interest Rate for Sustainability Check",
      viewInputFloat init.intrstRateSust model.intrstRateSust True IntrstRateSust
    ],
    div [] [
      text "Actual Interest Rate",
      viewInputFloat init.intrstRateActual model.intrstRateActual False IntrstRateActual
    ],
    div [] [ button [ onClick Calculate ] [ text "Calculate" ] ]
  ]

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