module Main exposing (main)

import Browser
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, placeholder, value, disabled, step)
import Html exposing (Html, div, text, button, input, table, tr, td)
import String exposing (toInt)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { propertyVal : Int
  , ownResources : Int
  , loanAmt : Int
  , loanRateNoAmo : Float
  , loanRate : Float
  , amoDurationYears : Int
  , intrstRate : Float
  , intrstAmt : Int
  , maintRate : Float
  , maintAmt : Int
  , income : Int
  , amoAmt : Int
  , costs : Int
  , sustainabilityRate : Float }

init : Model
init =
  { propertyVal = 1000000
  , ownResources = 800000
  , loanAmt = 200000
  , loanRateNoAmo = 0.65
  , loanRate = 0.8
  , amoDurationYears = 15
  , intrstRate = 0.05
  , intrstAmt = 40000
  , maintRate = 0.01
  , maintAmt = 10000
  , income = 80000
  , amoAmt = 450000
  , costs = 40000
  , sustainabilityRate = 0.5  }

type Msg =
    PropertyVal String
  | OwnResources String
  | LoanAmt String
  | LoanRateNoAmo String
  | AmoDurationYears String
  | IntrstRate String
  | IntrstAmt String
  | MaintRate String
  | MaintAmt String
  | Income String
  | AmoAmt String
  | Costs String
  | Calculate

update : Msg -> Model -> Model
update msg model =
    case msg of
      PropertyVal propertyVal -> {
        model | propertyVal = Maybe.withDefault 0 (String.toInt propertyVal) }
        |> updatePropertyVal
      OwnResources ownResources -> {
        model | ownResources = Maybe.withDefault 0 (String.toInt ownResources) }
        |> updateOwnResources
      Income income -> {
        model | income = Maybe.withDefault 0 (String.toInt income) }
        |> updateIncome
      _ -> model

updatePropertyVal : Model -> Model
updatePropertyVal model =
  recalcLoanAmt model
    |> recalcMaintAmt
    |> updateLoanAmt

updateOwnResources : Model -> Model
updateOwnResources = updatePropertyVal

updateIncome : Model -> Model
updateIncome = recalcSustainabilityRate

updateLoanAmt : Model -> Model
updateLoanAmt model =
  recalcInstrAmt model
    |> recalcLoanRate
    |> recalcAmoAmt
    |> recalcCosts
    |> recalcSustainabilityRate

recalcLoanAmt : Model -> Model
recalcLoanAmt model =
  { model | loanAmt = model.propertyVal - model.ownResources }

recalcMaintAmt : Model -> Model
recalcMaintAmt model =
  { model | maintAmt = round (toFloat model.propertyVal * model.maintRate) }

recalcInstrAmt : Model -> Model
recalcInstrAmt model =
  { model | intrstAmt = round (toFloat model.loanAmt * model.intrstRate) }

recalcLoanRate : Model -> Model
recalcLoanRate model =
  { model | loanRate = toFloat model.loanAmt / toFloat model.propertyVal }

recalcAmoAmt : Model -> Model
recalcAmoAmt model =
  { model | amoAmt = Basics.max 0 (round (toFloat model.propertyVal * model.loanRateNoAmo) - model.loanAmt) }

recalcCosts : Model -> Model
recalcCosts model =
  { model | costs = round (toFloat model.amoAmt / toFloat model.amoDurationYears) + model.intrstAmt }

recalcSustainabilityRate : Model -> Model
recalcSustainabilityRate model =
  { model | sustainabilityRate = toFloat model.costs / toFloat model.income }

view : Model -> Html Msg
view model =
  table []
    [ tr []
        [ td [] [ text "Kaufpreis " ]
        , td [] [ viewInputInt init.propertyVal model.propertyVal False PropertyVal ]
        ]
    , tr []
        [ td [] [ text "Eigenmittel " ]
        , td [] [ viewInputInt init.ownResources model.ownResources False OwnResources ]
        ]
    , tr []
        [ td [] [ text "Einkommen (p.a.) " ]
        , td [] [ viewInputInt init.income model.income False Income ]
        ]
    , tr []
        [ td [] [ text "Belehnung " ]
        , td [] [ text (String.fromFloat model.loanRate ++ "%") ]
        ]
    , tr []
        [ td [] [ text "Hypothek " ]
        , td [] [ viewInputInt init.loanAmt model.loanAmt True LoanAmt ]
        ]
    , tr []
        [ td [] [ text "Tragbarkeit " ]
        , td [] [ text (String.fromFloat model.sustainabilityRate ++ "%") ]
        ]
    , tr []
        [ td [] [ text "Kosten p.a. " ]
        , td [] [ viewInputInt init.costs model.costs True Costs ]
        ]
    , tr []
        [ td [] [ text "Hypothekarzinsen (%) " ]
        , td [] [ viewInputFloat init.intrstRate model.intrstRate True IntrstRate ]
        ]
    , tr []
        [ td [] [ text "Hypothekarzinsen " ]
        , td [] [ viewInputInt init.intrstAmt model.intrstAmt True IntrstAmt ]
        ]
    , tr []
        [ td [] [ text "Unterhalts- und Nebenkosten (%) " ]
        , td [] [ viewInputFloat init.maintRate model.maintRate True MaintRate ]
        ]
    , tr []
        [ td [] [ text "Unterhalts- und Nebenkosten " ]
        , td [] [ viewInputInt init.maintAmt model.maintAmt True MaintAmt ]
        ]
    , tr []
        [ td [] [ text "Belehnungswert nach Amartisation (%) " ]
        , td [] [ viewInputFloat init.loanRateNoAmo model.loanRateNoAmo True LoanRateNoAmo ]
        ]
    , tr []
        [ td [] [ text "Amortisationsdauer (Jahre) " ]
        , td [] [ viewInputInt init.amoDurationYears model.amoDurationYears True AmoDurationYears ]
        ]
    , tr []
        [ td [] [ text "Amortisationsbetrag " ]
        , td [] [ viewInputInt init.amoAmt model.amoAmt True AmoAmt ]
        ]
    , div [] [ button [ onClick Calculate ] [ text "Calculate" ] ]
    ]

viewInput : String -> String -> Bool -> Bool -> (String -> msg) -> Html msg
viewInput p v isFloat isDisabled toMsg =
  input (
    [ type_ "number"
    , placeholder p
    , value v
    , onInput toMsg
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
