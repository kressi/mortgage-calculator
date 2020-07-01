module Main exposing (main)

import Browser
import Html exposing (Html)
import Element exposing (row, column, text)
import Element.Input as Input
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
  Element.layout []
  <|
    Element.column []
      [ Element.row []
        [ viewInputInt init.propertyVal model.propertyVal "Kaufpreis" False PropertyVal
        ]
      , Element.row []
        [ viewInputInt init.ownResources model.ownResources "Eigenmittel" False OwnResources
        ]
      , Element.row []
        [ viewInputInt init.income model.income "Einkommen (p.a.)" False Income
        ]
      , Element.row []
        [ text ("Belehnung " ++ String.fromFloat model.loanRate ++ "%")
        ]
      , Element.row []
        [ viewInputInt init.loanAmt model.loanAmt "Hypothek" True LoanAmt
        ]
      , Element.row []
        [ text ("Tragbarkeit " ++ String.fromFloat model.sustainabilityRate ++ "%")
        ]
      , Element.row []
        [ viewInputInt init.costs model.costs "Kosten p.a." True Costs
        ]
      , Element.row []
        [ viewInputFloat init.intrstRate model.intrstRate "Hypothekarzinsen (%)" True IntrstRate
        ]
      , Element.row []
        [ viewInputInt init.intrstAmt model.intrstAmt "Hypothekarzinsen" True IntrstAmt
        ]
      , Element.row []
        [ viewInputFloat init.maintRate model.maintRate "Unterhalts- und Nebenkosten (%)" True MaintRate
        ]
      , Element.row []
        [ viewInputInt init.maintAmt model.maintAmt "Unterhalts- und Nebenkosten" True MaintAmt
        ]
      , Element.row []
        [ viewInputFloat init.loanRateNoAmo model.loanRateNoAmo "Belehnungswert nach Amartisation (%)" True LoanRateNoAmo
        ]
      , Element.row []
        [ viewInputInt init.amoDurationYears model.amoDurationYears "Amortisationsdauer (Jahre)" True AmoDurationYears
        ]
      , Element.row []
        [ viewInputInt init.amoAmt model.amoAmt "Amortisationsbetrag" True AmoAmt
        ]
      , Element.row []
        [ Element.el [] (Input.button [] { onPress = Just Calculate, label = Element.text "Calculate"}) ]
      ]

--viewInput : String -> String -> Bool -> Bool -> (String -> msg) -> Element msg
viewInput p v l isFloat isDisabled toMsg =
  Input.text []
    { onChange = toMsg
    , text = v
    , placeholder = Just (Input.placeholder [] (text p))
    , label = Input.labelLeft [] (text l)
    }

--viewInputInt : Int -> Int -> Bool -> (String -> msg) -> Element msg
viewInputInt p v l =
  viewInput (String.fromInt p) (String.fromInt v) l False

--viewInputFloat : Float -> Float -> Bool -> (String -> msg) -> Element msg
viewInputFloat p v l =
  viewInput (String.fromFloat p) (String.fromFloat v) l True

consIf : Bool -> a -> List a -> List a
consIf cond x xs =
  if cond then
    x :: xs
  else
    xs
