module Main exposing (main)

import Browser
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, placeholder, value, disabled, step, style)
import Html exposing (Html, Attribute, div, text, button, input, table, tr, td)
import String exposing (toInt)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }


-- Model

type alias Model =
  { propertyVal : Int
  , ownResources : Int
  , income : Int
  , loanAmt : Int
  , loanRateNoAmo : Float
  , loanRate : Float
  , amoDurationYears : Int
  , intrstRate : Float
  , intrstAmt : Int
  , maintRate : Float
  , maintAmt : Int
  , amoAmt : Int
  , amoAmtYearly : Int
  , costs : Int
  , sustainabilityRate : Float
  , sustainabilityMax : Float
  , loanRateMax : Float }

init : Model
init =
  { propertyVal = 1000000
  , ownResources = 200000
  , income = 80000
  , loanAmt = 800000
  , loanRateNoAmo = 0.65
  , loanRate = 0.8
  , amoDurationYears = 15
  , intrstRate = 0.05
  , intrstAmt = 40000
  , maintRate = 0.01
  , maintAmt = 10000
  , amoAmt = 150000
  , amoAmtYearly = 10000
  , costs = 60000
  , sustainabilityRate = 0.5
  , sustainabilityMax = 0.33
  , loanRateMax = 0.8 }

type Msg =
    PropertyVal String
  | OwnResources String
  | Income String
  | IntrstRate String


-- Update

update : Msg -> Model -> Model
update msg model =
    case msg of
      PropertyVal propertyVal -> {
        model | propertyVal = toIntOrZero propertyVal }
        |> updatePropertyVal
      OwnResources ownResources -> {
        model | ownResources = toIntOrZero ownResources }
        |> updateOwnResources
      Income income -> {
        model | income = toIntOrZero income }
        |> updateIncome
      IntrstRate intrstRate -> {
        model | intrstRate = toFloatOrZero intrstRate }
        |> updateIntrstRate

updatePropertyVal : Model -> Model
updatePropertyVal =
  recalcLoanAmt
  >> recalcMaintAmt
  >> recalcIntrstAmt
  >> recalcLoanRate
  >> recalcAmoAmt
  >> recalcAmoAmtYearly
  >> recalcCosts
  >> recalcSustainabilityRate

updateOwnResources : Model -> Model
updateOwnResources = updatePropertyVal

updateIncome : Model -> Model
updateIncome = recalcSustainabilityRate

updateIntrstRate : Model -> Model
updateIntrstRate =
  recalcIntrstAmt
  >> recalcCosts
  >> recalcSustainabilityRate

recalcLoanAmt : Model -> Model
recalcLoanAmt model =
  { model | loanAmt = model.propertyVal - model.ownResources }

recalcMaintAmt : Model -> Model
recalcMaintAmt model =
  { model | maintAmt = round (toFloat model.propertyVal * model.maintRate) }

recalcIntrstAmt : Model -> Model
recalcIntrstAmt model =
  { model | intrstAmt = round (toFloat model.loanAmt * model.intrstRate) }

recalcLoanRate : Model -> Model
recalcLoanRate model =
  { model | loanRate = toFloat model.loanAmt / toFloat model.propertyVal }

recalcAmoAmt : Model -> Model
recalcAmoAmt model =
  { model | amoAmt = Basics.max 0 (model.loanAmt - round (toFloat model.propertyVal * model.loanRateNoAmo)) }

recalcAmoAmtYearly : Model -> Model
recalcAmoAmtYearly model =
  { model | amoAmtYearly = round (toFloat model.amoAmt / toFloat model.amoDurationYears) }

recalcCosts : Model -> Model
recalcCosts model =
  { model | costs = model.amoAmtYearly + model.intrstAmt + model.maintAmt }

recalcSustainabilityRate : Model -> Model
recalcSustainabilityRate model =
  { model | sustainabilityRate = toFloat model.costs / toFloat model.income }


-- View

styleNumber : List (Attribute Msg)
styleNumber =
  [ style "text-align" "right" ]

styleThick : List (Attribute Msg)
styleThick =
  [ style "font-weight" "bold"]

styleGreen : List (Attribute Msg)
styleGreen =
  [ style "background-color" "lawngreen"
  , style "color" "darkgreen"
  ] ++ styleThick

styleRed : List (Attribute Msg)
styleRed =
  [ style "background-color" "orange"
  , style "color" "darkred"
  ] ++ styleThick

styleStatus : Bool -> List (Attribute Msg)
styleStatus cond =
  if cond then
    styleGreen
  else
    styleRed

styleBorder : List(Attribute Msg)
styleBorder =
  [ style "border" "1px solid"
  , style "border-radius" "5px"
  , style "margin" "2px"
  ]

styleInlineInput : List (Attribute Msg)
styleInlineInput = 
  [ style "-moz-appearance" "textfield"
  , style "width" "4em"
  ]

styleRow : List (Attribute Msg)
styleRow =
  [ style "display" "flex"
  , style "flex-wrap" "wrap"
  ]

styleLabel : List (Attribute Msg)
styleLabel =
  [ style "min-width" "21em"
  ]

view : Model -> Html Msg
view model =
  div
    [ style "display" "grid"
    , style "justify-content" "center"
    ]
  [ div styleBorder
      [ div styleRow
          [ div styleLabel [ text "Kaufpreis" ]
          , div [] [ viewInputInt init.propertyVal model.propertyVal (Just PropertyVal) ]
          ]
      , div styleRow
          [ div styleLabel [ text "Eigenmittel" ]
          , div [] [ viewInputInt init.ownResources model.ownResources (Just OwnResources) ]
          ]
      , div styleRow
          [ div styleLabel [ text "Einkommen p.a." ]
          , div [] [ viewInputInt init.income model.income (Just Income) ]
          ]
      ],
    div styleBorder
      [ div styleRow
          [ div styleLabel [ text "Hypothek" ]
          , div [] [ viewInputInt init.loanAmt model.loanAmt Nothing ]
          ]
      , div styleRow
          [ div styleLabel [ text ("Belehnung (max " ++ String.fromFloat model.loanRateMax ++ ")") ]
          , div
              (styleNumber ++ styleStatus (model.loanRate <= model.loanRateMax))
              [ text (String.fromFloat (round2 model.loanRate)) ]
          ]
      ],
    div styleBorder
      [ div styleRow
          [ div styleLabel [ text "Belehnungsrate nach Amortisation" ]
          , div [] [ viewInputFloat init.loanRateNoAmo model.loanRateNoAmo Nothing ]
          ]
      , div styleRow
          [ div styleLabel [ text "Amortisationsdauer (Jahre)" ]
          , div [] [ viewInputInt init.amoDurationYears model.amoDurationYears Nothing ]
          ]
      , div styleRow
          [ div styleLabel [ text "Amortisationsbetrag" ]
          , div [] [ viewInputInt init.amoAmt model.amoAmt Nothing ]
          ]
      ],
    div styleBorder
      [ div styleRow
          [ div styleLabel
              [ text "Hypothekarzinsen "
              , viewInputFloatSmall init.intrstRate model.intrstRate (Just IntrstRate)
              , text " p.a."
              ]
          , div [] [ viewInputInt init.intrstAmt model.intrstAmt Nothing ]
          ]
      , div styleRow
          [ div styleLabel [ text "Amortisationsbetrag p.a." ]
          , div [] [ viewInputInt init.amoAmtYearly model.amoAmtYearly Nothing ]
          ]
      , div styleRow
          [ div styleLabel [ text ("Unterhalts- und Nebenkosten p.a. (" ++ String.fromFloat model.maintRate ++ ")") ]
          , div [] [ viewInputInt init.maintAmt model.maintAmt Nothing ]
          ]
      , div styleRow
          [ div styleLabel [ text "Kosten p.a." ]
          , div [] [ viewInputInt init.costs model.costs Nothing ]
          ]
      , div styleRow
          [ div styleLabel [ text ("Tragbarkeit (max " ++ String.fromFloat model.sustainabilityMax ++ ")")]
          , div
              (styleNumber ++ styleStatus (model.sustainabilityRate <= model.sustainabilityMax))
              [ text (String.fromFloat (round2 model.sustainabilityRate)) ]
          ]
      ]
  ]

viewInput : String -> String -> Maybe String -> List (Attribute Msg) -> Maybe (String -> Msg) -> Html Msg
viewInput p v s style toMsg =
  input
    ( styleNumber ++ style ++
      [ type_ "number"
      , placeholder p
      , value v
      ] |> consJust (Maybe.map onInput toMsg)
        |> consIf (toMsg == Nothing) (disabled True)
        |> consJust (Maybe.map step s)
    ) []

viewInputInt : Int -> Int -> Maybe (String -> Msg) -> Html Msg
viewInputInt p v =
  viewInput (String.fromInt p) (String.fromInt v) (Just "1000") []

viewInputFloat : Float -> Float -> Maybe (String -> Msg) -> Html Msg
viewInputFloat p v =
  viewInput (String.fromFloat (round2 p)) (String.fromFloat (round2 v)) (Just "any") []

viewInputFloatSmall : Float -> Float -> Maybe (String -> Msg) -> Html Msg
viewInputFloatSmall p v =
  viewInput (String.fromFloat (round2 p)) (String.fromFloat (round2 v)) (Just "any") styleInlineInput

consIf : Bool -> a -> List a -> List a
consIf cond x xs =
  if cond then
    x :: xs
  else
    xs

ifThenElse : Bool -> a -> a -> a
ifThenElse cond x y =
  if cond then
    x
  else
    y

consJust : Maybe a -> List a -> List a
consJust maybe list =
  case maybe of
    Just value -> value :: list
    Nothing -> list

toIntOrZero : String -> Int
toIntOrZero s =
  String.toInt s
    |> Maybe.withDefault 0

toFloatOrZero : String -> Float
toFloatOrZero s =
  String.toFloat s
    |> Maybe.withDefault 0

round2 : Float -> Float
round2 num =
  toFloat (round (num * 100)) / 100