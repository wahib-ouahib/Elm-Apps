import Html exposing(..)
import Html.Attributes exposing(..)
import Html as App
import Html.Events exposing(onClick)
import String exposing(..)


main =
  App.beginnerProgram
      { model = init
      , view = view
      , update = update
      }



--Model
type alias Calculator =
    { add : Float -> Float -> Float
    , minus : Float -> Float -> Float
    , times : Float -> Float -> Float
    , divide : Float -> Float -> Float
    }


calculator : Calculator
calculator =
        { add = (\x y -> x + y)
        , minus = (\x y -> x - y)
        , times = (\x y -> x * y)
        , divide = (\x y -> x / y)
        }

type alias Model =
        { display : String
        , function : Float -> Float -> Float
        , lastValue : Float
        , append : Bool
        }


init : Model
init =
      { display = ""
      , function = (\x y -> y)
      , lastValue = 0
      , append = True
      }


parseFloat : String -> Float
parseFloat input =
          Result.withDefault 0 (String.toFloat input)

operation : Model -> (Float -> Float -> Float) -> Model
operation model function =
          { model
              | function = function
              , lastValue = (parseFloat model.display)
              , append = False
          }


--update


type Msg
        = None
        | Divide
        | Times
        | Minus
        | Add
        | Equal
        | Decimal
        | Zero
        | Number Int
        | Clear


update : Msg -> Model -> Model
update msg model =
              case msg of
                  None ->
                      model

                  Clear ->
                      init

                  Number number ->
                      updateDisplay model number

                  Decimal ->
                      decimal model

                  Zero ->
                      zero model

                  Divide ->
                      operation model calculator.divide

                  Times ->
                      operation model calculator.times

                  Minus ->
                      operation model calculator.minus

                  Add ->
                      operation model calculator.add

                  Equal ->
                      equal model



updateDisplay : Model -> Int -> Model
updateDisplay model number =
                    if model.append then
                      { model | display = model.display ++ toString (number) }
                    else
                      { model | display = toString (number), append = True }


equal : Model -> Model
equal model =
                          if model.append then
                              { model
                                  | display = calculate model
                                  , lastValue = (parseFloat model.display)
                                  , append = False
                              }
                          else
                              { model
                                  | display = calculate model
                                  , append = False
                              }


calculate : Model -> String
calculate model =
              model.function model.lastValue (parseFloat model.display) |> toString


zero : Model -> Model
zero model =
            if String.isEmpty model.display || not model.append then
              { model
                    | display = "0"
                    , append = False
              }
            else
              { model | display = model.display ++ "0" }


decimal : Model -> Model
decimal model =
              if not (String.isEmpty model.display) && model.append then
                      { model | display = appendDecimal model.display }
              else
                      { model | display = "0.", append = True }


appendDecimal : String -> String
appendDecimal string =
                    if String.contains "." string then
                          string
                    else
                          string ++ "."




-- VIEW


calculatorButton : Msg -> String -> Html Msg
calculatorButton msg buttonText =
                              button [ class "button", onClick msg ]
                                  [ span [] [ text (buttonText) ] ]


calculatorButtonWide : Msg -> String -> Html Msg
calculatorButtonWide msg buttonText =
                              button [ class "button wide", onClick msg ]
                                  [ span [] [ text (buttonText) ] ]


stylesheet : Html Msg
stylesheet =
                              let
                                  tag =
                                      "link"

                                  attrs =
                                      [ attribute "Rel" "stylesheet"
                                      , attribute "property" "stylesheet"
                                      , attribute "href" "styles.css"
                                      ]

                                  children =
                                      []
                              in
                                  node tag attrs children


view : Model -> Html Msg
view model =
                              div [ class "calculator" ]
                                  [ stylesheet
                                  , div [ class "row" ]
                                      [ div [ class "col-xs-12" ]
                                          [ div [ class "display" ]
                                              [ div [ class "display-text" ]
                                                  [ text (model.display) ]
                                              ]
                                          , div [ class "buttons" ]
                                              [ calculatorButtonWide Clear "Clear"
                                              , calculatorButton (Number 7) "7"
                                              , calculatorButton (Number 8) "8"
                                              , calculatorButton (Number 9) "9"
                                              , calculatorButton Divide "÷"
                                              , calculatorButton (Number 4) "4"
                                              , calculatorButton (Number 5) "5"
                                              , calculatorButton (Number 6) "6"
                                              , calculatorButton Times "x"
                                              , calculatorButton (Number 1) "1"
                                              , calculatorButton (Number 2) "2"
                                              , calculatorButton (Number 3) "3"
                                              , calculatorButton Minus "-"
                                              , calculatorButton Zero "0"
                                              , calculatorButton Decimal "."
                                              , calculatorButton Equal "="
                                              , calculatorButton Add "+"
                                              ]
                                          ]
                                      ]
                                  ]
