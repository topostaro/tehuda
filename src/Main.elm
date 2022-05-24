module Main exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { deck : String
    , drawn : String
    , target : String
    }


init : Model
init =
    Model "40" "5" "9"



-- UPDATE


type Msg
    = Deck String
    | Drawn String
    | Target String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Deck deck ->
            { model | deck = deck }

        Drawn drawn ->
            { model | drawn = drawn }

        Target target ->
            { model | target = target }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ myH1 [] [ text "手札シミュレーター" ]
        , viewInput "デッキ枚数 " model.deck 60 Deck
        , viewInput "ドロー枚数 " model.drawn 60 Drawn
        , viewInput "引きたいカード " model.target 60 Target
        , text <|
            "結果: "
                ++ (String.slice 0 5 <| String.fromFloat <| calculate model)
                ++ "%"
        ]


viewInput : String -> String -> Int -> (String -> msg) -> Html msg
viewInput name val max toMsg =
    div []
        [ text name
        , input
            [ type_ "number"
            , style "text-align" "right"
            , value val
            , Html.Styled.Attributes.min "0"
            , Html.Styled.Attributes.max <| String.fromInt max
            , onInput toMsg
            ]
            []
        , text "枚"
        ]


calculate : Model -> Float
calculate model =
    let
        choose n k =
            case k of
                0 ->
                    1

                _ ->
                    choose (n - 1) (k - 1) * n

        deck =
            String.toFloat model.deck |> Maybe.withDefault 40

        drawn =
            String.toInt model.drawn |> Maybe.withDefault 0

        target =
            String.toFloat model.target |> Maybe.withDefault 0
    in
    (1 - choose (deck - target) drawn / choose deck drawn) * 100.0


myH1 =
    styled h1
        [ backgroundImage <|
            linearGradient2
                toRight
                (stop2 (hex "ffffff") <| pct 5)
                (stop (hex "a7d6ff"))
                []
        ]
