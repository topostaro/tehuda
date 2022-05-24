module Main exposing (main)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import List exposing (range, sum)
import String exposing (slice)



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
    { deck : Int
    , drawn : Int
    , target : Int
    , godonBool : Bool
    , godonNum : Int
    }


init : Model
init =
    Model 40 5 9 False 0



-- UPDATE


type Msg
    = Deck String
    | Drawn String
    | Target String
    | GodonBool Bool
    | GodonNum String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Deck deck ->
            { model | deck = Maybe.withDefault 40 <| String.toInt deck }

        Drawn drawn ->
            { model | drawn = Maybe.withDefault 0 <| String.toInt drawn }

        Target target ->
            { model | target = Maybe.withDefault 0 <| String.toInt target }

        GodonBool godonBool ->
            { model | godonBool = godonBool }

        GodonNum godonNum ->
            { model | godonNum = Maybe.withDefault 0 <| String.toInt godonNum }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ myH1 [] [ text "手札シミュレーター" ]
        , viewInputNumber "デッキ枚数 " (String.fromInt model.deck) 60 Deck
        , viewInputNumber "ドロー枚数 " (String.fromInt model.drawn) 60 Drawn
        , viewInputNumber "引きたいカード " (String.fromInt model.target) 60 Target
        , input [ type_ "checkbox", Html.Styled.Attributes.checked model.godonBool, onCheck GodonBool ] []
        , h3 [] [ text "結果: " ]
        , viewResult model
        ]


viewInputNumber : String -> String -> Int -> (String -> msg) -> Html msg
viewInputNumber name val max toMsg =
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


viewResult model =
    let
        subview n =
            div []
                [ calculate model n
                    |> String.fromFloat
                    |> slice 0 5
                    |> (\s ->
                            String.fromInt n
                                ++ "枚: "
                                ++ s
                                ++ "%"
                       )
                    |> text
                ]
    in
    div [] <|
        (List.map subview <|
            range 1 model.drawn
        )
            ++ [ range 1 model.drawn
                    |> List.map (calculate model)
                    |> sum
                    |> String.fromFloat
                    |> slice 0 5
                    |> (\s ->
                            "計: "
                                ++ s
                                ++ "%"
                       )
                    |> text
               ]


calculate : Model -> Int -> Float
calculate model hit =
    let
        choose n k =
            case k of
                0 ->
                    1

                _ ->
                    choose (n - 1) (k - 1) * n / toFloat k

        deck =
            toFloat model.deck

        drawn =
            model.drawn

        target =
            toFloat model.target
    in
    100.0 * (choose target hit * choose (deck - target) (drawn - hit) / choose deck drawn)


myH1 =
    styled h1
        [ backgroundImage <|
            linearGradient2
                toRight
                (stop2 (hex "ffffff") <| pct 5)
                (stop (hex "a7d6ff"))
                []
        ]
