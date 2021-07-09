module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Font as Font
import Element.Input exposing (button)
import Hex
import Html
import String exposing (String)
import Svg exposing (svg)
import Svg.Attributes as Svga
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = viewLayout
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { snake : Snake
    , playing : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Snake 1 1) True
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SetPlaying Bool


increment : Snake -> Snake
increment snake =
    if snake.x + 1 <= 10 then
        { snake | x = snake.x + 1 }

    else if snake.y + 1 <= 10 then
        { snake | x = 1, y = snake.y + 1 }

    else
        { snake | x = 1, y = 1 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.playing then
                ( { model | snake = increment model.snake }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SetPlaying bool ->
            ( { model | playing = bool }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 200 Tick



-- VIEW


viewLayout : Model -> Html.Html Msg
viewLayout model =
    layout [ Background.color (Hex.toColor "#2E3440") ] (view model)


view : Model -> Element Msg
view model =
    column []
        [ board model.snake -- #8FBCBB
        , button [ Font.color (Hex.toColor "#ECEFF4") ]
            { label =
                text <|
                    if model.playing then
                        "stop"

                    else
                        "start"
            , onPress = Just (SetPlaying (not model.playing))
            }
        ]


type alias Snake =
    { x : Int
    , y : Int
    }


board : Snake -> Element Msg
board snake =
    column [ spacing 10 ]
        (List.map
            (\y ->
                row [ spacing 10 ]
                    (List.map (\x -> tile x y snake) (List.range 1 10))
            )
            (List.range 1 10)
        )


tile : Int -> Int -> Snake -> Element Msg
tile x y snake =
    el
        [ Background.color
            (if snake.x == x && snake.y == y then
                Hex.toColor "#5E81AC"

             else
                Hex.toColor "#A3BE8C"
            )
        , width (px 50)
        , height (px 50)
        , Element.Border.rounded 5
        ]
        none
