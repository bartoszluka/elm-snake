module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Font as Font
import Element.Input exposing (button)
import Hex
import Html
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
    , size : Size
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model startingSnake True (Size 1 10)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SetPlaying Bool
    | ChangeDirection Direction


validateSnake : Size -> Snake -> Bool
validateSnake size snake =
    (snake.head.x <= size.max)
        && (snake.head.x >= size.min)
        && (snake.head.y <= size.max)
        && (snake.head.y >= size.min)


moveSnake : Snake -> Snake
moveSnake { body, direction, head } =
    let
        tail =
            body |> List.tail |> Maybe.withDefault startingSnake.body

        updateBody newHead =
            Snake (tail ++ [ newHead ]) newHead direction
    in
    case direction of
        Right ->
            Point (head.x + 1) head.y |> updateBody

        Left ->
            Point (head.x - 1) head.y |> updateBody

        Up ->
            Point head.x (head.y - 1) |> updateBody

        Down ->
            Point head.x (head.y + 1) |> updateBody


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newSnake =
                    moveSnake model.snake
            in
            if model.playing && validateSnake model.size newSnake then
                ( { model | snake = newSnake }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SetPlaying bool ->
            ( { model | playing = bool }
            , Cmd.none
            )

        ChangeDirection direction ->
            let
                newSnake snake =
                    { snake | direction = direction }
            in
            ( { model | snake = newSnake model.snake }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 400 Tick



-- VIEW


viewLayout : Model -> Html.Html Msg
viewLayout model =
    layout [ Background.color (Hex.toColor "#2E3440") ] (view model)


view : Model -> Element Msg
view model =
    column [ centerX, centerY ]
        [ board model.size model.snake -- #8FBCBB
        , button [ Font.color white ]
            { label =
                text <|
                    if model.playing then
                        "stop"

                    else
                        "start"
            , onPress = Just (SetPlaying (not model.playing))
            }
        , arrows
        , currentSnake model.snake.body
        ]


currentSnake : List Point -> Element Msg
currentSnake list =
    row [ Font.color white, spaceEvenly, width fill ] <|
        List.map
            (\point ->
                text
                    (String.fromInt point.x ++ " " ++ String.fromInt point.y)
            )
            list


white : Color
white =
    Hex.toColor "#ECEFF4"


arrows : Element Msg
arrows =
    row
        [ Font.size 50
        , Font.color white
        , spaceEvenly
        , width fill
        ]
        [ button []
            { label = text "Left"
            , onPress = Just (ChangeDirection Left)
            }
        , button []
            { label = text "Right"
            , onPress = Just (ChangeDirection Right)
            }
        , button []
            { label = text "Up"
            , onPress = Just (ChangeDirection Up)
            }
        , button []
            { label = text "Down"
            , onPress = Just (ChangeDirection Down)
            }
        ]


type alias Snake =
    { body : List Point
    , head : Point
    , direction : Direction
    }


startingSnake : Snake
startingSnake =
    Snake
        (List.range 1 3
            |> List.map (\n -> Point n 1)
        )
        (Point 3 1)
        Right


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Point =
    { x : Int
    , y : Int
    }


type alias Size =
    { min : Int
    , max : Int
    }


board : Size -> Snake -> Element Msg
board size snake =
    let
        sizeRange =
            List.range size.min size.max

        spc =
            spacing 10
    in
    column [ spc ]
        (List.map
            (\y ->
                row [ spc ]
                    (List.map (\x -> tile x y snake) sizeRange)
            )
            sizeRange
        )


tile : Int -> Int -> Snake -> Element Msg
tile x y snake =
    let
        inside =
            List.member (Point x y) snake.body
    in
    el
        [ Background.color
            (if inside then
                Hex.toColor "#5E81AC"

             else
                Hex.toColor "#A3BE8C"
            )
        , width (px 50)
        , height (px 50)
        , Element.Border.rounded 5
        ]
        none
