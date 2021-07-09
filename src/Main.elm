module Main exposing (Direction(..), GameState(..), Model, Msg(..), Point, Size, Snake, arrows, board, currentSnake, init, initialModel, initialSnake, main, moveSnake, playButton, subscriptions, tile, update, validateSnake, view, viewLayout, white)

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
    , gameState : GameState
    , size : Size
    }


initialModel : Model
initialModel =
    Model initialSnake Playing (Size 1 10)


type GameState
    = Playing
    | Paused
    | Lost


type alias Snake =
    { body : List Point
    , head : Point
    , direction : Direction
    }


initialSnake : Snake
initialSnake =
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SetGameState GameState
    | ChangeDirection Direction
    | StartOver


validateSnake : Size -> Snake -> Bool
validateSnake size snake =
    isSnakeInBounds size snake && not (didEatHimself snake)


didEatHimself : Snake -> Bool
didEatHimself snake =
    let
        reversed =
            List.reverse snake.body
    in
    case reversed of
        x :: xs ->
            List.member x xs

        _ ->
            False


isSnakeInBounds : Size -> Snake -> Bool
isSnakeInBounds size snake =
    (snake.head.x <= size.max)
        && (snake.head.x >= size.min)
        && (snake.head.y <= size.max)
        && (snake.head.y >= size.min)


moveSnake : Snake -> Snake
moveSnake { body, direction, head } =
    let
        tail =
            body |> List.tail |> Maybe.withDefault initialSnake.body

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

                isValid =
                    validateSnake model.size newSnake
            in
            case model.gameState of
                Playing ->
                    if isValid then
                        ( { model | snake = newSnake }
                        , Cmd.none
                        )

                    else
                        ( { model | gameState = Lost }
                        , Cmd.none
                        )

                Paused ->
                    ( model, Cmd.none )

                Lost ->
                    ( model, Cmd.none )

        SetGameState gameState ->
            ( { model | gameState = gameState }
            , Cmd.none
            )

        ChangeDirection newDirection ->
            let
                newSnake snake =
                    { snake | direction = calcNextDirection snake.direction newDirection }
            in
            ( { model | snake = newSnake model.snake }
            , Cmd.none
            )

        StartOver ->
            ( initialModel, Cmd.none )


type Side
    = Horizontal
    | Vertical


mapDirectionToSide : Direction -> Side
mapDirectionToSide dir =
    case dir of
        Left ->
            Horizontal

        Right ->
            Horizontal

        Up ->
            Vertical

        Down ->
            Vertical


calcNextDirection : Direction -> Direction -> Direction
calcNextDirection prev next =
    let
        prevSide =
            mapDirectionToSide prev

        nextSide =
            mapDirectionToSide next
    in
    case ( prevSide, nextSide ) of
        ( Horizontal, Vertical ) ->
            next

        ( Vertical, Horizontal ) ->
            next

        _ ->
            prev



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
        , playButton model.gameState
        , arrows model.snake.direction
        , currentSnake model.snake.body
        ]


playButton : GameState -> Element Msg
playButton game =
    let
        templateButton str state =
            button []
                { label = text str
                , onPress = Just state
                }
    in
    case game of
        Playing ->
            templateButton "Pause" (SetGameState Paused)

        Paused ->
            templateButton "Resume" (SetGameState Playing)

        Lost ->
            templateButton "Start over" StartOver


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


arrows : Direction -> Element Msg
arrows direction =
    let
        templateButton str msg =
            button []
                { label = text str
                , onPress = Just (ChangeDirection msg)
                }
    in
    row
        [ Font.size 50
        , Font.color white
        , spaceEvenly
        , width fill
        ]
        [ templateButton "Left" Left
        , templateButton "Up" Up
        , templateButton "Down" Down
        , templateButton "Right" Right
        ]


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
