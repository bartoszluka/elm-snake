module Main exposing (Direction(..), GameState(..), Model, Msg(..), Point, Size, Snake, arrows, board, currentSnake, init, initialModel, initialSnake, main, moveSnake, playButton, subscriptions, update, validateSnake, view, viewLayout, viewTile)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Font as Font
import Element.Input exposing (button)
import Hex
import Html
import Json.Decode as Decode
import Random
import Random.Extra
import Svg.Attributes exposing (direction)
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
    , food : Point
    , score : Int
    , pressedKey : String
    }


initialModel : Model
initialModel =
    Model initialSnake Playing (Size 1 15) (Point 3 3) 0 "keys here"


type GameState
    = Playing
    | Paused
    | Lost


type alias Snake =
    { body : List Point
    , head : Point
    , currentDirection : Direction
    , nextDirection : Direction
    }


initialSnake : Snake
initialSnake =
    Snake
        (List.range 1 3
            |> List.map (\n -> Point n 1)
        )
        (Point 3 1)
        Right
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
    | NewFood Point
    | KeyPressed String
    | UnknownKeyPressed


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
    -- just to get it formatted correctly
    True
        && (snake.head.x <= size.max)
        && (snake.head.x >= size.min)
        && (snake.head.y <= size.max)
        && (snake.head.y >= size.min)


moveSnake : Snake -> Bool -> Snake
moveSnake { body, head, nextDirection } ateFood =
    let
        tail =
            if ateFood then
                body

            else
                body |> List.tail |> Maybe.withDefault initialSnake.body

        updateBody newHead =
            Snake (tail ++ [ newHead ]) newHead nextDirection nextDirection
    in
    case nextDirection of
        Right ->
            Point (head.x + 1) head.y |> updateBody

        Left ->
            Point (head.x - 1) head.y |> updateBody

        Up ->
            Point head.x (head.y - 1) |> updateBody

        Down ->
            Point head.x (head.y + 1) |> updateBody


generatePoint : Model -> Random.Generator Point
generatePoint model =
    let
        generateInt : Size -> Random.Generator Int
        generateInt { min, max } =
            Random.int min max

        generateGenericPoint : Size -> Random.Generator Point
        generateGenericPoint size =
            Random.map2 Point (generateInt size) (generateInt size)
    in
    Random.Extra.filter (\p -> not <| List.member p model.snake.body) (generateGenericPoint model.size)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newSnake =
                    moveSnake model.snake False

                ateFood =
                    List.member model.food newSnake.body

                isValid =
                    validateSnake model.size newSnake
            in
            case model.gameState of
                Playing ->
                    if isValid then
                        if ateFood then
                            ( { model | snake = moveSnake model.snake True, score = model.score + 1 }
                            , Random.generate NewFood (generatePoint model)
                            )

                        else
                            ( { model | snake = newSnake }
                            , Cmd.none
                            )

                    else
                        ( { model | gameState = Lost }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        SetGameState gameState ->
            ( { model | gameState = gameState }
            , Cmd.none
            )

        ChangeDirection direction ->
            if model.gameState == Playing then
                let
                    newDirection =
                        calcNextDirection model.snake.currentDirection direction

                    newSnake snake =
                        { snake | nextDirection = newDirection }
                in
                ( { model | snake = newSnake model.snake }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        StartOver ->
            ( initialModel, Cmd.none )

        NewFood point ->
            ( { model | food = point }
            , Cmd.none
            )

        KeyPressed key ->
            ( { model | pressedKey = key }
            , Cmd.none
            )

        UnknownKeyPressed ->
            ( model, Cmd.none )


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
subscriptions model =
    Sub.batch
        [ Time.every (speedUp model.score) Tick
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyDown keyDecoderToString
        ]


speedUp : Int -> Float
speedUp score =
    let
        x =
            toFloat score

        rate =
            1.1

        initialSpeed =
            300

        finalSpeed =
            150
    in
    (rate ^ -x) * (initialSpeed - finalSpeed) + finalSpeed


keyDecoderToString : Decode.Decoder Msg
keyDecoderToString =
    Decode.map KeyPressed (Decode.field "key" Decode.string)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMessage (Decode.field "key" Decode.string)


toMessage : String -> Msg
toMessage string =
    string |> foobar



-- VIEW


view : Model -> Element Msg
view model =
    column [ centerX, centerY ]
        [ playButton model.gameState
        , viewScore model.score
        , board model
        , arrows
        , el [] (text model.pressedKey)
        ]


viewScore : Int -> Element Msg
viewScore score =
    el [ Font.size 70, centerX ] (text <| String.fromInt score)


type DirectionMap
    = Match Direction
    | NoMatch String


coolMap : List String -> Direction -> DirectionMap -> DirectionMap
coolMap bindings direction dirmap =
    case dirmap of
        Match d ->
            Match d

        NoMatch key ->
            if List.member key bindings then
                Match direction

            else
                NoMatch key


unWrap : DirectionMap -> Msg
unWrap dirmap =
    case dirmap of
        Match direction ->
            ChangeDirection direction

        NoMatch _ ->
            UnknownKeyPressed


foobar : String -> Msg
foobar key =
    NoMatch key
        |> coolMap [ "h", "ArrowLeft", "a" ] Left
        |> coolMap [ "l", "ArrowRight", "d" ] Right
        |> coolMap [ "k", "ArrowUp", "w" ] Up
        |> coolMap [ "j", "ArrowDown", "s" ] Down
        |> unWrap


keyToMsg : String -> Msg
keyToMsg key =
    case key of
        "h" ->
            ChangeDirection Left

        "ArrowLeft" ->
            ChangeDirection Left

        "l" ->
            ChangeDirection Right

        "ArrowRight" ->
            ChangeDirection Right

        "k" ->
            ChangeDirection Up

        "ArrowUp" ->
            ChangeDirection Up

        "j" ->
            ChangeDirection Down

        "ArrowDown" ->
            ChangeDirection Down

        _ ->
            UnknownKeyPressed


viewLayout : Model -> Html.Html Msg
viewLayout model =
    layout [ Background.color (Hex.toColor "#2E3440"), Font.color white ] (view model)


playButton : GameState -> Element Msg
playButton game =
    let
        templateButton str state =
            button [ Font.size 30 ]
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
    row [ spaceEvenly, width fill ] <|
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
    let
        templateButton str msg =
            button []
                { label = text str
                , onPress = Just (ChangeDirection msg)
                }
    in
    row
        [ Font.size 50
        , spaceEvenly
        , width fill
        ]
        [ templateButton "Left" Left
        , templateButton "Up" Up
        , templateButton "Down" Down
        , templateButton "Right" Right
        ]


board : Model -> Element Msg
board model =
    let
        sizeRange =
            List.range model.size.min model.size.max

        spc =
            spacing 7

        tileType x y =
            if List.member (Point x y) model.snake.body then
                SnakePart

            else if Point x y == model.food then
                Food

            else
                Board
    in
    column [ spc ]
        (List.map
            (\y ->
                row [ spc ]
                    (List.map (\x -> viewTile (tileType x y)) sizeRange)
            )
            sizeRange
        )


type TileType
    = SnakePart
    | Board
    | Food


viewTile : TileType -> Element Msg
viewTile tile =
    let
        pixels =
            px 30

        template str =
            el [ Background.color (Hex.toColor str), width pixels, height pixels, Element.Border.rounded 5 ] none
    in
    case tile of
        SnakePart ->
            template "#5E81AC"

        Board ->
            template "#A3BE8C"

        Food ->
            template "#BF616A"
