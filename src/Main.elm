module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Font as Font
import Html
import Svg exposing (svg)
import Svg.Attributes as Svga
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = viewLayout
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


viewLayout : Model -> Html.Html Msg
viewLayout model =
    layout [] (view model)


view : Model -> Element Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)
    in
    column []
        [ clock
        , el [ Font.size 30 ] (text (hour ++ ":" ++ minute ++ ":" ++ second))
        ]


clock : Element Msg
clock =
    Element.html <|
        Svg.svg
            [ Svga.width "120"
            , Svga.height "120"
            , Svga.viewBox "0 0 120 120"
            ]
            [ Svg.circle
                [ Svga.cx "50"
                , Svga.cy "50"
                , Svga.r "50"
                , Svga.fill "#0000FF"
                ]
                []
            , Svg.line
                [ Svga.x1 "0"
                , Svga.y1 "80"
                , Svga.x2 "100"
                , Svga.y2 "20"
                , Svga.stroke "red"
                ]
                []
            ]
