module WebTimer exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Maybe
import Round
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--Model


type alias Model =
    { time : String
    , behavior : Bool
    , timeMemos : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = "0.0", behavior = False, timeMemos = [] }, Cmd.none )



--Update


rounding : Int -> Float -> String
rounding decimalPoint num =
    Round.round decimalPoint num
        |> str2Flo
        |> \n ->
                if n - toFloat (floor n) > 0 then
                    String.fromFloat n

                else
                    String.fromFloat n ++ ".0"



str2Flo : String -> Float
str2Flo string =
    String.toFloat string
        |> Maybe.withDefault 0


alignment : Float -> String
alignment num =
    if num - toFloat (floor num) > 0 then
        String.fromFloat num

    else
        String.fromFloat num ++ ".0"


type Msg
    = Start
    | Stop
    | Tick Time.Posix
    | Reset
    | Save
    | Clear
    | Resume


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            if not model.behavior then
                ( { model | behavior = True, time = "0.0" }, Cmd.none )

            else
                ( model, Cmd.none )

        Stop ->
            ( { model | behavior = False }, Cmd.none )

        Reset ->
            ( { model | time = "0.0" }, Cmd.none )

        Tick _ ->
            if model.behavior then
                let
                    newTime =
                        str2Flo model.time
                            |> (+) 0.1
                            |> rounding 1
                in
                ( { model | time = newTime }, Cmd.none )

            else
                ( model, Cmd.none )

        Save ->
            if str2Flo model.time /= 0 then
                let
                    newTimeMemos =
                        model.time :: model.timeMemos
                in
                ( { model | timeMemos = newTimeMemos }, Cmd.none )

            else
                ( model, Cmd.none )

        Clear ->
            ( { model | timeMemos = [] }, Cmd.none )

        Resume ->
            if model.behavior then
                ( model, Cmd.none )

            else
                ( { model | behavior = True }, Cmd.none )



--Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.behavior then
        Time.every 100 Tick

    else
        Time.every 0 Tick



--View


view : Model -> Html Msg
view model =
    let
        showMemos =
            List.map str2Flo model.timeMemos
                |> List.sort
                |> List.map alignment
                |> List.map (\w -> li [] [ text w ])
    in
    div []
        [ h1 [] [ text model.time ]
        , button [ onClick Start ] [ text "Start" ]
        , button [ onClick Stop ] [ text "Stop" ]
        , button [ onClick Resume ] [ text "Resume" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Save ] [ text "Save" ]
        , button [ onClick Clear ] [ text "Clear" ]
        , ul [] showMemos
        ]
