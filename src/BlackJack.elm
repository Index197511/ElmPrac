module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Random exposing (..)
import Random.List exposing (..)
import String
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { deck : List Int
    , gameState : Bool
    , playersHand : List Int
    , playersPoint : Int
    , playersState : Bool
    , dealersHand : List Int
    , isShow : Bool
    , dealersPoint : Int
    , dealersState : Bool
    , result : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        manyTen =
            List.repeat 12 10

        stack =
            List.range 1 10
                |> List.repeat 4
                |> List.concat
    in
    ( { deck = List.append stack manyTen
      , gameState = False
      , playersHand = []
      , playersPoint = 0
      , playersState = True
      , dealersHand = []
      , isShow = False
      , dealersPoint = 0
      , dealersState = True
      , result = ""
      }
    , Cmd.none
    )


type Msg
    = Shuffle Time.Posix
    | ConstructDeck (List Int)
    | Start
    | DealersTurn Time.Posix
    | Hit
    | Result
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle _ ->
            ( model, generate ConstructDeck (shuffle model.deck) )

        ConstructDeck newDeck ->
            ( { model | deck = newDeck }, Cmd.none )

        Start ->
            case model.gameState of
                False ->
                    let
                        initCard =
                            List.take 4 model.deck

                        playersDraw =
                            List.take 2 initCard

                        dealersDraw =
                            List.drop 2 initCard
                                |> List.take 2
                    in
                    ( { model
                        | gameState = True
                        , playersHand = List.append model.playersHand playersDraw
                        , dealersHand = List.append model.dealersHand dealersDraw
                        , playersPoint = model.playersPoint + List.sum playersDraw
                        , dealersPoint = model.dealersPoint + List.sum dealersDraw
                        , deck = List.drop 4 model.deck
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DealersTurn _ ->
            if model.dealersPoint < 17 then
                let
                    newCard =
                        List.take 1 model.deck
                in
                ( { model
                    | dealersHand = List.append model.dealersHand newCard
                    , deck = List.drop 1 model.deck
                    , dealersPoint = model.dealersPoint + List.sum newCard
                  }
                , Cmd.none
                )

            else if model.dealersPoint > 21 then
                ( { model | dealersState = False }, Cmd.none )

            else
                ( model, Cmd.none )

        Hit ->
            let
                newCard =
                    List.take 1 model.deck

                flagPoint =
                    model.playersPoint + List.sum (List.take 1 model.deck)
            in
            if model.playersPoint <= 21 then
                if flagPoint > 21 then
                    ( { model
                        | playersHand = List.append model.playersHand newCard
                        , deck = List.drop 1 model.deck
                        , playersPoint = model.playersPoint + List.sum newCard
                        , playersState = False
                      }
                    , Cmd.none
                    )

                else
                    ( { model
                        | playersHand = List.append model.playersHand newCard
                        , deck = List.drop 1 model.deck
                        , playersPoint = model.playersPoint + List.sum newCard
                      }
                    , Cmd.none
                    )

            else
                ( model, Cmd.none )

        Result ->
            if model.playersState && model.dealersState then
                if model.playersPoint < model.dealersPoint then
                    ( { model | isShow = True, result = "Dealer Wins!!" }, Cmd.none )

                else if model.playersPoint > model.dealersPoint then
                    ( { model | isShow = True, result = "Player Wins!!" }, Cmd.none )

                else
                    ( { model | isShow = True, result = "Draw..." }, Cmd.none )

            else if not model.playersState && model.dealersState then
                ( { model | isShow = True, result = "Dealer Wins" }, Cmd.none )

            else if model.playersState && not model.dealersState then
                ( { model | isShow = True, result = "Player Wins" }, Cmd.none )

            else
                ( { model | isShow = True, result = "Draw..." }, Cmd.none )

        Reset ->
            ( { model
                | gameState = False
                , playersHand = []
                , playersPoint = 0
                , playersState = True
                , dealersHand = []
                , isShow = False
                , dealersPoint = 0
                , dealersState = True
                , result = ""
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameState then
        Time.every 0 DealersTurn

    else
        Time.every 0 Shuffle


view : Model -> Html Msg
view model =
    let
        playersHandFormat =
            List.map String.fromInt model.playersHand
                |> String.join " "

        dealersHandFormat =
            if model.isShow then
                List.map String.fromInt model.dealersHand
                    |> String.join " "

            else
                "* * *"
    in
    div []
        [ text "BlackJack"
        , br [] []
        , text "DealersHand"
        , br [] []
        , text dealersHandFormat
        , br [] []
        , text
            (if model.isShow then
                String.fromInt model.dealersPoint

             else
                "*"
            )
        , br [] []
        , text "PlayersHand"
        , br [] []
        , text playersHandFormat
        , br [] []
        , text (String.fromInt model.playersPoint)
        , br [] []
        , button [ onClick Start ] [ text "Start" ]
        , br [] []
        , button [ onClick Hit ] [ text "Hit" ]
        , br [] []
        , button [ onClick Result ] [ text "Stand" ]
        , br [] []
        , button [ onClick Reset ] [ text "Reset" ]
        , br [] []
        , text model.result
        ]
