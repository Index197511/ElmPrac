module IncrementalSearch exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



--Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



--Model


type alias Model =
    { searchedWordsList : List String
    , searchWord : String
    }






init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchedWordsList = wordsList, searchWord = "" }, Cmd.none )



--Update


type Msg
    = UpdateSearchWord String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSearchWord inputText ->
            ( { model | searchWord = inputText }, Cmd.none )



--View


view : Model -> Html Msg
view model =
    let
        filteredWords =
            model.searchedWordsList
                |> List.filter (\word -> String.contains model.searchWord word)
                |> List.map (\filterword -> li [] [ text filterword ])
    in
    div []
        [ input [ value model.searchWord, onInput UpdateSearchWord ] []
        , ul [] filteredWords
        ]



--Data


wordsList : List String
wordsList =
    [ "useful"
    , "harvest"
    , "eel"
    , "rotten"
    , "pivot"
    , "perceptible"
    , "diction"
    , "envisage"
    , "chum"
    , "request"
    , "calculator"
    , "painful"
    , "otherwise"
    , "peninsula"
    , "claimant"
    ]
