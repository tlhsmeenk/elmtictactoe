module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { player1 : Player
    , player2 : Player
    , player1Error : Maybe String
    , player2Error : Maybe String
    , playStarted : Bool
    }


type alias Player =
    { name : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    Model (Player Nothing) (Player Nothing) Nothing Nothing False ! []



-- Update


type Msg
    = Player1InputUpdate Player String
    | Player2InputUpdate Player String
    | Play


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Player1InputUpdate player update ->
            { model | player1 = (updatedPlayerName player update) } ! []

        Player2InputUpdate player update ->
            { model | player2 = (updatedPlayerName player update) } ! []

        Play ->
            let
                validatedModel =
                    validate model

                playStarted =
                    hasWarning validatedModel.player1Error
            in
                { validatedModel | playStarted = playStarted } ! []


updatedPlayerName : Player -> String -> Player
updatedPlayerName player update =
    let
        updatedPlayer =
            if String.isEmpty update || (String.length update) > 25 then
                { player | name = Nothing }
            else
                { player | name = Just update }
    in
        updatedPlayer


validate : Model -> Model
validate model =
    let
        player1Error =
            validateName model.player1 "Player 1"

        player2Error =
            validateName model.player2 "Player 2"
    in
        { model | player1Error = player1Error, player2Error = player2Error }


validateName : Player -> String -> Maybe String
validateName player name =
    case player.name of
        Nothing ->
            Just (String.append name ": A name is not optional also it shouldn't be longer then 25 characters ;-)")

        Just name ->
            Nothing



-- View


view : Model -> Html Msg
view model =
    div [ class "mw9 center pa4 pt5-ns ph7-l" ]
        [ p [ class "f6 f5-ns lh-copy measure i pl4 bl bw1 b--gold mb4" ]
            [ text "Welcome! Please choose a name so we can start the game :-)!" ]
        , renderWarnings model
        , div []
            [ div [] [ input [ placeholder "Player 1 name", class "f6 f5-l input-reset bn fl black-80 bg-white pa3 lh-solid w-100 w-75-m w-80-l", onInput (Player1InputUpdate model.player1) ] [] ]
            , div [] [ input [ placeholder "Player 2 name", class "f6 f5-l input-reset bn fl black-80 bg-white pa3 lh-solid w-100 w-75-m w-80-l", onInput (Player2InputUpdate model.player2) ] [] ]
            , button [ class "ttu f6 f5-l fl pv3 tc bn bg-animate bg-black-70 hover-bg-black white pointer w-100 w-25-m w-20-l", onClick Play ] [ text "Start" ]
            ]
        ]


renderWarnings : Model -> Html Msg
renderWarnings model =
    let
        warnings =
            hasWarning model.player1Error || hasWarning model.player2Error
    in
        if warnings then
            p [ class "f6 f5-ns lh-copy measure i pl4 bl bw1 b--red mb4" ] [ text (String.append "Oh oh! We have an error! " (modelToErrors model)) ]
        else
            p [] []


modelToErrors : Model -> String
modelToErrors model =
    let
        player1Error =
            playerError model.player1Error

        allErrors =
            String.append player1Error (playerError model.player2Error)
    in
        allErrors


playerError : Maybe String -> String
playerError error =
    Maybe.withDefault "" error


hasWarning : Maybe String -> Bool
hasWarning possibleError =
    case possibleError of
        Nothing ->
            False

        Just value ->
            True



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
