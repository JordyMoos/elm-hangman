module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value)
import String exposing (fromChar, uncons)
import Char


type alias Letter =
    { char : Char
    , isGuessed : Bool
    }


type alias Model =
    { word : List Letter
    , attemptsLeft : Int
    , state : GameState
    }


type GameState
    = Playing
    | Won
    | Lost


toLetters : List Char -> List Letter
toLetters chars =
    List.map (\char -> Letter char False) chars


init : Model
init =
    Model (toLetters [ 'X', 'I', 'M', 'E', 'D', 'E', 'S' ]) 10 Playing


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type Msg
    = GuessChar String
    | Reset


update : Msg -> Model -> Model
update msg model =
    case ( model.state, msg ) of
        ( Playing, GuessChar guessedString ) ->
            case uncons guessedString of
                Just ( guessedCharRaw, "" ) ->
                    let
                        guessedChar =
                            Char.toUpper guessedCharRaw

                        applyGuessToWord model =
                            let
                                updateWord word =
                                    List.map
                                        (\letter ->
                                            if letter.char == guessedChar then
                                                { letter | isGuessed = True }
                                            else
                                                letter
                                        )
                                        word
                            in
                                { model | word = updateWord model.word }

                        penaltyWhenWrongGuess model =
                            let
                                isCorrectGuess word =
                                    List.any
                                        (\letter -> letter.char == guessedChar)
                                        word

                                attemptsLeft =
                                    if isCorrectGuess model.word then
                                        model.attemptsLeft
                                    else
                                        model.attemptsLeft - 1
                            in
                                { model | attemptsLeft = attemptsLeft }

                        updateState model =
                            let
                                wordIsGuessed word =
                                    List.all
                                        (\letter -> letter.isGuessed)
                                        word

                                state =
                                    if model.attemptsLeft < 1 then
                                        Lost
                                    else if wordIsGuessed model.word then
                                        Won
                                    else
                                        Playing
                            in
                                { model | state = state }
                    in
                        model
                            |> applyGuessToWord
                            |> penaltyWhenWrongGuess
                            |> updateState

                _ ->
                    model

        ( _, Reset ) ->
            init

        -- Void on bibs
        ( _, _ ) ->
            model


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "Hangman the game!" ]
        , showState model
        , showReset
        ]


showState : Model -> Html Msg
showState model =
    case model.state of
        Playing ->
            showPlayingState model

        Won ->
            showWonState

        Lost ->
            showLostState


showPlayingState : Model -> Html Msg
showPlayingState model =
    div
        []
        [ showWord model.word
        , showAttemptsLeft model.attemptsLeft
        , showInput ""
        ]


showWonState : Html Msg
showWonState =
    div [] [ text "Jeej you won!, share it with your friends!" ]


showLostState : Html Msg
showLostState =
    div [] [ text "To bad... you lost" ]


showWord : List Letter -> Html Msg
showWord word =
    ul [] (List.map showLetter word)


showLetter : Letter -> Html Msg
showLetter letter =
    let
        message =
            if letter.isGuessed then
                fromChar letter.char
            else
                "_"
    in
        li [] [ text message ]


showAttemptsLeft : Int -> Html Msg
showAttemptsLeft attemptsLeft =
    div [] [ text (toString attemptsLeft) ]


showInput : String -> Html Msg
showInput inputValue =
    input [ onInput GuessChar, value inputValue ] []


showReset : Html Msg
showReset =
    div
        []
        [ button [ onClick Reset ] [ text "Reset" ] ]
