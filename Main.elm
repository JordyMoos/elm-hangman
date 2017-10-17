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


init : Model
init =
    Model (asLetters [ 'W', 'O', 'R', 'K', 'S', 'H', 'O', 'P' ]) 10 Playing


asLetters : List Char -> List Letter
asLetters word =
    List.map (\c -> Letter (Char.toUpper c) False) word


type Msg
    = GuessChar String
    | Reset


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case ( model.state, msg ) of
        ( Playing, GuessChar inputValue ) ->
            case uncons inputValue of
                Just ( guessedCharRaw, "" ) ->
                    let
                        guessedChar =
                            Char.toUpper guessedCharRaw

                        weHaveGuessedChar =
                            List.any (\letter -> letter.char == guessedChar)

                        updateLetters =
                            List.map
                                (\letter ->
                                    if letter.char == guessedChar then
                                        { letter | isGuessed = True }
                                    else
                                        letter
                                )

                        applyGuessToWord model =
                            { model | word = updateLetters model.word }

                        isGuessed letter =
                            letter.isGuessed

                        allIsGuessed =
                            List.all isGuessed

                        winWhenDone model =
                            if allIsGuessed model.word then
                                { model | state = Won }
                            else
                                model

                        penaltyWhenWrongGuess model =
                            if weHaveGuessedChar model.word then
                                model
                            else
                                { model | attemptsLeft = model.attemptsLeft - 1 }

                        loseIfOutOfAttemps model =
                            if model.attemptsLeft < 1 then
                                { model | state = Lost }
                            else
                                model

                        newModel =
                            model
                                |> applyGuessToWord
                                |> penaltyWhenWrongGuess
                                |> loseIfOutOfAttemps
                                |> winWhenDone
                    in
                        newModel

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
        [ h1 [] [ text "Hangman the Game!" ]
        , showWord model.word
        , showGameState model
        , showResetButton
        ]


showGameState : Model -> Html Msg
showGameState model =
    case model.state of
        Playing ->
            showPlaying model

        Won ->
            showWon

        Lost ->
            showLost


showPlaying : Model -> Html Msg
showPlaying model =
    div
        []
        [ showInput ""
        , showAttemptsLeft model.attemptsLeft
        ]


showLost : Html Msg
showLost =
    h1 [] [ text "Too bad you lost :(" ]


showWon : Html Msg
showWon =
    h1 [] [ text "Jeej you won!" ]


showWord : List Letter -> Html Msg
showWord word =
    List.map showLetter word
        |> ul []


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


showInput : String -> Html Msg
showInput inputValue =
    div
        []
        [ input [ onInput GuessChar, value inputValue ] [] ]


showAttemptsLeft : Int -> Html Msg
showAttemptsLeft attemptsLeft =
    text (toString attemptsLeft)


showResetButton : Html Msg
showResetButton =
    button
        [ onClick Reset ]
        [ text "Reset" ]
