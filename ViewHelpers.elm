module ViewHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


primaryButton : msg -> String -> Html msg
primaryButton msg name =
    button [ class "primary", onClick msg ] [ text name ]


alert : msg -> Maybe String -> Html msg
alert msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick msg] [ text "x" ]
                , text message
                ]

        Nothing ->
            text ""
