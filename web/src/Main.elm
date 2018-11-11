module Main exposing (main)

import Browser
import Html


init flags =
    ( (), Cmd.none )


view model =
    { title = "WeConnect"
    , body = [ Html.text "Hello!" ]
    }


update msg model =
    ( model, Cmd.none )


subscriptions model =
    Sub.none


main : Program () () ()
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
