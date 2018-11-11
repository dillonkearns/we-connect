module Main exposing (main)

import Browser
import Element
import Html


init flags =
    ( (), Cmd.none )


view model =
    { title = "WeConnect"
    , body =
        [ Element.layout [] (mainView model) ]
    }


mainView model =
    Element.text "123"


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
