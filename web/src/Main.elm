module Main exposing (main)

import Api.Enum.Interest
import Browser
import Element
import Element.Background
import Element.Border
import Html
import View.Navbar


init flags =
    ( (), Cmd.none )


view model =
    { title = "WeConnect"
    , body =
        [ Element.layout [ Element.padding 30 ]
            (Element.column [ Element.spacing 20 ]
                [ View.Navbar.view
                , mainView model
                ]
            )
        ]
    }


mainView model =
    Element.column [ Element.spacing 10 ]
        [ Element.text "Please select your interests..."
        , interestsView model
        ]


interestsView model =
    Api.Enum.Interest.list
        |> List.map interestButton
        |> Element.column
            [ Element.spacing 10
            , Element.centerX
            ]


interestButton interest =
    interest
        |> Debug.toString
        |> Element.text
        |> Element.el
            [ Element.Border.width 2
            , Element.padding 10
            , Element.Border.rounded 5
            , Element.Background.color (Element.rgba255 0 200 200 1)
            ]


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
