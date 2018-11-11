module Main exposing (main)

import Api.Enum.Interest
import Browser
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Input
import Html
import View.Navbar


type Msg
    = EditedUsername String
    | SetUsername


type Username
    = Entering String
    | Entered String


type alias Model =
    { username : Username }


init flags =
    ( { username = Entering ""
      }
    , Cmd.none
    )


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
    case model.username of
        Entering usernameInput ->
            usernameView usernameInput

        Entered username ->
            Element.column [ Element.spacing 10 ]
                [ Element.text "Please select your interests..."
                , interestsView model
                ]


usernameView username =
    Element.column [ Element.spacing 10 ]
        [ Element.Input.text []
            { onChange = EditedUsername
            , text = username
            , placeholder = Nothing
            , label = Element.text "Username" |> Element.Input.labelAbove []
            }
        , "Enter" |> Element.text |> button |> Element.el [ Element.Events.onClick SetUsername ]
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
        |> button


button content =
    content
        |> Element.el
            [ Element.Border.width 2
            , Element.padding 10
            , Element.Border.rounded 5
            , Element.Background.color (Element.rgba255 0 200 200 1)
            , Element.pointer
            , Element.mouseOver
                [ Element.Background.color (Element.rgba255 0 200 200 0.8)
                ]
            ]


update msg model =
    case msg of
        EditedUsername usernameInput ->
            ( { model | username = Entering usernameInput }, Cmd.none )

        SetUsername ->
            ( { model | username = setUsername model.username }, Cmd.none )


setUsername username =
    case username of
        Entering input ->
            Entered input

        _ ->
            username


subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
