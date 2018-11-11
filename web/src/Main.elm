module Main exposing (main)

import Api.Enum.Interest exposing (Interest)
import Browser
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Input
import Graphql.Http
import Html
import Request.Interests
import View.Navbar


type Msg
    = EditedUsername String
    | SetUsername
    | NoOp (Result.Result (Graphql.Http.Error ()) ())
    | AddInterest Interest


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
            ]


interestButton interest =
    interest
        |> Debug.toString
        |> Element.text
        |> button
        |> Element.el [ Element.Events.onClick (AddInterest interest) ]


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
            ( { model | username = setUsername model.username }
            , createUser (getUsername model.username)
            )

        NoOp _ ->
            ( model, Cmd.none )

        AddInterest interest ->
            ( model, addInterest "" interest )


addInterest : String -> Interest -> Cmd Msg
addInterest userId interest =
    Request.Interests.addInterest "" interest
        |> Graphql.Http.mutationRequest "https://eu1.prisma.sh/dillon-kearns-bf5811/we-connect/dev"
        |> Graphql.Http.send NoOp


createUser : String -> Cmd Msg
createUser username =
    Request.Interests.createUser username
        |> Graphql.Http.mutationRequest "https://eu1.prisma.sh/dillon-kearns-bf5811/we-connect/dev"
        |> Graphql.Http.send NoOp


setUsername username =
    case username of
        Entering input ->
            Entered input

        _ ->
            username


getUsername username =
    case username of
        Entering input ->
            input

        Entered name ->
            name


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
