module Main exposing (main)

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
    | AddInterest String
    | GotInterests (Result.Result (Graphql.Http.Error (List String)) (List String))
    | GotAllInterests (Result.Result (Graphql.Http.Error (List String)) (List String))


type Username
    = Entering String
    | Entered String


type alias Model =
    { username : Username
    , userInterests : List String
    , allInterests : List String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { username = Entering ""
      , userInterests = []
      , allInterests = []
      }
    , getAllInterests
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
                [ Element.text ("Your interests so far: " ++ Debug.toString model.userInterests)
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
    model.allInterests
        |> List.map interestButton
        |> Element.column
            [ Element.spacing 10
            ]


interestButton interest =
    interest
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
            , Cmd.batch
                [ createUser (getUsername model.username)
                , getUserInterests (getUsername model.username)
                ]
            )

        NoOp _ ->
            ( model, Cmd.none )

        AddInterest interest ->
            ( model, addInterest model.username model.userInterests interest )

        GotInterests interestsResult ->
            case interestsResult of
                Ok latestInterests ->
                    ( { model | userInterests = latestInterests }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotAllInterests interestsResult ->
            case interestsResult of
                Ok latestInterests ->
                    ( { model | allInterests = latestInterests }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


addInterest : Username -> List String -> String -> Cmd Msg
addInterest username currentInterests interest =
    Request.Interests.addInterest (getUsername username) currentInterests interest
        |> Graphql.Http.mutationRequest "https://eu1.prisma.sh/dillon-kearns-bf5811/we-connect/dev"
        |> Graphql.Http.send GotInterests


createUser : String -> Cmd Msg
createUser username =
    Request.Interests.createUser username
        |> Graphql.Http.mutationRequest "https://eu1.prisma.sh/dillon-kearns-bf5811/we-connect/dev"
        |> Graphql.Http.send NoOp


getUserInterests : String -> Cmd Msg
getUserInterests username =
    Request.Interests.getUserInterests username
        |> Graphql.Http.queryRequest "https://eu1.prisma.sh/dillon-kearns-bf5811/we-connect/dev"
        |> Graphql.Http.send GotInterests


getAllInterests : Cmd Msg
getAllInterests =
    Request.Interests.getInterests
        |> Graphql.Http.queryRequest "https://eu1.prisma.sh/dillon-kearns-bf5811/we-connect/dev"
        |> Graphql.Http.send GotAllInterests


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
