module Main exposing (main)

import Browser exposing (Document)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Input
import Graphql.Http
import Html
import RemoteData exposing (RemoteData)
import Request.Interests
import View.Navbar


type Msg
    = EditedUsername String
    | SetUsername
    | NoOp (Result.Result (Graphql.Http.Error ()) ())
    | AddInterest String
    | GotInterests (Result.Result (Graphql.Http.Error (List String)) (List String))
    | GotAllInterests (RemoteData (Graphql.Http.Error ()) (List String))


mapError : Result (Graphql.Http.Error something) something -> RemoteData (Graphql.Http.Error ()) something
mapError result =
    result
        |> RemoteData.fromResult
        |> RemoteData.mapError Graphql.Http.ignoreParsedErrorData


type Username
    = Entering String
    | Entered String


type alias Model =
    { username : Username
    , userInterests : List String
    , allInterests : RemoteData (Graphql.Http.Error ()) (List String)
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { username = Entering ""
      , userInterests = []
      , allInterests = RemoteData.Loading
      }
    , getAllInterests
    )


view : Model -> Document Msg
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


mainView : Model -> Element Msg
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


interestsView : Model -> Element Msg
interestsView model =
    case model.allInterests of
        RemoteData.Success allInterests ->
            allInterests
                |> List.map (interestButton model.userInterests)
                |> Element.column
                    [ Element.spacing 10
                    ]

        _ ->
            Element.text "..."


interestButton : List String -> String -> Element Msg
interestButton userInterests interest =
    if List.member interest userInterests then
        ("✔ " ++ interest)
            |> Element.text
            |> button
            |> Element.el
                [ Element.Events.onClick (AddInterest interest) ]

    else
        interest
            |> Element.text
            |> button
            |> Element.el
                [ Element.Events.onClick (AddInterest interest) ]


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


update : Msg -> Model -> ( Model, Cmd Msg )
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
            ( { model | allInterests = interestsResult }, Cmd.none )


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
        |> Graphql.Http.send (mapError >> GotAllInterests)


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
