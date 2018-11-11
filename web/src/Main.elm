module Main exposing (main)

import Api.Object exposing (Interest)
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
import Request.TimeSlot
import View.Navbar


type Msg
    = EditedUsername String
    | SetUsername
    | NoOp (RemoteData (Graphql.Http.Error ()) ())
    | SignedUpForTime (RemoteData (Graphql.Http.Error ()) ())
    | AddInterest String
    | SignupForTime String
    | GotInterests (RemoteData (Graphql.Http.Error ()) (List String))
    | GotAllInterests (RemoteData (Graphql.Http.Error ()) (List Request.Interests.Interest))
    | GotTimeSlots (RemoteData (Graphql.Http.Error ()) (List Request.TimeSlot.TimeSlot))
    | GotMatches (RemoteData (Graphql.Http.Error ()) (List Request.TimeSlot.Availability))


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
    , userInterests : RemoteData (Graphql.Http.Error ()) (List String)
    , allInterests : RemoteData (Graphql.Http.Error ()) (List Request.Interests.Interest)
    , timeSlots : RemoteData (Graphql.Http.Error ()) (List Request.TimeSlot.TimeSlot)
    , matches : RemoteData (Graphql.Http.Error ()) (List Request.TimeSlot.Availability)
    }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        username =
            Entered "Dillon"
    in
    ( { -- username = Entering ""
        username = username
      , userInterests = RemoteData.Loading
      , allInterests = RemoteData.Loading
      , timeSlots = RemoteData.Loading
      , matches = RemoteData.Loading
      }
    , Cmd.batch
        [ getAllInterests
        , getTimeSlots
        , getUserInterests (getUsername username)
        , getMatches (getUsername username)
        ]
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


type alias SuccessData =
    { allInterests : List Request.Interests.Interest
    , userInterests : List String
    , timeSlots : List Request.TimeSlot.TimeSlot
    }


mainView : Model -> Element Msg
mainView model =
    case model.username of
        Entering usernameInput ->
            usernameView usernameInput

        Entered username ->
            case
                RemoteData.map3 SuccessData
                    model.allInterests
                    model.userInterests
                    model.timeSlots
            of
                RemoteData.Success { allInterests, userInterests, timeSlots } ->
                    let
                        availabilities =
                            []

                        slotCounts =
                            Request.TimeSlot.userInterestsToSlotCounts userInterests
                                availabilities
                    in
                    Element.row [ Element.spacing 20 ]
                        [ interestsView allInterests userInterests
                        , timeSlotsView timeSlots
                        , slotCountsView userInterests model.matches
                        ]

                _ ->
                    Element.text "Loading..."


slotCountsView userInterests timeSlots =
    -- slotCounts : List { time : String, things : List { interest : String, count : Int } }
    case
        timeSlots
    of
        RemoteData.Success success ->
            Request.TimeSlot.userInterestsToSlotCounts userInterests success
                |> Debug.toString
                |> Element.text

        _ ->
            Element.text "..."


timeSlotsView : List Request.TimeSlot.TimeSlot -> Element Msg
timeSlotsView timeSlots =
    Element.column [ Element.spacing 10 ]
        (timeSlots |> List.map timeSlotView)


timeSlotView : Request.TimeSlot.TimeSlot -> Element Msg
timeSlotView timeSlot =
    if timeSlot.userIsAvailable then
        (timeSlot.time ++ " ✔")
            |> Element.text
            |> button

    else
        timeSlot.time
            |> Element.text
            |> button
            |> Element.el [ Element.Events.onClick (SignupForTime timeSlot.time) ]


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


interestsView : List Request.Interests.Interest -> List String -> Element Msg
interestsView allInterests userInterests =
    allInterests
        |> List.map (interestButton userInterests)
        |> Element.column
            [ Element.spacing 10
            ]


interestButton : List String -> Request.Interests.Interest -> Element Msg
interestButton userInterests interest =
    if List.member interest.name userInterests then
        ("✔ " ++ interest.name ++ " (" ++ String.fromInt interest.interestedCount ++ ")")
            |> Element.text
            |> button
            |> Element.el
                [-- Element.Events.onClick (AddInterest interest.name)
                ]

    else
        interest.name
            |> Element.text
            |> button
            |> Element.el
                [ Element.Events.onClick (AddInterest interest.name) ]


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
            ( model, addInterest model.username interest )

        SignupForTime timeDescription ->
            ( model, signupForTime (getUsername model.username) timeDescription )

        GotInterests interestsResult ->
            ( { model | userInterests = interestsResult }, Cmd.none )

        GotAllInterests interestsResult ->
            ( { model | allInterests = interestsResult }, Cmd.none )

        GotTimeSlots timeSlotsResponse ->
            ( { model | timeSlots = timeSlotsResponse }, Cmd.none )

        SignedUpForTime response ->
            case response of
                RemoteData.Success _ ->
                    ( model, getTimeSlots )

                _ ->
                    ( model, Cmd.none )

        GotMatches matchesResponse ->
            ( { model | matches = matchesResponse }, Cmd.none )


addInterest : Username -> String -> Cmd Msg
addInterest username interest =
    Request.Interests.addInterest (getUsername username) interest
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (mapError >> GotInterests)


createUser : String -> Cmd Msg
createUser username =
    Request.Interests.createUser username
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (mapError >> NoOp)


getUserInterests : String -> Cmd Msg
getUserInterests username =
    Request.Interests.getUserInterests username
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (mapError >> GotInterests)


getAllInterests : Cmd Msg
getAllInterests =
    Request.Interests.getInterests
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (mapError >> GotAllInterests)


getTimeSlots : Cmd Msg
getTimeSlots =
    Request.TimeSlot.getAll
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (mapError >> GotTimeSlots)


getMatches : String -> Cmd Msg
getMatches username =
    Request.TimeSlot.matches username
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (mapError >> GotMatches)


signupForTime : String -> String -> Cmd Msg
signupForTime username timeDescription =
    Request.TimeSlot.signup username timeDescription
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (mapError >> SignedUpForTime)


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


apiUrl : String
apiUrl =
    "https://eu1.prisma.sh/dillon-kearns-bf5811/we-connect/dev"


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
