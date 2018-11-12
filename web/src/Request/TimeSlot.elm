module Request.TimeSlot exposing (Availability, TimeSlot, availableSlots, getAll, matches, meetsMinimum, signup, userInterestsToSlotCounts)

import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Interest
import Api.Object.TimeSlot
import Api.Object.User
import Api.Query
import Graphql.Field as Field exposing (Field)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, fieldSelection, hardcoded, with)


type alias TimeSlot =
    { time : String
    , userIsAvailable : Bool
    }


availableSlots : List Availability -> List String -> List String
availableSlots availabilities userTimes =
    userTimes
        |> List.filter
            (\time ->
                let
                    maybeAvailability =
                        findAvailabilityByTime time availabilities
                in
                case maybeAvailability of
                    Just availability ->
                        availability.interests == []

                    Nothing ->
                        False
            )


findAvailabilityByTime : String -> List Availability -> Maybe Availability
findAvailabilityByTime time availabilities =
    availabilities
        |> List.filter (\availability -> availability.time == time)
        |> List.head


type alias Availability =
    { time : String
    , interests : List (List String)
    }


whereUserIsAvailable : List String -> List Availability -> List Availability
whereUserIsAvailable userAvailableTimes availabilities =
    availabilities
        |> List.filter (\availability -> List.member availability.time userAvailableTimes)


meetsMinimum : List { time : String, things : List { interest : String, count : Int } } -> List { time : String, things : List { interest : String, count : Int } }
meetsMinimum availabilities =
    availabilities
        |> List.filter (\availability -> List.any (\thing -> thing.count > 2) availability.things)


userInterestsToSlotCounts : List String -> List Availability -> List { time : String, things : List { interest : String, count : Int } }
userInterestsToSlotCounts userInterests availabilities =
    availabilities
        |> List.map
            (\availability ->
                { time = availability.time
                , things =
                    List.map (userInterestWithCount availability) userInterests
                }
            )


userInterestWithCount : Availability -> String -> { interest : String, count : Int }
userInterestWithCount availability userInterest =
    { interest = userInterest
    , count =
        availability.interests
            |> List.filter (\lists -> List.member userInterest lists)
            |> List.length
    }


matches : String -> SelectionSet (List Availability) RootQuery
matches username =
    Api.Query.user
        { where_ =
            { id = Absent, name = Present username }
        }
        userSelection
        |> Field.nonNullOrFail
        |> fieldSelection


userSelection : SelectionSet (List Availability) Api.Object.User
userSelection =
    (Api.Object.User.availability identity
        timeSlotSelection
        |> Field.nonNullOrFail
    )
        |> fieldSelection


timeSlotSelection : SelectionSet Availability Api.Object.TimeSlot
timeSlotSelection =
    Api.Object.TimeSlot.selection Availability
        |> with Api.Object.TimeSlot.time
        |> with
            (Api.Object.TimeSlot.users identity
                (Api.Object.User.interests identity
                    (Api.Object.Interest.name |> fieldSelection)
                    |> Field.nonNullOrFail
                    |> fieldSelection
                )
                |> Field.nonNullOrFail
            )


getAll : Maybe String -> SelectionSet (List TimeSlot) RootQuery
getAll username =
    Api.Query.timeSlots
        (\input ->
            { input
                | where_ =
                    Absent
            }
        )
        (timeSlotsSelection username)
        |> Field.nonNullElementsOrFail
        |> fieldSelection


signup : String -> String -> SelectionSet () RootMutation
signup username timeDescription =
    Api.Mutation.updateUser
        { where_ =
            Api.InputObject.buildUserWhereUniqueInput
                (\input -> { input | name = Present username })
        , data =
            Api.InputObject.buildUserUpdateInput
                (\input ->
                    { input
                        | availability =
                            Present
                                (Api.InputObject.buildTimeSlotUpdateManyWithoutUsersInput
                                    (\input2 ->
                                        { input2
                                            | connect =
                                                Present
                                                    [ Api.InputObject.buildTimeSlotWhereUniqueInput
                                                        (\input3 ->
                                                            { input3 | time = Present timeDescription }
                                                        )
                                                    ]
                                        }
                                    )
                                )
                    }
                )
        }
        SelectionSet.empty
        |> Field.map (\_ -> ())
        |> fieldSelection


timeSlotsSelection : Maybe String -> SelectionSet TimeSlot Api.Object.TimeSlot
timeSlotsSelection username =
    Api.Object.TimeSlot.selection TimeSlot
        |> with Api.Object.TimeSlot.time
        |> with (userIsAvailableField username)


userIsAvailableField : Maybe String -> Field Bool Api.Object.TimeSlot
userIsAvailableField maybeUsername =
    Api.Object.TimeSlot.users identity
        (Api.Object.User.name
            |> fieldSelection
        )
        |> Field.nonNullOrFail
        |> Field.map
            (\list ->
                case maybeUsername of
                    Just username ->
                        List.member username list

                    Nothing ->
                        False
            )
