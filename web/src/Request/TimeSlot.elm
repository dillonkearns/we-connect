module Request.TimeSlot exposing (TimeSlot, getAll, signup)

import Api.InputObject
import Api.Mutation
import Api.Object
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


type alias Availability =
    { time : String
    , interests : List (List String)
    }


matches : String -> SelectionSet (List Availability) RootQuery
matches username =
    Api.Query.user
        { where_ =
            { id = Absent, name = Present username }
        }
        (Api.Object.User.availability identity
            (SelectionSet.empty |> SelectionSet.map (\_ -> { time = "", interests = [] }))
            |> Field.nonNullOrFail
            |> fieldSelection
        )
        |> Field.nonNullOrFail
        |> fieldSelection


getAll : SelectionSet (List TimeSlot) RootQuery
getAll =
    Api.Query.timeSlots identity timeSlotsSelection
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


timeSlotsSelection : SelectionSet TimeSlot Api.Object.TimeSlot
timeSlotsSelection =
    Api.Object.TimeSlot.selection TimeSlot
        |> with Api.Object.TimeSlot.time
        |> with userIsAvailableField


userIsAvailableField : Field Bool Api.Object.TimeSlot
userIsAvailableField =
    Api.Object.TimeSlot.users identity
        (Api.Object.User.id
            |> fieldSelection
        )
        |> Field.nonNullOrFail
        |> Field.map (\list -> not (List.isEmpty list))
