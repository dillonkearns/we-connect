module Request.TimeSlot exposing (TimeSlot, getAll)

import Api.Object
import Api.Object.TimeSlot
import Api.Object.User
import Api.Query
import Graphql.Field as Field exposing (Field)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, fieldSelection, hardcoded, with)


type alias TimeSlot =
    { time : String
    , userIsAvailable : Bool
    }


getAll : SelectionSet (List TimeSlot) RootQuery
getAll =
    Api.Query.timeSlots identity timeSlotsSelection
        |> Field.nonNullElementsOrFail
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
