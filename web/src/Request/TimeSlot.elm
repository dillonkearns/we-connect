module Request.TimeSlot exposing (TimeSlot, getAll)

import Api.Object.TimeSlot
import Api.Query
import Graphql.Field as Field
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, fieldSelection)


type alias TimeSlot =
    String


getAll : SelectionSet (List TimeSlot) RootQuery
getAll =
    Api.Query.timeSlots identity
        (Api.Object.TimeSlot.time |> fieldSelection)
        |> Field.nonNullElementsOrFail
        |> fieldSelection
