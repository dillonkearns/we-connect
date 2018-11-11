module Request.Interests exposing (createUser)

import Api.Mutation
import Graphql.Field as Field
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, fieldSelection, with)


createUser : String -> SelectionSet () RootMutation
createUser username =
    Api.Mutation.createUser
        { data =
            { name = username, interests = Absent }
        }
        SelectionSet.empty
        |> fieldSelection
