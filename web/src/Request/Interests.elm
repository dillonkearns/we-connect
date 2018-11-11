module Request.Interests exposing (addInterest, createUser, getInterests)

import Api.Enum.Interest as Interest exposing (Interest)
import Api.Mutation
import Api.Object.User
import Api.Query
import Api.Scalar
import Graphql.Field as Field
import Graphql.Operation exposing (RootMutation, RootQuery)
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


getInterests : String -> SelectionSet (List Interest) RootQuery
getInterests username =
    Api.Query.user
        { where_ =
            { id = Absent
            , name = Absent
            }
        }
        (Api.Object.User.interests |> fieldSelection)
        |> Field.nonNullOrFail
        |> fieldSelection


addInterest : String -> Interest -> SelectionSet (List Interest) RootMutation
addInterest userName interest =
    Api.Mutation.updateUser
        { data =
            { interests =
                Present
                    { set = Present [ interest ]
                    }
            , name = Absent

            -- { name : (OptionalArgument String), interests : (OptionalArgument UserUpdateinterestsInput) }
            }
        , where_ = { id = Absent, name = Present userName }
        }
        (Api.Object.User.interests |> fieldSelection)
        |> Field.nonNullOrFail
        |> fieldSelection



-- |> SelectionSet.map (\_ -> ())
-- |> SelectionSet.map (\_ -> ())
