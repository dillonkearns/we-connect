module Request.Interests exposing (addInterest, createUser)

import Api.Enum.Interest as Interest exposing (Interest)
import Api.Mutation
import Api.Scalar
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


addInterest : String -> Interest -> SelectionSet () RootMutation
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
        SelectionSet.empty
        |> fieldSelection
        |> SelectionSet.map (\_ -> ())



-- |> SelectionSet.map (\_ -> ())
