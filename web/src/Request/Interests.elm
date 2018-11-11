module Request.Interests exposing (addInterest, createUser, getInterests)

import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Interest
import Api.Object.User
import Api.Query
import Api.Scalar
import Graphql.Field as Field
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, fieldSelection, with)


type alias Interest =
    String


createUser : String -> SelectionSet () RootMutation
createUser username =
    Api.Mutation.createUser
        { data =
            Api.InputObject.buildUserCreateInput { name = username } identity
        }
        SelectionSet.empty
        |> fieldSelection


getInterests : String -> SelectionSet (List String) RootQuery
getInterests username =
    Api.Query.user
        { where_ =
            { id = Absent
            , name = Present username
            }
        }
        interestsSelection
        |> Field.nonNullOrFail
        |> fieldSelection


interestsSelection : SelectionSet (List String) Api.Object.User
interestsSelection =
    Api.Object.User.interests identity interestNameSelection
        |> Field.nonNullOrFail
        |> fieldSelection


interestNameSelection : SelectionSet String Api.Object.Interest
interestNameSelection =
    Api.Object.Interest.name |> fieldSelection


addInterest : String -> List Interest -> Interest -> SelectionSet (List Interest) RootMutation
addInterest username interests interest =
    Api.Mutation.updateUser
        { data =
            Api.InputObject.buildUserUpdateInput
                (\input ->
                    { input
                        | interests =
                            Present
                                (Api.InputObject.buildInterestUpdateManyWithoutInterestedUsersInput
                                    (\interestInput ->
                                        { interestInput
                                            | connect =
                                                Present
                                                    [ { id = Absent, name = Present interest }
                                                    ]
                                        }
                                    )
                                )
                    }
                )
        , where_ = { id = Absent, name = Present username }
        }
        interestsSelection
        |> Field.nonNullOrFail
        |> fieldSelection



-- |> SelectionSet.map (\_ -> ())
-- |> SelectionSet.map (\_ -> ())
