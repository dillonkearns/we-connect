module Request.Interests exposing (Interest, addInterest, createUser, getInterests, getUserInterests)

import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Interest
import Api.Object.User
import Api.Query
import Api.Scalar
import Graphql.Field as Field exposing (Field)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, fieldSelection, with)


createUser : String -> SelectionSet () RootMutation
createUser username =
    Api.Mutation.createUser
        { data =
            Api.InputObject.buildUserCreateInput { name = username } identity
        }
        SelectionSet.empty
        |> fieldSelection


type alias Interest =
    { name : String
    , interestedCount : Int
    }


getInterests : SelectionSet (List Interest) RootQuery
getInterests =
    Api.Query.interests identity interestSelection
        |> Field.nonNullElementsOrFail
        |> fieldSelection


interestSelection : SelectionSet Interest Api.Object.Interest
interestSelection =
    Api.Object.Interest.selection Interest
        |> with Api.Object.Interest.name
        |> with interestedUserCountSelection


interestedUserCountSelection : Field Int Api.Object.Interest
interestedUserCountSelection =
    Api.Object.Interest.interestedUsers identity SelectionSet.empty
        |> Field.map (Maybe.withDefault [])
        |> Field.map List.length


getUserInterests : String -> SelectionSet (List String) RootQuery
getUserInterests username =
    Api.Query.user
        { where_ =
            { id = Absent
            , name = Present username
            }
        }
        interestsSelection
        |> Field.map (Maybe.withDefault [])
        |> fieldSelection


interestsSelection : SelectionSet (List String) Api.Object.User
interestsSelection =
    Api.Object.User.interests identity interestNameSelection
        |> Field.map (Maybe.withDefault [])
        |> fieldSelection


interestNameSelection : SelectionSet String Api.Object.Interest
interestNameSelection =
    Api.Object.Interest.name |> fieldSelection


addInterest : String -> String -> SelectionSet (List String) RootMutation
addInterest username interest =
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
        |> Field.map (Maybe.withDefault [])
        |> fieldSelection



-- |> SelectionSet.map (\_ -> ())
-- |> SelectionSet.map (\_ -> ())
