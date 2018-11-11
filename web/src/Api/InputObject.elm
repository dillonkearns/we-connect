-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.InputObject exposing (..)


import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Field as Field exposing (Field)
import Graphql.Internal.Builder.Object as Object
import Graphql.SelectionSet exposing (SelectionSet)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Api.Object
import Api.Interface
import Api.Union
import Api.Scalar
import Json.Decode as Decode
import Graphql.Internal.Encode as Encode exposing (Value)
import Api.Enum.Interest
import Api.Enum.MutationType
import Api.Enum.Interest



buildUserCreateInput : UserCreateInputRequiredFields -> (UserCreateInputOptionalFields -> UserCreateInputOptionalFields) -> UserCreateInput
buildUserCreateInput required fillOptionals =

    let
        optionals =
            
            fillOptionals
                { interests = Absent }
    in
    { name = required.name, interests = optionals.interests }

type alias UserCreateInputRequiredFields =
    { name : String }
type alias UserCreateInputOptionalFields =
    { interests : (OptionalArgument UserCreateinterestsInput) }


{-| Type for the UserCreateInput input object.
-}
type alias UserCreateInput =
    { name : String, interests : (OptionalArgument UserCreateinterestsInput) }
    

{-| Encode a UserCreateInput into a value that can be used as an argument.
-}
encodeUserCreateInput : UserCreateInput -> Value
encodeUserCreateInput input =
    Encode.maybeObject
        [ ( "name", (Encode.string)  input.name |> Just ), ( "interests", (encodeUserCreateinterestsInput)  |> Encode.optional input.interests ) ]


buildUserCreateinterestsInput : (UserCreateinterestsInputOptionalFields -> UserCreateinterestsInputOptionalFields) -> UserCreateinterestsInput
buildUserCreateinterestsInput fillOptionals =

    let
        optionals =
            
            fillOptionals
                { set = Absent }
    in
    { set = optionals.set }


type alias UserCreateinterestsInputOptionalFields =
    { set : (OptionalArgument (List Api.Enum.Interest.Interest)) }


{-| Type for the UserCreateinterestsInput input object.
-}
type alias UserCreateinterestsInput =
    { set : (OptionalArgument (List Api.Enum.Interest.Interest)) }
    

{-| Encode a UserCreateinterestsInput into a value that can be used as an argument.
-}
encodeUserCreateinterestsInput : UserCreateinterestsInput -> Value
encodeUserCreateinterestsInput input =
    Encode.maybeObject
        [ ( "set", ((Encode.enum Api.Enum.Interest.toString) |> Encode.list)  |> Encode.optional input.set ) ]


buildUserSubscriptionWhereInput : (UserSubscriptionWhereInputOptionalFields -> UserSubscriptionWhereInputOptionalFields) -> UserSubscriptionWhereInput
buildUserSubscriptionWhereInput fillOptionals =

    let
        optionals =
            
            fillOptionals
                { and = Absent, or = Absent, not = Absent, mutation_in = Absent, updatedFields_contains = Absent, updatedFields_contains_every = Absent, updatedFields_contains_some = Absent, node = Absent }
    in
    UserSubscriptionWhereInput{ and = optionals.and, or = optionals.or, not = optionals.not, mutation_in = optionals.mutation_in, updatedFields_contains = optionals.updatedFields_contains, updatedFields_contains_every = optionals.updatedFields_contains_every, updatedFields_contains_some = optionals.updatedFields_contains_some, node = optionals.node }


type alias UserSubscriptionWhereInputOptionalFields =
    { and : (OptionalArgument (List UserSubscriptionWhereInput)), or : (OptionalArgument (List UserSubscriptionWhereInput)), not : (OptionalArgument (List UserSubscriptionWhereInput)), mutation_in : (OptionalArgument (List Api.Enum.MutationType.MutationType)), updatedFields_contains : (OptionalArgument String), updatedFields_contains_every : (OptionalArgument (List String)), updatedFields_contains_some : (OptionalArgument (List String)), node : (OptionalArgument UserWhereInput) }


{-| Type alias for the `UserSubscriptionWhereInput` attributes. Note that this type
needs to use the `UserSubscriptionWhereInput` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias UserSubscriptionWhereInputRaw =
    { and : (OptionalArgument (List UserSubscriptionWhereInput)), or : (OptionalArgument (List UserSubscriptionWhereInput)), not : (OptionalArgument (List UserSubscriptionWhereInput)), mutation_in : (OptionalArgument (List Api.Enum.MutationType.MutationType)), updatedFields_contains : (OptionalArgument String), updatedFields_contains_every : (OptionalArgument (List String)), updatedFields_contains_some : (OptionalArgument (List String)), node : (OptionalArgument UserWhereInput) }


{-| Type for the UserSubscriptionWhereInput input object.
-}
type UserSubscriptionWhereInput
    = UserSubscriptionWhereInput UserSubscriptionWhereInputRaw
    

{-| Encode a UserSubscriptionWhereInput into a value that can be used as an argument.
-}
encodeUserSubscriptionWhereInput : UserSubscriptionWhereInput -> Value
encodeUserSubscriptionWhereInput (UserSubscriptionWhereInput input) =
    Encode.maybeObject
        [ ( "AND", (encodeUserSubscriptionWhereInput |> Encode.list)  |> Encode.optional input.and ), ( "OR", (encodeUserSubscriptionWhereInput |> Encode.list)  |> Encode.optional input.or ), ( "NOT", (encodeUserSubscriptionWhereInput |> Encode.list)  |> Encode.optional input.not ), ( "mutation_in", ((Encode.enum Api.Enum.MutationType.toString) |> Encode.list)  |> Encode.optional input.mutation_in ), ( "updatedFields_contains", (Encode.string)  |> Encode.optional input.updatedFields_contains ), ( "updatedFields_contains_every", (Encode.string |> Encode.list)  |> Encode.optional input.updatedFields_contains_every ), ( "updatedFields_contains_some", (Encode.string |> Encode.list)  |> Encode.optional input.updatedFields_contains_some ), ( "node", (encodeUserWhereInput)  |> Encode.optional input.node ) ]


buildUserUpdateInput : (UserUpdateInputOptionalFields -> UserUpdateInputOptionalFields) -> UserUpdateInput
buildUserUpdateInput fillOptionals =

    let
        optionals =
            
            fillOptionals
                { name = Absent, interests = Absent }
    in
    { name = optionals.name, interests = optionals.interests }


type alias UserUpdateInputOptionalFields =
    { name : (OptionalArgument String), interests : (OptionalArgument UserUpdateinterestsInput) }


{-| Type for the UserUpdateInput input object.
-}
type alias UserUpdateInput =
    { name : (OptionalArgument String), interests : (OptionalArgument UserUpdateinterestsInput) }
    

{-| Encode a UserUpdateInput into a value that can be used as an argument.
-}
encodeUserUpdateInput : UserUpdateInput -> Value
encodeUserUpdateInput input =
    Encode.maybeObject
        [ ( "name", (Encode.string)  |> Encode.optional input.name ), ( "interests", (encodeUserUpdateinterestsInput)  |> Encode.optional input.interests ) ]


buildUserUpdateManyMutationInput : (UserUpdateManyMutationInputOptionalFields -> UserUpdateManyMutationInputOptionalFields) -> UserUpdateManyMutationInput
buildUserUpdateManyMutationInput fillOptionals =

    let
        optionals =
            
            fillOptionals
                { name = Absent, interests = Absent }
    in
    { name = optionals.name, interests = optionals.interests }


type alias UserUpdateManyMutationInputOptionalFields =
    { name : (OptionalArgument String), interests : (OptionalArgument UserUpdateinterestsInput) }


{-| Type for the UserUpdateManyMutationInput input object.
-}
type alias UserUpdateManyMutationInput =
    { name : (OptionalArgument String), interests : (OptionalArgument UserUpdateinterestsInput) }
    

{-| Encode a UserUpdateManyMutationInput into a value that can be used as an argument.
-}
encodeUserUpdateManyMutationInput : UserUpdateManyMutationInput -> Value
encodeUserUpdateManyMutationInput input =
    Encode.maybeObject
        [ ( "name", (Encode.string)  |> Encode.optional input.name ), ( "interests", (encodeUserUpdateinterestsInput)  |> Encode.optional input.interests ) ]


buildUserUpdateinterestsInput : (UserUpdateinterestsInputOptionalFields -> UserUpdateinterestsInputOptionalFields) -> UserUpdateinterestsInput
buildUserUpdateinterestsInput fillOptionals =

    let
        optionals =
            
            fillOptionals
                { set = Absent }
    in
    { set = optionals.set }


type alias UserUpdateinterestsInputOptionalFields =
    { set : (OptionalArgument (List Api.Enum.Interest.Interest)) }


{-| Type for the UserUpdateinterestsInput input object.
-}
type alias UserUpdateinterestsInput =
    { set : (OptionalArgument (List Api.Enum.Interest.Interest)) }
    

{-| Encode a UserUpdateinterestsInput into a value that can be used as an argument.
-}
encodeUserUpdateinterestsInput : UserUpdateinterestsInput -> Value
encodeUserUpdateinterestsInput input =
    Encode.maybeObject
        [ ( "set", ((Encode.enum Api.Enum.Interest.toString) |> Encode.list)  |> Encode.optional input.set ) ]


buildUserWhereInput : (UserWhereInputOptionalFields -> UserWhereInputOptionalFields) -> UserWhereInput
buildUserWhereInput fillOptionals =

    let
        optionals =
            
            fillOptionals
                { and = Absent, or = Absent, not = Absent, id = Absent, id_not = Absent, id_in = Absent, id_not_in = Absent, id_lt = Absent, id_lte = Absent, id_gt = Absent, id_gte = Absent, id_contains = Absent, id_not_contains = Absent, id_starts_with = Absent, id_not_starts_with = Absent, id_ends_with = Absent, id_not_ends_with = Absent, name = Absent, name_not = Absent, name_in = Absent, name_not_in = Absent, name_lt = Absent, name_lte = Absent, name_gt = Absent, name_gte = Absent, name_contains = Absent, name_not_contains = Absent, name_starts_with = Absent, name_not_starts_with = Absent, name_ends_with = Absent, name_not_ends_with = Absent }
    in
    UserWhereInput{ and = optionals.and, or = optionals.or, not = optionals.not, id = optionals.id, id_not = optionals.id_not, id_in = optionals.id_in, id_not_in = optionals.id_not_in, id_lt = optionals.id_lt, id_lte = optionals.id_lte, id_gt = optionals.id_gt, id_gte = optionals.id_gte, id_contains = optionals.id_contains, id_not_contains = optionals.id_not_contains, id_starts_with = optionals.id_starts_with, id_not_starts_with = optionals.id_not_starts_with, id_ends_with = optionals.id_ends_with, id_not_ends_with = optionals.id_not_ends_with, name = optionals.name, name_not = optionals.name_not, name_in = optionals.name_in, name_not_in = optionals.name_not_in, name_lt = optionals.name_lt, name_lte = optionals.name_lte, name_gt = optionals.name_gt, name_gte = optionals.name_gte, name_contains = optionals.name_contains, name_not_contains = optionals.name_not_contains, name_starts_with = optionals.name_starts_with, name_not_starts_with = optionals.name_not_starts_with, name_ends_with = optionals.name_ends_with, name_not_ends_with = optionals.name_not_ends_with }


type alias UserWhereInputOptionalFields =
    { and : (OptionalArgument (List UserWhereInput)), or : (OptionalArgument (List UserWhereInput)), not : (OptionalArgument (List UserWhereInput)), id : (OptionalArgument Api.Scalar.Id), id_not : (OptionalArgument Api.Scalar.Id), id_in : (OptionalArgument (List Api.Scalar.Id)), id_not_in : (OptionalArgument (List Api.Scalar.Id)), id_lt : (OptionalArgument Api.Scalar.Id), id_lte : (OptionalArgument Api.Scalar.Id), id_gt : (OptionalArgument Api.Scalar.Id), id_gte : (OptionalArgument Api.Scalar.Id), id_contains : (OptionalArgument Api.Scalar.Id), id_not_contains : (OptionalArgument Api.Scalar.Id), id_starts_with : (OptionalArgument Api.Scalar.Id), id_not_starts_with : (OptionalArgument Api.Scalar.Id), id_ends_with : (OptionalArgument Api.Scalar.Id), id_not_ends_with : (OptionalArgument Api.Scalar.Id), name : (OptionalArgument String), name_not : (OptionalArgument String), name_in : (OptionalArgument (List String)), name_not_in : (OptionalArgument (List String)), name_lt : (OptionalArgument String), name_lte : (OptionalArgument String), name_gt : (OptionalArgument String), name_gte : (OptionalArgument String), name_contains : (OptionalArgument String), name_not_contains : (OptionalArgument String), name_starts_with : (OptionalArgument String), name_not_starts_with : (OptionalArgument String), name_ends_with : (OptionalArgument String), name_not_ends_with : (OptionalArgument String) }


{-| Type alias for the `UserWhereInput` attributes. Note that this type
needs to use the `UserWhereInput` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias UserWhereInputRaw =
    { and : (OptionalArgument (List UserWhereInput)), or : (OptionalArgument (List UserWhereInput)), not : (OptionalArgument (List UserWhereInput)), id : (OptionalArgument Api.Scalar.Id), id_not : (OptionalArgument Api.Scalar.Id), id_in : (OptionalArgument (List Api.Scalar.Id)), id_not_in : (OptionalArgument (List Api.Scalar.Id)), id_lt : (OptionalArgument Api.Scalar.Id), id_lte : (OptionalArgument Api.Scalar.Id), id_gt : (OptionalArgument Api.Scalar.Id), id_gte : (OptionalArgument Api.Scalar.Id), id_contains : (OptionalArgument Api.Scalar.Id), id_not_contains : (OptionalArgument Api.Scalar.Id), id_starts_with : (OptionalArgument Api.Scalar.Id), id_not_starts_with : (OptionalArgument Api.Scalar.Id), id_ends_with : (OptionalArgument Api.Scalar.Id), id_not_ends_with : (OptionalArgument Api.Scalar.Id), name : (OptionalArgument String), name_not : (OptionalArgument String), name_in : (OptionalArgument (List String)), name_not_in : (OptionalArgument (List String)), name_lt : (OptionalArgument String), name_lte : (OptionalArgument String), name_gt : (OptionalArgument String), name_gte : (OptionalArgument String), name_contains : (OptionalArgument String), name_not_contains : (OptionalArgument String), name_starts_with : (OptionalArgument String), name_not_starts_with : (OptionalArgument String), name_ends_with : (OptionalArgument String), name_not_ends_with : (OptionalArgument String) }


{-| Type for the UserWhereInput input object.
-}
type UserWhereInput
    = UserWhereInput UserWhereInputRaw
    

{-| Encode a UserWhereInput into a value that can be used as an argument.
-}
encodeUserWhereInput : UserWhereInput -> Value
encodeUserWhereInput (UserWhereInput input) =
    Encode.maybeObject
        [ ( "AND", (encodeUserWhereInput |> Encode.list)  |> Encode.optional input.and ), ( "OR", (encodeUserWhereInput |> Encode.list)  |> Encode.optional input.or ), ( "NOT", (encodeUserWhereInput |> Encode.list)  |> Encode.optional input.not ), ( "id", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id ), ( "id_not", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_not ), ( "id_in", ((\(Api.Scalar.Id raw) -> Encode.string raw) |> Encode.list)  |> Encode.optional input.id_in ), ( "id_not_in", ((\(Api.Scalar.Id raw) -> Encode.string raw) |> Encode.list)  |> Encode.optional input.id_not_in ), ( "id_lt", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_lt ), ( "id_lte", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_lte ), ( "id_gt", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_gt ), ( "id_gte", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_gte ), ( "id_contains", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_contains ), ( "id_not_contains", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_not_contains ), ( "id_starts_with", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_starts_with ), ( "id_not_starts_with", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_not_starts_with ), ( "id_ends_with", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_ends_with ), ( "id_not_ends_with", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id_not_ends_with ), ( "name", (Encode.string)  |> Encode.optional input.name ), ( "name_not", (Encode.string)  |> Encode.optional input.name_not ), ( "name_in", (Encode.string |> Encode.list)  |> Encode.optional input.name_in ), ( "name_not_in", (Encode.string |> Encode.list)  |> Encode.optional input.name_not_in ), ( "name_lt", (Encode.string)  |> Encode.optional input.name_lt ), ( "name_lte", (Encode.string)  |> Encode.optional input.name_lte ), ( "name_gt", (Encode.string)  |> Encode.optional input.name_gt ), ( "name_gte", (Encode.string)  |> Encode.optional input.name_gte ), ( "name_contains", (Encode.string)  |> Encode.optional input.name_contains ), ( "name_not_contains", (Encode.string)  |> Encode.optional input.name_not_contains ), ( "name_starts_with", (Encode.string)  |> Encode.optional input.name_starts_with ), ( "name_not_starts_with", (Encode.string)  |> Encode.optional input.name_not_starts_with ), ( "name_ends_with", (Encode.string)  |> Encode.optional input.name_ends_with ), ( "name_not_ends_with", (Encode.string)  |> Encode.optional input.name_not_ends_with ) ]


buildUserWhereUniqueInput : (UserWhereUniqueInputOptionalFields -> UserWhereUniqueInputOptionalFields) -> UserWhereUniqueInput
buildUserWhereUniqueInput fillOptionals =

    let
        optionals =
            
            fillOptionals
                { id = Absent }
    in
    { id = optionals.id }


type alias UserWhereUniqueInputOptionalFields =
    { id : (OptionalArgument Api.Scalar.Id) }


{-| Type for the UserWhereUniqueInput input object.
-}
type alias UserWhereUniqueInput =
    { id : (OptionalArgument Api.Scalar.Id) }
    

{-| Encode a UserWhereUniqueInput into a value that can be used as an argument.
-}
encodeUserWhereUniqueInput : UserWhereUniqueInput -> Value
encodeUserWhereUniqueInput input =
    Encode.maybeObject
        [ ( "id", ((\(Api.Scalar.Id raw) -> Encode.string raw))  |> Encode.optional input.id ) ]