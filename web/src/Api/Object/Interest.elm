-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.Interest exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Field as Field exposing (Field)
import Graphql.Internal.Builder.Object as Object
import Graphql.SelectionSet exposing (SelectionSet)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Api.Object
import Api.Interface
import Api.Union
import Api.Scalar
import Api.InputObject
import Json.Decode as Decode
import Graphql.Internal.Encode as Encode exposing (Value)
import Api.Enum.UserOrderByInput


{-| Select fields to build up a SelectionSet for this object.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.Interest
selection constructor =
    Object.selection constructor
id : Field Api.Scalar.Id Api.Object.Interest
id =
      Object.fieldDecoder "id" [] (Object.scalarDecoder |> Decode.map Api.Scalar.Id)


name : Field String Api.Object.Interest
name =
      Object.fieldDecoder "name" [] (Decode.string)


type alias InterestedUsersOptionalArguments = { where_ : OptionalArgument Api.InputObject.UserWhereInput, orderBy : OptionalArgument Api.Enum.UserOrderByInput.UserOrderByInput, skip : OptionalArgument Int, after : OptionalArgument String, before : OptionalArgument String, first : OptionalArgument Int, last : OptionalArgument Int }

{-|

  - where_ - 

-}
interestedUsers : (InterestedUsersOptionalArguments -> InterestedUsersOptionalArguments) -> SelectionSet decodesTo Api.Object.User -> Field (Maybe (List decodesTo)) Api.Object.Interest
interestedUsers fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { where_ = Absent, orderBy = Absent, skip = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "where" filledInOptionals.where_ (Api.InputObject.encodeUserWhereInput), Argument.optional "orderBy" filledInOptionals.orderBy ((Encode.enum Api.Enum.UserOrderByInput.toString)), Argument.optional "skip" filledInOptionals.skip (Encode.int), Argument.optional "after" filledInOptionals.after (Encode.string), Argument.optional "before" filledInOptionals.before (Encode.string), Argument.optional "first" filledInOptionals.first (Encode.int), Argument.optional "last" filledInOptionals.last (Encode.int) ]
                |> List.filterMap identity
    in
      Object.selectionField "interestedUsers" optionalArgs (object_) (identity >> Decode.list >> Decode.nullable)
