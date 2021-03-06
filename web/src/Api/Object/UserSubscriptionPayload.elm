-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.UserSubscriptionPayload exposing (..)

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
import Api.Enum.MutationType


{-| Select fields to build up a SelectionSet for this object.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.UserSubscriptionPayload
selection constructor =
    Object.selection constructor
mutation : Field Api.Enum.MutationType.MutationType Api.Object.UserSubscriptionPayload
mutation =
      Object.fieldDecoder "mutation" [] (Api.Enum.MutationType.decoder)


node : SelectionSet decodesTo Api.Object.User -> Field (Maybe decodesTo) Api.Object.UserSubscriptionPayload
node object_ =
      Object.selectionField "node" [] (object_) (identity >> Decode.nullable)


updatedFields : Field (Maybe (List String)) Api.Object.UserSubscriptionPayload
updatedFields =
      Object.fieldDecoder "updatedFields" [] (Decode.string |> Decode.list |> Decode.nullable)


previousValues : SelectionSet decodesTo Api.Object.UserPreviousValues -> Field (Maybe decodesTo) Api.Object.UserSubscriptionPayload
previousValues object_ =
      Object.selectionField "previousValues" [] (object_) (identity >> Decode.nullable)
