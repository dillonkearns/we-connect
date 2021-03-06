-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.TimeSlotEdge exposing (..)

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



{-| Select fields to build up a SelectionSet for this object.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.TimeSlotEdge
selection constructor =
    Object.selection constructor
{-| The item at the end of the edge.
-}
node : SelectionSet decodesTo Api.Object.TimeSlot -> Field decodesTo Api.Object.TimeSlotEdge
node object_ =
      Object.selectionField "node" [] (object_) (identity)


{-| A cursor for use in pagination.
-}
cursor : Field String Api.Object.TimeSlotEdge
cursor =
      Object.fieldDecoder "cursor" [] (Decode.string)
