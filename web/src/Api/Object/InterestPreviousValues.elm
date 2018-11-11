-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.InterestPreviousValues exposing (..)

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
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.InterestPreviousValues
selection constructor =
    Object.selection constructor
id : Field Api.Scalar.Id Api.Object.InterestPreviousValues
id =
      Object.fieldDecoder "id" [] (Object.scalarDecoder |> Decode.map Api.Scalar.Id)


name : Field String Api.Object.InterestPreviousValues
name =
      Object.fieldDecoder "name" [] (Decode.string)
