-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Enum.TimeSlotOrderByInput exposing (..)

import Json.Decode as Decode exposing (Decoder)


type TimeSlotOrderByInput
    = Id_ASC
    | Id_DESC
    | Time_ASC
    | Time_DESC
    | UpdatedAt_ASC
    | UpdatedAt_DESC
    | CreatedAt_ASC
    | CreatedAt_DESC
list : List TimeSlotOrderByInput
list =
    [Id_ASC, Id_DESC, Time_ASC, Time_DESC, UpdatedAt_ASC, UpdatedAt_DESC, CreatedAt_ASC, CreatedAt_DESC]
decoder : Decoder TimeSlotOrderByInput
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id_ASC" ->
                        Decode.succeed Id_ASC

                    "id_DESC" ->
                        Decode.succeed Id_DESC

                    "time_ASC" ->
                        Decode.succeed Time_ASC

                    "time_DESC" ->
                        Decode.succeed Time_DESC

                    "updatedAt_ASC" ->
                        Decode.succeed UpdatedAt_ASC

                    "updatedAt_DESC" ->
                        Decode.succeed UpdatedAt_DESC

                    "createdAt_ASC" ->
                        Decode.succeed CreatedAt_ASC

                    "createdAt_DESC" ->
                        Decode.succeed CreatedAt_DESC

                    _ ->
                        Decode.fail ("Invalid TimeSlotOrderByInput type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : TimeSlotOrderByInput -> String
toString enum =
    case enum of
        Id_ASC ->
                "id_ASC"


        Id_DESC ->
                "id_DESC"


        Time_ASC ->
                "time_ASC"


        Time_DESC ->
                "time_DESC"


        UpdatedAt_ASC ->
                "updatedAt_ASC"


        UpdatedAt_DESC ->
                "updatedAt_DESC"


        CreatedAt_ASC ->
                "createdAt_ASC"


        CreatedAt_DESC ->
                "createdAt_DESC"
