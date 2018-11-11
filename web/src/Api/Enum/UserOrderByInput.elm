-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Enum.UserOrderByInput exposing (..)

import Json.Decode as Decode exposing (Decoder)


type UserOrderByInput
    = Id_ASC
    | Id_DESC
    | Name_ASC
    | Name_DESC
    | UpdatedAt_ASC
    | UpdatedAt_DESC
    | CreatedAt_ASC
    | CreatedAt_DESC
list : List UserOrderByInput
list =
    [Id_ASC, Id_DESC, Name_ASC, Name_DESC, UpdatedAt_ASC, UpdatedAt_DESC, CreatedAt_ASC, CreatedAt_DESC]
decoder : Decoder UserOrderByInput
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id_ASC" ->
                        Decode.succeed Id_ASC

                    "id_DESC" ->
                        Decode.succeed Id_DESC

                    "name_ASC" ->
                        Decode.succeed Name_ASC

                    "name_DESC" ->
                        Decode.succeed Name_DESC

                    "updatedAt_ASC" ->
                        Decode.succeed UpdatedAt_ASC

                    "updatedAt_DESC" ->
                        Decode.succeed UpdatedAt_DESC

                    "createdAt_ASC" ->
                        Decode.succeed CreatedAt_ASC

                    "createdAt_DESC" ->
                        Decode.succeed CreatedAt_DESC

                    _ ->
                        Decode.fail ("Invalid UserOrderByInput type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
        )
        

{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : UserOrderByInput -> String
toString enum =
    case enum of
        Id_ASC ->
                "id_ASC"


        Id_DESC ->
                "id_DESC"


        Name_ASC ->
                "name_ASC"


        Name_DESC ->
                "name_DESC"


        UpdatedAt_ASC ->
                "updatedAt_ASC"


        UpdatedAt_DESC ->
                "updatedAt_DESC"


        CreatedAt_ASC ->
                "createdAt_ASC"


        CreatedAt_DESC ->
                "createdAt_DESC"
