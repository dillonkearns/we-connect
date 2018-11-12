module View.Navbar exposing (view)

import Element
import Element.Background
import Element.Font


titleColor =
    -- Element.rgb255 17 138 178
    Element.rgba255 0 200 200 1


view username =
    [ Element.row [ Element.spacing 25 ]
        [ Element.text "WeConnect"
        , Element.text "Just Show Up"
            |> Element.el
                [ Element.Font.size 18
                , Element.Font.color (Element.rgba255 255 255 255 0.85)
                ]
        ]
    , Element.text username
    ]
        |> Element.row
            [ Element.Font.color (Element.rgba255 255 255 255 1.0)
            , Element.Font.bold
            , Element.Font.size 30
            , Element.Background.color (Element.rgba255 20 20 20 1.0)
            , Element.padding 25
            , Element.width Element.fill
            , Element.spaceEvenly
            ]



-- rgb(17, 138, 178)
