module View.Navbar exposing (view)

import Element
import Element.Background
import Element.Font


titleColor =
    -- Element.rgb255 17 138 178
    Element.rgba255 0 200 200 1


view =
    Element.text "WeConnect"
        |> Element.el
            [ Element.Font.color titleColor
            , Element.Font.bold
            , Element.Font.size 30
            ]



-- rgb(17, 138, 178)
