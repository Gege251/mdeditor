module Style exposing (..)

import Css exposing (..)
import Html.Styled.Attributes exposing (css)
import Css.Colors.Mixers as Mixers


palette =
    { c1 = hex "343635"
    , c2 = hex "2e4874"
    , c3 = hex "7eaba4"
    , c4 = hex "f4eade"
    , c5 = hex "347c83"
    }


selected =
    css [ backgroundColor palette.c5 ]


line =
    css
        [ displayFlex
        , minHeight (Css.rem 1)
        ]


input =
    css
        [ border (px 0) ]


pointer =
    css
        [ width (Css.rem 1), minHeight (Css.rem 1) ]


button =
    css
        [ border (px 0)
        , backgroundColor palette.c3
        , color palette.c4
        , display inlineBlock
        , textAlign center
        , textDecoration none
        , fontSize (em 0.8)
        , cursor default
        , padding (em 0.5)
        , hover
            [ backgroundColor (Mixers.darken 10 palette.c3) ]
        ]


header =
    css
        [ backgroundColor palette.c4 ]


headerTitle =
    css
        [ fontSize (em 0.7)
        , padding2 (px 0) (em 1.5)
        ]


overlay =
    css
        [ position fixed
        , top (px 0)
        , left (px 0)
        , width (pct 100)
        , height (pct 100)
        , backgroundColor (rgba 0 0 0 0.5)
        , zIndex (int 2)
        ]


modal =
    css
        [ position fixed
        , minWidth (em 20)
        , minHeight (em 10)
        , top (pct 50)
        , left (pct 50)
        , transform (translate2 (pct -50) (pct -50))
        , backgroundColor (hex "fff")
        , color (hex "000")
        , borderRadius (px 5)
        , boxShadow4 (px 0) (px 0) (px 50) (rgba 0 0 0 0.5)
        ]


modalCloseBtn =
    css
        [ textShadow4 (px 1) (px 1) (px 2) (hex "000")
        , cursor default
        , padding (em 0.7)
        , hover
            [ textShadow4 (px 0) (px 0) (px 1) (hex "000")
            ]
        ]


modalTitle =
    css
        [ fontWeight bold ]


modalTitleBar =
    css
        [ backgroundColor (hex "eee")
        , borderRadius4 (px 5) (px 5) (px 0) (px 0)
        , padding (px 5)
        ]


modalContent =
    css [ padding (px 10) ]



-- [ position fixed
-- , top (px 10)
-- , left (px 0)
-- ]
