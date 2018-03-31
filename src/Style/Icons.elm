module Style.Icons exposing (..)

import Html.Styled exposing (i)
import Html.Styled.Attributes exposing (class, css)
import Css exposing (..)


type Pack
    = Solid
    | Brands


icon pack name =
    let
        packname =
            case pack of
                Solid ->
                    "fas"

                Brands ->
                    "fab"
    in
        i
            [ class (packname ++ " fa-" ++ name)
            , css [ margin2 (px 0) (Css.rem 0.3) ]
            ]
            []


file =
    icon Solid "file"


save =
    icon Solid "save"


questionCircle =
    icon Solid "question-circle"


toggleOn =
    icon Solid "toggle-on"


toggleOff =
    icon Solid "toggle-off"


toggle cond =
    icon Solid <|
        if cond then
            "toggle-on"
        else
            "toggle-off"


pencilAlt =
    icon Solid "pencil-alt"


upload =
    icon Solid "upload"


download =
    icon Solid "download"


github =
    icon Brands "github"


github2x =
    icon Brands "github fa-lg"
