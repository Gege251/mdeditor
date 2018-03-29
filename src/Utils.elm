module Utils exposing (..)

import Html.Styled
import Html.Styled.Attributes
import Base64
import SelectList exposing (SelectList)
import FileReader exposing (NativeFile)


serialize : SelectList String -> String
serialize document =
    SelectList.toList document
        |> String.join "\n"


deserialize : String -> Maybe (SelectList String)
deserialize text =
    String.split "\n" text
        |> SelectList.fromList


mkDataURI : String -> String
mkDataURI raw =
    let
        prefix =
            "data:text/plain;base64,"

        encoded =
            Base64.encode raw
    in
        prefix ++ encoded


onFileChange : (List NativeFile -> msg) -> Html.Styled.Attribute msg
onFileChange msg =
    Html.Styled.Attributes.fromUnstyled (FileReader.onFileChange msg)
