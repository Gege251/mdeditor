module Modals exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Style
import Model exposing (..)
import Utils exposing (..)


modalContents : Modal -> Model -> ( String, Html Msg, Html Msg )
modalContents modal model =
    case modal of
        NoModal ->
            ( "", text "", text "" )

        About ->
            let
                title =
                    appName

                content =
                    div []
                        [ h2 [] [ text appName ]
                        , div [] [ text "by Szabo Gergely" ]
                        , a [ href "https://github.com/gege251/mdeditor", target "_blank" ]
                            [ text "https://github.com/gege251/mdeditor" ]
                        ]

                footer =
                    text ""
            in
                ( title, content, footer )

        Help ->
            let
                keys =
                    [ ( "j", "Move down" )
                    , ( "k", "Move up" )
                    , ( "d 2x", "Delete line" )
                    , ( "p", "Paste line" )
                    , ( "o", "New line" )
                    , ( "i", "Switch to edit mode" )
                    , ( "Esc", "Exit edit mode" )
                    , ( "Esc 2x", "Switch to view mode" )
                    ]

                title =
                    "Help"

                content =
                    div []
                        [ text "I tried to emulate Vim behaviour so these hotkeys might seem familiar."
                        , hr [] []
                        , table
                            []
                          <|
                            List.map
                                (\( key, command ) ->
                                    tr []
                                        [ td [ Style.divide2 ] [ text key ]
                                        , td [ Style.divide2 ] [ text command ]
                                        ]
                                )
                                keys
                        ]

                footer =
                    text ""
            in
                ( title, content, footer )

        ImportExport ->
            let
                title =
                    "Import/Export"

                content =
                    div [ Style.widthPx 300, Style.centered ]
                        [ text "Warning! If you import a new file, all your changes will be lost!"
                        ]

                footer =
                    div []
                        [ label
                            [ Style.button, Style.divide2 ]
                            [ input
                                [ type_ "file"
                                , hidden True
                                , onFileChange ImportMD
                                , multiple False
                                ]
                                []
                            , text "Import"
                            ]
                        , a
                            [ Style.button
                            , Style.divide2
                            , href ((serialize >> mkDataURI) model.document)
                            , downloadAs "export.md"
                            , onClick (ToggleModal ImportExport)
                            ]
                            [ text "Export" ]
                        ]
            in
                ( title, content, footer )


viewModal : Modal -> Model -> Html Msg
viewModal modal model =
    if modal == NoModal then
        text ""
    else
        let
            ( title, content, footer ) =
                modalContents modal model
        in
            div [ Style.overlay ]
                [ div [ Style.modal ]
                    [ div [ Style.modalTitleBar ]
                        [ span [ Style.modalCloseBtn, onClick (ToggleModal modal) ] [ text "x" ]
                        , span [ Style.modalTitle ] [ text title ]
                        ]
                    , div [ Style.modalContent ] [ content ]
                    , div [ Style.modalFooter ] [ footer ]
                    ]
                ]
