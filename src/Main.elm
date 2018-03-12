port module Main exposing (..)

import Dom
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( style
        , id
        , multiple
        , target
        , type_
        , value
        , href
        , downloadAs
        , hidden
        )
import Html.Attributes
import Html.Styled.Events exposing (onInput, onClick)
import Style
import Markdown
import SelectList exposing (SelectList)
import Keyboard
import Task
import Base64
import Dict exposing (Dict, fromList)
import FileReader exposing (NativeFile)


port scrollToId : String -> Cmd msg


type alias Model =
    { document : SelectList String
    , editMode : Bool
    , showHelp : Bool
    , showExport : Bool
    }


type Msg
    = NoOp
    | InputMD String
    | MoveUp
    | MoveUpVim
    | MoveDown
    | MoveDownVim
    | ToggleEdit
    | ToggleEditVim
    | ExitEdit
    | NewLine
    | DelLine
    | ToggleHelp
    | ToggleExport
    | ImportMD (List NativeFile)
    | OnFileLoaded (Result FileReader.Error String)


model : Model
model =
    { document = SelectList.singleton ""
    , editMode = False
    , showHelp = False
    , showExport = False
    }


init : ( Model, Cmd Msg )
init =
    model ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! [ Cmd.none ]

        InputMD markdown ->
            { model
                | document =
                    SelectList.updateCurrent (\_ -> markdown) model.document
            }
                ! [ Cmd.none ]

        ToggleEdit ->
            { model | editMode = not model.editMode }
                ! [ focusInput ]

        NewLine ->
            newLine model ! [ Cmd.none ]

        DelLine ->
            { model | document = SelectList.removeCurrent model.document }
                ! [ Cmd.none ]

        MoveUp ->
            move -1 model

        MoveDown ->
            move 1 model

        MoveDownVim ->
            if not model.editMode then
                move 1 model
            else
                model ! [ Cmd.none ]

        MoveUpVim ->
            if not model.editMode then
                move -1 model
            else
                model ! [ Cmd.none ]

        ToggleEditVim ->
            if not model.editMode then
                { model | editMode = True }
                    ! [ focusInput ]
            else
                model ! [ Cmd.none ]

        ExitEdit ->
            { model | editMode = False } ! [ Cmd.none ]

        ToggleHelp ->
            { model | showHelp = not model.showHelp } ! [ Cmd.none ]

        ToggleExport ->
            { model | showExport = not model.showExport } ! [ Cmd.none ]

        ImportMD files ->
            case files of
                [ a ] ->
                    model ! [ getFileContents a ]

                _ ->
                    model ! [ Cmd.none ]

        OnFileLoaded result ->
            case result of
                Ok content ->
                    let
                        newdoc =
                            (String.split "\n" >> SelectList.fromList) content
                    in
                        case newdoc of
                            Just document ->
                                { model | document = document, showExport = False }
                                    ! [ Cmd.none ]

                            Nothing ->
                                model ! [ Cmd.none ]

                Err err ->
                    model ! [ Cmd.none ]


getFileContents : NativeFile -> Cmd Msg
getFileContents file =
    FileReader.readAsTextFile file.blob
        |> Task.attempt OnFileLoaded


focusInput : Cmd Msg
focusInput =
    Dom.focus "lineinput"
        |> Task.attempt (\_ -> NoOp)


move : Int -> Model -> ( Model, Cmd Msg )
move amount model =
    { model
        | document = SelectList.jump amount model.document
    }
        ! [ scrollToId "pointer" ]


newLine : Model -> Model
newLine model =
    if model.editMode then
        { model | document = SelectList.append "" model.document |> SelectList.next }
    else
        model


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


view : Model -> Html Msg
view model =
    div
        []
        [ viewHeader
        , viewExportModal model.showExport model.document
        , viewHelpModal model.showHelp
        , viewTextArea model
        ]


viewHeader : Html Msg
viewHeader =
    div [ Style.header ]
        [ span [ Style.headerTitle ] [ text "Markdown Editor 0.1" ]
        , a [ Style.button, onClick ToggleEdit ] [ text "Edit Mode" ]
        , a [ Style.button, onClick ToggleExport ] [ text "Import/Export" ]
        , a [ Style.button, onClick ToggleHelp ] [ text "Help" ]
        ]


viewHelpModal : Bool -> Html Msg
viewHelpModal show =
    let
        keys =
            [ ( "j", "Move down" )
            , ( "k", "Move up" )
            , ( "i", "Switch to edit mode" )
            , ( "Esc", "Exit edit mode" )
            ]
    in
        viewModal "Help" show ToggleHelp <|
            table [] <|
                List.map
                    (\( key, command ) ->
                        tr []
                            [ td [] [ text key ]
                            , td [] [ text command ]
                            ]
                    )
                    keys


onFileChange : (List NativeFile -> Msg) -> Html.Styled.Attribute Msg
onFileChange msg =
    Html.Styled.Attributes.fromUnstyled (FileReader.onFileChange msg)


viewExportModal : Bool -> SelectList String -> Html Msg
viewExportModal show document =
    viewModal "Import/Export" show ToggleExport <|
        div []
            [ label
                [ Style.button ]
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
                , href ((serialize >> mkDataURI) document)
                , downloadAs "export.md"
                , onClick ToggleExport
                ]
                [ text "Export" ]
            ]


viewModal : String -> Bool -> Msg -> Html Msg -> Html Msg
viewModal title show closeMsg content =
    if show then
        div [ Style.overlay ]
            [ div [ Style.modal ]
                [ div [ Style.modalTitleBar ]
                    [ span [ Style.modalCloseBtn, onClick closeMsg ] [ text "x" ]
                    , span [ Style.modalTitle ] [ text title ]
                    ]
                , div [ Style.modalContent ] [ content ]
                ]
            ]
    else
        text ""


viewTextArea : Model -> Html Msg
viewTextArea model =
    div []
        [ div
            [ Style.line ]
            [ viewPointer False
            , SelectList.preceding model.document
                |> String.join "\n"
                |> toHtml
            ]
        , div
            [ Style.line ]
            [ viewPointer True
            , let
                current =
                    SelectList.current model.document
              in
                if model.editMode then
                    input
                        [ id "lineinput", value current, onInput InputMD ]
                        []
                else
                    toHtml current
            ]
        , div
            [ Style.line ]
            [ viewPointer False
            , SelectList.following model.document
                |> String.join "\n"
                |> toHtml
            ]
        ]


viewPointer : Bool -> Html Msg
viewPointer isSelected =
    div
        ([ Style.pointer ]
            ++ if isSelected then
                [ id "pointer", Style.selected ]
               else
                []
        )
        [ text " " ]


toHtml : String -> Html Msg
toHtml markdown =
    Markdown.toHtml
        [ Html.Attributes.style
            [ ( "display", "inline" )
            , ( "width", "100%" )
            ]
        ]
        markdown
        |> fromUnstyled


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        hotkeys =
            Dict.fromList
                [ ( 13, NewLine )
                , ( 38, MoveUp )
                , ( 40, MoveDown )
                , ( 74, MoveDownVim )
                , ( 75, MoveUpVim )
                , ( 73, ToggleEditVim )
                , ( 27, ExitEdit )
                ]
    in
        Keyboard.downs
            (\key ->
                case Dict.get key hotkeys of
                    Nothing ->
                        NoOp

                    Just msg ->
                        msg
            )


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
