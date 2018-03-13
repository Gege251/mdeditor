port module Main exposing (..)

import Dom
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Attributes
import Html.Styled.Events exposing (..)
import Style
import Markdown
import SelectList exposing (SelectList)
import Keyboard exposing (KeyCode)
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
    , showAbout : Bool
    , lastKey : Maybe KeyCode
    }


type Msg
    = NoOp
    | InputMD String
    | MoveUp
    | MoveDown
    | NewDocument
    | ToggleEdit
    | NewLine
    | DelLine
    | ToggleHelp
    | ToggleExport
    | ToggleAbout
    | ImportMD (List NativeFile)
    | OnFileLoaded (Result FileReader.Error String)
    | KeyDown KeyCode


model : Model
model =
    { document = SelectList.singleton ""
    , editMode = False
    , showHelp = False
    , showExport = False
    , showAbout = False
    , lastKey = Nothing
    }


init : ( Model, Cmd Msg )
init =
    model ! [ Cmd.none ]


appName : String
appName =
    "Markdown Editor 0.0.1"


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

        NewDocument ->
            { model | document = SelectList.singleton "" } ! [ Cmd.none ]

        ToggleHelp ->
            { model | showHelp = not model.showHelp } ! [ Cmd.none ]

        ToggleExport ->
            { model | showExport = not model.showExport } ! [ Cmd.none ]

        ToggleAbout ->
            { model | showAbout = not model.showAbout } ! [ Cmd.none ]

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

        KeyDown keyCode ->
            let
                modelLastKeyReset =
                    { model | lastKey = Nothing }
            in
                case keyCode of
                    -- enter
                    13 ->
                        newLine modelLastKeyReset ! [ Cmd.none ]

                    -- up arrow
                    38 ->
                        move -1 modelLastKeyReset

                    -- down arrow
                    40 ->
                        move 1 modelLastKeyReset

                    -- j (move down)
                    74 ->
                        if not model.editMode then
                            move 1 modelLastKeyReset
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- k (move up)
                    75 ->
                        if not model.editMode then
                            move -1 modelLastKeyReset
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- d (delete line)
                    68 ->
                        if model.lastKey == Just 68 then
                            { modelLastKeyReset
                                | document =
                                    SelectList.removeCurrent model.document
                            }
                                ! [ Cmd.none ]
                        else
                            { modelLastKeyReset | lastKey = Just 68 } ! [ Cmd.none ]

                    -- i (edit mode on)
                    73 ->
                        if not model.editMode then
                            { modelLastKeyReset | editMode = True }
                                ! [ focusInput ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- Esc (edit mode off)
                    27 ->
                        { modelLastKeyReset | editMode = False } ! [ Cmd.none ]

                    _ ->
                        modelLastKeyReset ! [ Cmd.none ]


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
        , viewAboutModal model.showAbout
        , viewTextArea model
        ]


viewHeader : Html Msg
viewHeader =
    div [ Style.header ]
        [ span [ Style.headerTitle, onClick ToggleAbout ] [ text appName ]
        , a [ Style.button, onClick NewDocument ] [ text "New Document" ]
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
            , ( "d (twice)", "Delete line" )
            , ( "i", "Switch to edit mode" )
            , ( "Esc", "Exit edit mode" )
            ]

        content =
            table [] <|
                List.map
                    (\( key, command ) ->
                        tr []
                            [ td [] [ text key ]
                            , td [] [ text command ]
                            ]
                    )
                    keys

        footer =
            text ""
    in
        viewModal "Help" show ToggleHelp content footer


viewAboutModal : Bool -> Html Msg
viewAboutModal show =
    let
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
        viewModal appName show ToggleAbout content footer


onFileChange : (List NativeFile -> Msg) -> Html.Styled.Attribute Msg
onFileChange msg =
    Html.Styled.Attributes.fromUnstyled (FileReader.onFileChange msg)


viewExportModal : Bool -> SelectList String -> Html Msg
viewExportModal show document =
    let
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
                    , href ((serialize >> mkDataURI) document)
                    , downloadAs "export.md"
                    , onClick ToggleExport
                    ]
                    [ text "Export" ]
                ]
    in
        viewModal "Import/Export" show ToggleExport content footer


viewModal : String -> Bool -> Msg -> Html Msg -> Html Msg -> Html Msg
viewModal title show closeMsg content footer =
    if show then
        div [ Style.overlay ]
            [ div [ Style.modal ]
                [ div [ Style.modalTitleBar ]
                    [ span [ Style.modalCloseBtn, onClick closeMsg ] [ text "x" ]
                    , span [ Style.modalTitle ] [ text title ]
                    ]
                , div [ Style.modalContent ] [ content ]
                , div [ Style.modalFooter ] [ footer ]
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
                        [ Style.inputField
                        , id "lineinput"
                        , value current
                        , onInput InputMD
                        ]
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
                [ Style.deselected ]
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
        |> Html.Styled.fromUnstyled


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
