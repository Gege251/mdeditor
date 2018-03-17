port module Main exposing (..)

import Dom
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Attributes
import Html.Styled.Events exposing (..)
import Maybe.Extra
import Style
import Markdown
import SelectList exposing (SelectList)
import Keyboard exposing (KeyCode)
import Process
import Task
import Base64
import Dict exposing (Dict, fromList)
import FileReader exposing (NativeFile)
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as Pipeline


port scrollToId : String -> Cmd msg


port localStorageSet : LocalStorageItem -> Cmd msg


port localStorageGet : String -> Cmd msg


port localStorageRemove : String -> Cmd msg


port onLocalStorageResponse : (Value -> msg) -> Sub msg


type ViewMode
    = Show ShowMode
    | Edit


type ShowMode
    = Markdown
    | Compiled


type alias Model =
    { document : SelectList String
    , editMode : Bool
    , viewMode : ViewMode
    , showHelp : Bool
    , showExport : Bool
    , showAbout : Bool
    , notifications : List Notification
    , lastKey : Maybe KeyCode
    , clipboard : String
    }


type Msg
    = NoOp
    | InputMD String
    | MoveUp
    | MoveDown
    | NewDocument
    | ToggleEdit
    | ToggleView
    | ShowMarkdown
    | ShowCompiled
    | NewLine
    | ToggleHelp
    | ToggleExport
    | ToggleAbout
    | NotificationFade Notification
    | ImportMD (List NativeFile)
    | OnFileLoaded (Result FileReader.Error String)
    | KeyDown KeyCode
    | SaveToLocalStorage
    | OnLocalStorageResponse LocalStorageItem


type Notification
    = Saved
    | Imported


type alias LocalStorageItem =
    { key : String
    , value : Maybe String
    }


model : Model
model =
    { document = SelectList.singleton ""
    , editMode = False
    , viewMode = Show Compiled
    , showHelp = False
    , showExport = False
    , showAbout = False
    , notifications = []
    , lastKey = Nothing
    , clipboard = ""
    }


init : ( Model, Cmd Msg )
init =
    model ! [ localStorageGet "document" ]


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

        ToggleView ->
            let
                newMode =
                    case model.viewMode of
                        Show _ ->
                            Edit

                        Edit ->
                            Show Compiled
            in
                { model | viewMode = newMode }
                    ! [ Cmd.none ]

        ShowMarkdown ->
            case model.viewMode of
                Show _ ->
                    { model | viewMode = Show Markdown }
                        ! [ Cmd.none ]

                _ ->
                    model ! [ Cmd.none ]

        ShowCompiled ->
            case model.viewMode of
                Show _ ->
                    { model | viewMode = Show Compiled }
                        ! [ Cmd.none ]

                _ ->
                    model ! [ Cmd.none ]

        NewLine ->
            newLine model ! [ Cmd.none ]

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
                    { model | notifications = Imported :: model.notifications }
                        ! [ Cmd.batch
                                [ getFileContents a
                                , notificationTimer Imported
                                ]
                          ]

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

        SaveToLocalStorage ->
            { model | notifications = Saved :: model.notifications }
                ! [ Cmd.batch
                        [ localStorageSet <|
                            LocalStorageItem "document" <|
                                (Just << serialize) model.document
                        , notificationTimer Saved
                        ]
                  ]

        NotificationFade notification ->
            let
                filtered =
                    List.filter (\n -> n /= notification) model.notifications
            in
                { model | notifications = filtered } ! [ Cmd.none ]

        OnLocalStorageResponse { key, value } ->
            case key of
                "document" ->
                    case value |> Maybe.andThen deserialize of
                        Just document ->
                            { model | document = document } ! [ Cmd.none ]

                        Nothing ->
                            model ! [ Cmd.none ]

                otherwise ->
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
                                , clipboard = SelectList.current model.document
                            }
                                ! [ Cmd.none ]
                        else
                            { modelLastKeyReset | lastKey = Just 68 } ! [ Cmd.none ]

                    -- y (yank)
                    89 ->
                        if not model.editMode then
                            { modelLastKeyReset | clipboard = SelectList.current model.document }
                                ! [ Cmd.none ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- i (edit mode on)
                    73 ->
                        if not model.editMode then
                            { modelLastKeyReset | editMode = True, viewMode = Edit }
                                ! [ focusInput ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- o (edit new line)
                    79 ->
                        if not model.editMode then
                            newLine { modelLastKeyReset | editMode = True } ! [ focusInput ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- p (paste)
                    80 ->
                        if not model.editMode then
                            { model
                                | document =
                                    model.document
                                        |> SelectList.append model.clipboard
                                        |> SelectList.next
                            }
                                ! [ Cmd.none ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- Esc (edit mode off)
                    27 ->
                        if model.lastKey == Just 27 then
                            { modelLastKeyReset | viewMode = Show Compiled } ! [ Cmd.none ]
                        else
                            { modelLastKeyReset
                                | lastKey = Just 27
                                , editMode = False
                            }
                                ! [ Cmd.none ]

                    _ ->
                        modelLastKeyReset ! [ Cmd.none ]


getFileContents : NativeFile -> Cmd Msg
getFileContents file =
    FileReader.readAsTextFile file.blob
        |> Task.attempt OnFileLoaded


notificationTimer : Notification -> Cmd Msg
notificationTimer notification =
    Process.sleep 1000
        |> Task.perform (\_ -> NotificationFade notification)


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
        [ viewHeader model
        , viewExportModal model.showExport model.document
        , viewHelpModal model.showHelp
        , viewAboutModal model.showAbout
        , viewTextArea model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Style.header ]
        (Maybe.Extra.values
            [ Just <| span [ Style.headerTitle, onClick ToggleAbout ] [ text appName ]
            , Just <| button [ Style.button, onClick NewDocument ] [ text "New Document" ]
            , Just <| button [ Style.button, onClick SaveToLocalStorage ] [ text "Save in browser" ]
            , Just <| button [ Style.button, onClick ToggleExport ] [ text "Import/Export" ]
            , Just <|
                button
                    (Maybe.Extra.values
                        [ Just <| Style.button
                        , Just <| onClick ToggleView
                        , ifShowMode model <| Style.highlight
                        ]
                    )
                    [ text "Toggle Mode" ]
            , ifShowMode model <|
                button
                    (Maybe.Extra.values
                        [ Just <| Style.button
                        , Just <| onClick ShowCompiled
                        , if model.viewMode == Show Compiled then
                            Just <| Style.highlight
                          else
                            Nothing
                        ]
                    )
                    [ text "Compiled" ]
            , ifShowMode model <|
                button
                    (Maybe.Extra.values
                        [ Just <| Style.button
                        , Just <| onClick ShowMarkdown
                        , if model.viewMode == Show Markdown then
                            Just <| Style.highlight
                          else
                            Nothing
                        ]
                    )
                    [ text "Raw" ]
            , Just <| button [ Style.button, onClick ToggleHelp ] [ text "Help" ]
            , Just <| span [ Style.headerTitle ] <| List.map viewNotification model.notifications
            ]
        )


ifShowMode : { r | viewMode : ViewMode } -> a -> Maybe a
ifShowMode { viewMode } value =
    case viewMode of
        Show _ ->
            Just value

        otherwise ->
            Nothing


viewNotification : Notification -> Html Msg
viewNotification notification =
    case notification of
        Saved ->
            text "Saved."

        Imported ->
            text "Imported."


viewHelpModal : Bool -> Html Msg
viewHelpModal show =
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
    case model.viewMode of
        Show Compiled ->
            viewCompiledMode model

        Show Markdown ->
            viewMarkdownMode model

        Edit ->
            viewEditMode model


viewCompiledMode : Model -> Html Msg
viewCompiledMode model =
    div
        [ Style.line ]
        [ viewPointer False
        , SelectList.toList model.document
            |> String.join "\n"
            |> toHtml
        ]


viewMarkdownMode : Model -> Html Msg
viewMarkdownMode model =
    div
        [ Style.line ]
        [ viewPointer False
        , pre []
            [ SelectList.toList model.document
                |> String.join "\n"
                |> text
            ]
        ]


viewEditMode : Model -> Html Msg
viewEditMode model =
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
                    text current
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
        (List.concat
            [ [ Style.pointer ]
            , if isSelected then
                [ id "pointer", Style.selected ]
              else
                [ Style.deselected ]
            ]
        )
        []


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
    Sub.batch
        [ Keyboard.downs KeyDown
        , onLocalStorageResponse decodeLocalStorage
        ]


decodeLocalStorage : Value -> Msg
decodeLocalStorage val =
    let
        decoder =
            Pipeline.decode LocalStorageItem
                |> Pipeline.required "key" Decode.string
                |> Pipeline.required "value" (Decode.nullable Decode.string)
    in
        case Decode.decodeValue decoder val of
            Ok storage ->
                OnLocalStorageResponse storage

            otherwise ->
                NoOp


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
