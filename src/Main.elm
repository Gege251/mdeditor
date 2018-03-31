port module Main exposing (..)

import Dom
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Attributes
import Html.Styled.Events exposing (..)
import Maybe.Extra
import Style
import Style.Icons as Icons
import Markdown
import Keyboard exposing (KeyCode)
import SelectList exposing (SelectList)
import Process
import Task
import FileReader exposing (NativeFile)
import Json.Decode as Decode exposing (Value)
import Json.Decode.Pipeline as Pipeline
import Utils exposing (..)
import Modals exposing (..)
import Model exposing (..)


port scrollToId : String -> Cmd msg


port localStorageSet : LocalStorageItem -> Cmd msg


port localStorageGet : String -> Cmd msg


port localStorageRemove : String -> Cmd msg


port onLocalStorageResponse : (Value -> msg) -> Sub msg


initModel : Model
initModel =
    { document = SelectList.singleton ""
    , insertMode = False
    , editMode = False
    , compiledView = True
    , visibleModal = NoModal
    , notifications = []
    , lastKey = Nothing
    , clipboard = ""
    }


init : ( Model, Cmd Msg )
init =
    initModel ! [ localStorageGet "document" ]


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

        InputRaw raw ->
            { model
                | document =
                    Maybe.withDefault (SelectList.singleton "") (deserialize raw)
            }
                ! [ Cmd.none ]

        ToggleInsert ->
            { model | insertMode = not model.insertMode }
                ! [ focusInput ]

        ToggleEdit ->
            { model | editMode = not model.editMode }
                ! [ Cmd.none ]

        ToggleView ->
            { model | compiledView = not model.compiledView }
                ! [ Cmd.none ]

        NewLine ->
            newLine model ! [ Cmd.none ]

        MoveUp ->
            move -1 model

        MoveDown ->
            move 1 model

        NewDocument ->
            { model | document = SelectList.singleton "" } ! [ Cmd.none ]

        ToggleModal modal ->
            if model.visibleModal == modal then
                { model | visibleModal = NoModal } ! [ Cmd.none ]
            else
                { model | visibleModal = modal } ! [ Cmd.none ]

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
                                { model | document = document, visibleModal = NoModal }
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
                        if not model.insertMode then
                            move 1 modelLastKeyReset
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- k (move up)
                    75 ->
                        if not model.insertMode then
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
                        if not model.insertMode then
                            { modelLastKeyReset | clipboard = SelectList.current model.document }
                                ! [ Cmd.none ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- i (edit mode on)
                    73 ->
                        if not model.insertMode then
                            { modelLastKeyReset | insertMode = True, editMode = True }
                                ! [ focusInput ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- o (edit new line)
                    79 ->
                        if not model.insertMode then
                            newLine { modelLastKeyReset | insertMode = True } ! [ focusInput ]
                        else
                            modelLastKeyReset ! [ Cmd.none ]

                    -- p (paste)
                    80 ->
                        if not model.insertMode then
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
                            { modelLastKeyReset | editMode = False } ! [ Cmd.none ]
                        else
                            { modelLastKeyReset
                                | lastKey = Just 27
                                , insertMode = False
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
    if model.insertMode then
        { model | document = SelectList.append "" model.document |> SelectList.next }
    else
        model


view : Model -> Html Msg
view model =
    div
        []
        [ viewHeader model
        , viewHeaderSpacer
        , viewModal model.visibleModal model
        , viewTextArea model
        ]


viewHeaderSpacer : Html Msg
viewHeaderSpacer =
    div [ Style.headerSpacer ] []


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Style.header ]
        [ span [ Style.headerTitle, onClick (ToggleModal About) ] [ text appName ]
        , button
            [ Style.button, onClick NewDocument ]
            [ Icons.file, text "New" ]
        , button
            [ Style.button, onClick SaveToLocalStorage ]
            [ Icons.save, text "Save" ]
        , button
            [ Style.button, onClick (ToggleModal ImportExport) ]
            [ Icons.upload, text "Import/Export" ]
        , button
            [ Style.button
            , onClick ToggleEdit
            ]
            [ Icons.toggle model.editMode, text "Editing" ]
        , button
            [ Style.button
            , onClick ToggleView
            ]
            [ Icons.toggle model.compiledView, text "Formatted" ]
        , button [ Style.button, onClick (ToggleModal Help) ] [ Icons.questionCircle ]
        , span [ Style.headerTitle ] <| List.map viewNotification model.notifications
        ]


viewNotification : Notification -> Html Msg
viewNotification notification =
    case notification of
        Saved ->
            text "Saved."

        Imported ->
            text "Imported."


viewTextArea : Model -> Html Msg
viewTextArea model =
    case ( model.editMode, model.compiledView ) of
        ( True, True ) ->
            viewCompiledEditMode model

        ( True, False ) ->
            viewEditMode model

        ( False, True ) ->
            viewCompiledMode model

        ( False, False ) ->
            viewMarkdownMode model


viewCompiledMode : Model -> Html Msg
viewCompiledMode model =
    div
        [ Style.line ]
        [ viewPointer False
        , SelectList.toList model.document
            |> String.join "\n"
            |> toHtml
        ]


viewEditMode : Model -> Html Msg
viewEditMode model =
    div
        [ Style.line ]
        [ viewPointer False
        , textarea [ Style.textArea, onInput InputRaw ]
            [ text (serialize model.document) ]
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


viewCompiledEditMode : Model -> Html Msg
viewCompiledEditMode model =
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
                if model.insertMode then
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
        [ Html.Attributes.class "markdown-body"
        , Html.Attributes.style
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
