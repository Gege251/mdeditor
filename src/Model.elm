module Model exposing (..)

import FileReader exposing (NativeFile)
import Keyboard exposing (KeyCode)
import SelectList exposing (SelectList)


appName : String
appName =
    "Markdown Editor 0.0.1"


type alias Model =
    { document : SelectList String
    , insertMode : Bool
    , editMode : Bool
    , compiledView : Bool
    , visibleModal : Modal
    , notifications : List Notification
    , lastKey : Maybe KeyCode
    , clipboard : String
    }


type Msg
    = NoOp
    | InputMD String
    | InputRaw String
    | MoveUp
    | MoveDown
    | NewDocument
    | ToggleInsert
    | ToggleEdit
    | ToggleView
    | NewLine
    | ToggleModal Modal
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


type Modal
    = NoModal
    | Help
    | ImportExport
    | About
