{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module GUI (mainGUI) where

import Commands
import Control.Exception
import Control.Monad.Reader
import Data.Maybe
import Data.Sort
import FileManager
import FileManagerEnv
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core 
import System.FilePath

handleErrors :: (SomeException -> UI ()) -> Env -> FileManager a -> (Env -> a -> UI ()) -> UI ()
handleErrors handler env fm actions = void $ do
  res <- liftIO $ try $ runFileManager fm env
  case res of
    Right (env, a) -> actions env a
    Left e  -> handler e

mainGUI :: String -> IO ()
mainGUI root = void $ do
  (eDirectoryUpdated, emitDirectoryUpdated) <- newEvent @(Env, [DirectoryEntry])
  (eFileOpened, emitFileOpened)             <- newEvent @(Env, String)
  (eFileClosed, emitFileClosed)             <- newEvent @(Env, ())
  (eError, emitError)                       <- newEvent @String
  (eItemSelected, emitItemSelected)         <- newEvent @DirectoryEntry
  (eLoadedFileInfo, emitLoadedFileInfo)     <- newEvent @Info
  (eDirectoryNeedUpdate, emitDirectoryNeedUpdate) <- newEvent @(Env, ())
  
  startGUI defaultConfig { jsStatic = Just "html" }$ \window -> void $ mdo
    return window # set title "Simple Filesystem Client"

    -- GUI elements
    upBtn         <- UI.button #+ [string "Up"]
    createFileBtn <- UI.button #+ [string "+ file"]
    createDirBtn  <- UI.button #+ [string "+ dir"]
    removeEntryBtn <- UI.button #+ [string "-"]
    closeFileBtn  <- UI.button #+ [string "Close file"]
    searchFileBtn  <- UI.button #+ [string "Search"]
    directoryView <- UI.listBox bDirectoryViewItems bSelection bDisplayDataItem
    fileEdit      <- UI.textarea  -- # set style [("height", "100%")]
    errorView     <- UI.textarea #. "errors-area"
                        # set (attr "readonly") "" 
    infoView      <- UI.textarea #. "info-area"
                        # set (attr "readonly") ""
    currentDir    <- UI.textarea #. "currentDir-area"
                        # set (attr "readonly") ""
    userInputField <- UI.input 
    
    -- GUI layout
    UI.addStyleSheet window "style.css"
    element directoryView 
      # set (attr "size") "10" 
    
    getBody window # set style [("height", "600px")]
    getBody window #+ [ UI.div #. "current-dir" #+ 
                        [ element currentDir ]
                      , UI.div #. "buttons" #+ 
                        [ element upBtn
                        , element createFileBtn
                        , element createDirBtn
                        , element removeEntryBtn
                        , element closeFileBtn
                        , element searchFileBtn
                        , element userInputField
                        ]
                      , UI.div #. "others" #+ 
                        [ UI.div #. "main-row" #+ [element directoryView] 
                        , UI.div #. "main-row" #+ [element fileEdit] 
                        , UI.div #. "main-row" #+ [element infoView] 
                        ]
                      , UI.div #+ [UI.div #. "errors" #+ 
                        [ element errorView ]]
                      ]

    -- helpers
    
    let voidHandleErrors :: FileManager () -> (Env -> () -> UI ()) -> UI ()
        voidHandleErrors = \fileManagerActions okHandler -> void $ do
            env <- liftIO $ currentValue bFS
            handleErrors (liftIO . emitError . show) env fileManagerActions okHandler

    -- events
    
    let eSelection = rumors $ UI.userSelection directoryView

    bFS <- accumB (Env root "") $ concatenate <$> unions 
      [ const . fst <$> eDirectoryNeedUpdate ]

    bDirectoryViewItems <- accumB [] $ concatenate <$> unions
      [ const . sort . snd <$> eDirectoryUpdated ]

    bDisplayDataItem <- stepper (string . show) never

    bSelection <- stepper Nothing $ head <$> unions 
      [ eSelection 
      , Nothing <$ eDirectoryUpdated
      ]
       
    bFileOpened <- stepper False $ head <$> unions 
      [ True <$ eFileOpened 
      , False <$ eFileClosed
      ]

    bFileInfo <- stepper "" $ head <$> unions
      [ show <$> eLoadedFileInfo
      , "" <$ eDirectoryUpdated
      ]

    bError <- stepper "" $ head <$> unions
      [ eError 
      , "" <$ eSelection
      , "" <$ eDirectoryUpdated
      , "" <$ eFileClosed
      , "" <$ eFileOpened
      ]

    element errorView # sink UI.value bError 
    element errorView # sink UI.content bError 
    element infoView # sink UI.value bFileInfo

    mapM (\elem -> element elem # sink UI.enabled bFileOpened)
      [ closeFileBtn
      , fileEdit
      ]
    mapM (\elem -> element elem # sink UI.enabled (not <$> bFileOpened)) 
      [ removeEntryBtn 
      , upBtn
      , createFileBtn
      , createDirBtn
      , userInputField
      , getElement directoryView
      , searchFileBtn
      ]

    on UI.click closeFileBtn $ \_ -> do
      content <- fileEdit # get UI.value 
      element fileEdit # set UI.value ""
      path <- liftIO $ currentValue $ entryName . fromJust <$> bSelection
      voidHandleErrors (write [path, content])
        (\st _ -> void $ liftIO $ emitFileClosed (st, ()))

    on UI.click createFileBtn $ \_ -> do
      name <- userInputField # get UI.value
      voidHandleErrors (mkfile [name])
        (\st _ -> void $ liftIO $ emitDirectoryNeedUpdate (st, ()))

    on UI.click createDirBtn $ \_ -> do
      name <- userInputField # get UI.value
      voidHandleErrors (mkdir [name]) 
        (\st _ -> void $ liftIO $ emitDirectoryNeedUpdate (st, ()))

    on UI.click searchFileBtn $ \_ -> do
      name <- userInputField # get UI.value
      env <- currentValue bFS
      handleErrors (liftIO . emitError . show) env (find_ [name]) 
        (\st pathMaybe -> void $ 
          if isJust pathMaybe 
          then
            voidHandleErrors (cd [takeDirectory $ fromJust pathMaybe])
              (\st _ -> void $ liftIO $ emitDirectoryNeedUpdate (st, ()))
          else
            liftIO $ emitError $ "File " ++ name ++ " not found")

    --
    on UI.click upBtn $ \_ -> do
      voidHandleErrors (cd [".."]) 
        (\st _ -> void $ liftIO $ emitDirectoryNeedUpdate (st, ()))

    on UI.click removeEntryBtn $ \_ -> do
      selection <- currentValue bSelection
      when (isJust selection) $ do
        voidHandleErrors (do
            case fromJust selection of
              Directory dir -> rmdir [dir]
              File file -> rmfile [file]
          ) (\st _ -> void $ liftIO $ emitDirectoryNeedUpdate (st, ()))

    onEvent eDirectoryNeedUpdate $ \_ -> void $ do
      env <- currentValue bFS
      handleErrors (liftIO . emitError . show) env (ls []) 
        (\st entries -> void $ liftIO $ emitDirectoryUpdated (st, entries))

    onEvent eDirectoryUpdated $ \_ -> void $ do
      env <- currentValue bFS
      let path = getRealPathToCurrentDirFromEnv env
      element currentDir # set UI.value path

    --
    onChanges bSelection $ \selection -> 
      liftIO $ when (isJust selection) $ do
        emitItemSelected $ fromJust selection

    onEvent eItemSelected $ \item -> void $ do
      env <- currentValue bFS
      handleErrors (liftIO . emitError . show) env (info [entryName item])
        (\st entryInfo-> void $ liftIO $ emitLoadedFileInfo entryInfo)

    --
    on UI.keydown (getElement directoryView) $ 
      \case 
        13 -> do
          val <- liftIO $ currentValue bSelection
          when (isJust val) $ do
            env <- liftIO $ currentValue bFS
            case fromJust val of
              Directory path -> 
                handleErrors (liftIO . emitError . show) env (cd [path]) 
                  (\st entries -> void $ liftIO $ emitDirectoryNeedUpdate (st, entries))
              File path -> do
                handleErrors (liftIO . emitError . show) env (cat [path]) 
                  (\st content -> void $ do 
                    element fileEdit # set UI.value content
                    liftIO $ emitFileOpened (st, content))
        _ -> return ()

    liftIO $ emitDirectoryNeedUpdate (Env root "", ())




