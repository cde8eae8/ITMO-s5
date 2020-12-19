{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad  (void)
import Control.Monad.Reader

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core 
import FileManager
import FileManagerEnv
import Commands
import Control.Exception
import Data.IORef
import Data.Maybe

wrapExceptions 
  :: FileManager m 
  -> FileManager (Either SomeException m)
wrapExceptions fm = do 
  envRef <- ask
  env <- liftIO $ readIORef envRef
  liftIO $ try $
    do
      (modifiedEnv, res) <- runFileManager fm env
      writeIORef envRef modifiedEnv
      return res

data DirectoryEntry = Directory String
                    | File String
                    deriving (Show, Ord)

main :: IO ()
main = void $ do
  -- replace entrie with Entry = File | Directory
  (changedDirectory, emitChangedDirectory) <- newEvent @(Env, [String])
  startGUI defaultConfig $ \window -> void $ mdo
    return window # set title "Simple Filesystem Client"

    -- GUI elements
    upBtn         <- UI.button #+ [string "Up"]
    createFileBtn <- UI.button #+ [string "Create file"]
    createDirBtn  <- UI.button #+ [string "Create directory"]
    directoryView <- UI.listBox bDirectoryViewItems bSelection bDisplayDataItem
    
    --createBtn   <- UI.button #+ [string "Create"]
    --deleteBtn   <- UI.button #+ [string "Delete"]
    --listBox     <- UI.listBox  bListBoxItems bSelection bDisplayDataItem
    --filterEntry <- UI.entry    bFilterString
    --((firstname, lastname), tDataItem)
    --            <- dataItem    bSelectionDataItem

    -- GUI layout
    element directoryView # set (attr "size") "10" # set style [("width","200px")]
    
    --let glue = string " "
    getBody window #+ [grid
        [[element upBtn, element createFileBtn, element createDirBtn]
        ,[element directoryView]
        --,[row [element createBtn, element deleteBtn], glue]
        ]]

    -- events
    let eSelection = rumors $ UI.userSelection directoryView

    --bDirectoryViewItems <- stepper [1, 2, 3] never
    --bDatabase <- accumB emptydb $ concatenate <$> unions
    --    [ create ("Emil","Example") <$ eCreate
    --    , filterJust $ update' <$> bSelection <@> eDataItemIn
    --    , delete <$> filterJust (bSelection <@ eDelete)
    --    ]

    --bDisplayDataItem <- (\v -> string . show) <$> bDirectoryViewItems
    
    bFS <- accumB (Env "" "") $ concatenate <$> unions 
      [ const . fst <$> changedDirectory ]

    bDirectoryViewItems <- accumB [] $ concatenate <$> unions
      [ const . snd <$> changedDirectory ]

    bDisplayDataItem <- stepper (string . show) never

    bSelection <- stepper Nothing $ head <$> unions 
      [ eSelection ]

    on UI.click upBtn $ \_ -> liftIO $ do
      putStrLn "Event"
      env <- currentValue bFS
      (st, entries) <- flip runFileManager env $ do 
        cdCommand [".."]
        lsCommand []
      --line <- liftIO readLn
      emitChangedDirectory (st, entries)

    on UI.keydown (getElement directoryView) $ 
      \key -> 
        case key of 
          13 -> liftIO $ do
            val <- currentValue bSelection
            if isJust val 
            then do
              env <- currentValue bFS
              (st, entries) <- flip runFileManager env $ do 
                cdCommand [fromJust val]
                lsCommand []
              --line <- liftIO readLn
              emitChangedDirectory (st, entries)
            else
              return ()



