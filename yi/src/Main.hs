module Main where

import Control.Monad.State.Lazy      (execStateT)
import Data.List                     (intersperse)
import Lens.Micro.Platform           ((.=))
import System.Environment            (getArgs)
import Yi
import Yi.Config.Default             (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.Vim         (configureVim)
import Yi.Config.Default.Vty         (configureVty)
import Yi.Config.Simple.Types        (ConfigM(runConfigM))
import Yi.Types                      (Action(EditorA))

myConfig :: ConfigM ()
myConfig = do
  configureVty
  configureVim
  configureHaskellMode

main :: IO ()
main = do
  files <- getArgs
  let actions = intersperse (EditorA newTabE) (map (YiA . openNewFile) files)
  config <- execStateT (runConfigM (myConfig >> (startActionsA .= actions))) defaultConfig
  startEditor config Nothing
