import Control.Monad.State.Lazy (execStateT)
import Yi
import Yi.Config.Default (defaultConfig)
import Yi.Config.Default.HaskellMode (configureHaskellMode)
import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)
import Yi.Config.Simple.Types

myConfig :: ConfigM ()
myConfig = do
  configureVty
  configureVim
  configureHaskellMode

main :: IO ()
main = do
  config <- execStateT (runConfigM myConfig) defaultConfig
  startEditor config Nothing
