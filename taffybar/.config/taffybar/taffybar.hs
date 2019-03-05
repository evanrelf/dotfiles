import System.Taffybar
import System.Taffybar.SimpleConfig
import qualified System.Taffybar.Widget as Widget

main :: IO ()
main = do
  let clock = Widget.textClockNew Nothing "%a %b %e %l:%M %p" 1
  -- let workspaces = Widget.workspacesNew Widget.defaultWorkspacesConfig

  simpleTaffybar $ defaultSimpleTaffyConfig
    { barPosition = Top
    , startWidgets = []
    , endWidgets = [ clock ]
    }

