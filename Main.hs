import Graphics.UI.Threepenny.Core (liftIO, startGUI, defaultConfig)

import Control.Applicative ((<$>), (<*>), pure)
import System.Random (getStdGen)

import Tetris (newGame, emptyGame, BoardHeight(..), BoardWidth(..))
import ThreepennyController (tetrisEvent, bindNow)
import ThreepennyView (setupView, drawGame)


main :: IO ()
main = startGUI defaultConfig $ \window -> do setupView dims window
                                              beginning <- liftIO $ newGame <$> getStdGen <*> pure dims
                                              e <- tetrisEvent beginning window
                                              bindNow (\(oldT, newT) -> drawGame oldT newT window) (emptyGame dims, beginning) e
    where dims = (BW 10, BH 20)
