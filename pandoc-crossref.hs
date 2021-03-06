import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Control.Monad.State

import References
import Util.Settings
import Util.Options

main :: IO ()
main = toJSONFilter go

go :: Maybe Format -> Pandoc -> IO Pandoc
go fmt (Pandoc meta bs) = do
  dtv <- getSettings meta
  let
    doWalk =
      walkM (replaceBlocks opts) bs
      >>= bottomUpM (replaceRefs opts)
      >>= bottomUpM (listOf opts)
    opts = getOptions dtv fmt
  return $ Pandoc meta $ evalState doWalk def
