module Util.Options where

import Text.Pandoc.Definition
import Util.Meta
import Util.Template

data Options = Options { useCleveref :: Bool
                       , sepChapters :: Bool
                       , figPrefix   :: [Inline]
                       , eqnPrefix   :: [Inline]
                       , tblPrefix   :: [Inline]
                       , chapDelim   :: [Inline]
                       , rangeDelim  :: [Inline]
                       , lofTitle    :: [Block]
                       , lotTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       , figureTemplate :: Template
                       , tableTemplate  :: Template
                       }

getOptions :: Meta -> Maybe Format -> Options
getOptions dtv fmt =
  Options {
      useCleveref = getMetaBool "cref" dtv
    , sepChapters = getMetaBool "chapters" dtv
    , figPrefix   = getMetaInlines "figPrefix" dtv
    , eqnPrefix   = getMetaInlines "eqnPrefix" dtv
    , tblPrefix   = getMetaInlines "tblPrefix" dtv
    , chapDelim   = getMetaInlines "chapDelim" dtv
    , rangeDelim  = getMetaInlines "rangeDelim" dtv
    , lofTitle    = getMetaBlock "lofTitle" dtv
    , lotTitle    = getMetaBlock "lotTitle" dtv
    , outFormat   = fmt
    , figureTemplate = makeTemplate dtv $ getMetaInlines "figureTemplate" dtv
    , tableTemplate  = makeTemplate dtv $ getMetaInlines "tableTemplate" dtv
  }
