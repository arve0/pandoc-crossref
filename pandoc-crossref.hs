{-# LANGUAGE FlexibleContexts #-}
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Pandoc.Generic
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M

data RefRec = RefRec { refIndex :: (Int, Int)
                     , refTitle :: [Inline]
                     }

type RefMap = M.Map String RefRec

-- from data-accessor http://www.haskell.org/haskellwiki/Record_access
-- Copyright (c) Henning Thielemann <haskell@henning-thielemann.de>, Luke Palmer <lrpalmer@gmail.com>
-- Licensed under BSD3 -- see BSD3.md
type Accessor r a  =  a -> r -> (a, r)

setProp :: Accessor r a -> a -> r -> r
setProp f x = snd . f x

getProp :: Accessor r a -> r -> a
getProp f = fst . f undefined

modifyProp :: Accessor r a -> (a -> a) -> r -> r
modifyProp f g rOld =
   let (a,rNew) = f (g a) rOld
   in  rNew
-- end data-accessor

-- state data type
data References = References { imgRefs :: RefMap
                             , eqnRefs :: RefMap
                             , tblRefs :: RefMap
                             , curChap :: Int
                             }

-- accessors
imgRefs' :: Accessor References RefMap
imgRefs' new r@References{imgRefs=old} = (old, r{imgRefs=new})

eqnRefs' :: Accessor References RefMap
eqnRefs' new r@References{eqnRefs=old} = (old, r{eqnRefs=new})

tblRefs' :: Accessor References RefMap
tblRefs' new r@References{tblRefs=old} = (old, r{tblRefs=new})

defaultReferences :: References
defaultReferences = References M.empty M.empty M.empty 0

data Options = Options { useCleveref :: Bool
                       , sepChapters :: Bool
                       , figureTitle :: [Inline]
                       , tableTitle  :: [Inline]
                       , titleDelim  :: [Inline]
                       , figPrefix   :: [Inline]
                       , eqnPrefix   :: [Inline]
                       , tblPrefix   :: [Inline]
                       , chapDelim   :: [Inline]
                       , rangeDelim  :: [Inline]
                       , lofTitle    :: [Block]
                       , lotTitle    :: [Block]
                       , outFormat   :: Maybe Format
                       }

--state monad
type WS a = State References a

main :: IO ()
main = toJSONFilter go

go :: Maybe Format -> Pandoc -> Pandoc
go fmt (Pandoc meta bs) = Pandoc meta $ evalState doWalk defaultReferences
  where
  doWalk =
    walkM (replaceAttrImages opts) bs
    >>= bottomUpM (replaceRefs opts)
    >>= bottomUpM (listOf opts)
  opts = Options {
      useCleveref = getMetaBool False "cref"
    , sepChapters = getMetaBool False "chapters"
    , figureTitle = getMetaString "Figure" "figureTitle"
    , tableTitle  = getMetaString "Table" "tableTitle"
    , titleDelim  = getMetaString ":" "titleDelimiter"
    , figPrefix   = getMetaString "fig." "figPrefix"
    , eqnPrefix   = getMetaString "eq." "eqnPrefix"
    , tblPrefix   = getMetaString "tbl." "tblPrefix"
    , chapDelim   = getMetaString "." "chapDelim"
    , rangeDelim  = getMetaString "-" "rangeDelim"
    , lofTitle    = getMetaBlock (Header 1 nullAttr) "List of Figures" "lofTitle"
    , lotTitle    = getMetaBlock (Header 1 nullAttr) "List of Tables" "lotTitle"
    , outFormat   = fmt
  }
  getMetaBool def name = getBool def $ lookupMeta name meta
  getBool _ (Just (MetaBool b)) = b
  getBool b _ = b
  getMetaString def name = getString def $ lookupMeta name meta
  getString _ (Just (MetaString s)) = normalizeInlines [Str s]
  getString _ (Just (MetaInlines s)) = s
  getString def _ = normalizeInlines [Str def]
  getMetaBlock block def name = getBlock block def $ lookupMeta name meta
  getBlock _ _ (Just (MetaBlocks b)) = b
  getBlock block def m = [block $ getString def m]

replaceAttrImages :: Options -> Block -> WS Block
replaceAttrImages opts x@(Header 1 _ _)
  | sepChapters opts
  = do
    modify (\r@References{curChap=cc} -> r{curChap=cc+1})
    return x
replaceAttrImages opts (Para (Image alt img:c))
  | Just label <- getRefLabel "fig" c
  = do
    idxStr <- replaceAttr opts label alt imgRefs'
    let alt' = case outFormat opts of
          Just f | isFormat "latex" f ->
            RawInline (Format "tex") ("\\label{"++label++"}") : alt
          _  ->
            figureTitle opts++ Space : idxStr ++ titleDelim opts ++ [Space]++alt
    return $ Para [Image alt' (fst img,"fig:")]
replaceAttrImages opts (Para (Math DisplayMath eq:c))
  | Just label <- getRefLabel "eq" c
  = case outFormat opts of
      Just f | isFormat "latex" f ->
        let eqn = "\\begin{equation}"++eq++"\\label{"++label++"}\\end{equation}"
        in return $ Para [RawInline (Format "tex") eqn]
      _ -> do
        idxStr <- replaceAttr opts label [] eqnRefs'
        let eq' = eq++"\\qquad("++stringify idxStr++")"
        return $ Para [Math DisplayMath eq']
replaceAttrImages opts (Table title align widths header cells)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = do
    idxStr <- replaceAttr opts label (init title) tblRefs'
    let title' =
          case outFormat opts of
              Just f | isFormat "latex" f ->
                [RawInline (Format "tex") ("\\label{"++label++"}")]
              _  ->
                tableTitle opts++Space : idxStr++titleDelim opts++[Space]
          ++ init title
    return $ Table title' align widths header cells
replaceAttrImages _ x = return x

getRefLabel :: String -> [Inline] -> Maybe String
getRefLabel _ [] = Nothing
getRefLabel tag ils
  | Str attr <- last ils
  , all (==Space) (init ils)
  , "}" `isSuffixOf` attr
  , ("{#"++tag++":") `isPrefixOf` attr
  = init `fmap` stripPrefix "{#" attr
getRefLabel _ _ = Nothing

replaceAttr :: Options -> String -> [Inline] -> Accessor References RefMap -> WS [Inline]
replaceAttr o label title prop
  = do
    chap  <- gets curChap
    index <- (1+) `fmap` gets (M.size . getProp prop)
    modify $ modifyProp prop $ M.insert label RefRec {
      refIndex=(chap,index)
    , refTitle=title
    }
    if sepChapters o
    then return $ Str (show chap) : chapDelim o ++ [Str (show index)]
    else return [Str (show index)]

-- accessors to state variables
accMap :: M.Map String (Accessor References RefMap)
accMap = M.fromList [("fig:",imgRefs')
                    ,("eq:" ,eqnRefs')
                    ,("tbl:",tblRefs')
                    ]

-- accessors to options
prefMap :: M.Map String (Options -> [Inline])
prefMap = M.fromList [("fig:",figPrefix)
                     ,("eq:" ,eqnPrefix)
                     ,("tbl:",tblPrefix)
                     ]

prefixes :: [String]
prefixes = M.keys accMap

getRefPrefix :: Options -> String -> [Inline]
getRefPrefix opts prefix | null refprefix = []
                         | otherwise   = refprefix ++ [Space]
                         where refprefix = lookupUnsafe prefix prefMap opts

lookupUnsafe :: Ord k => k -> M.Map k v -> v
lookupUnsafe = (fromMaybe undefined .) . M.lookup

replaceRefs :: Options -> [Inline] -> WS [Inline]
replaceRefs opts (Cite cits _:xs)
  | Just prefix <- allCitsPrefix cits
  = (++ xs) `fmap` replaceRefs' prefix opts cits
  where
    replaceRefs' = case outFormat opts of
                    Just f | isFormat "latex" f -> replaceRefsLatex
                    _                           -> replaceRefsOther
replaceRefs _ x = return x

allCitsPrefix :: [Citation] -> Maybe String
allCitsPrefix cits = foldl f Nothing prefixes
  where
  f x@(Just _) _ = x
  f _ p | all (isPrefixOf p . citationId) cits = Just p
  f _ _ = Nothing

replaceRefsLatex :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsLatex prefix opts cits =
  return $ p ++ [texcit]
  where
    texcit =
      RawInline (Format "tex") $
      if useCleveref opts then
        " \\cref{"++listLabels prefix "" "" cits++"}"
        else
          listLabels prefix " \\ref{" "}" cits
    p | useCleveref opts = []
      | otherwise = getRefPrefix opts prefix

listLabels :: String -> String -> String -> [Citation] -> String
listLabels prefix p s = foldl' joinStr "" . mapMaybe (getLabel prefix)
  where
  joinStr acc i | null acc  = p++i++s
                | otherwise = acc++","++p++i++s

getLabel :: String -> Citation -> Maybe String
getLabel prefix Citation{citationId=cid}
  | prefix `isPrefixOf` cid = Just cid
  | otherwise = Nothing

replaceRefsOther :: String -> Options -> [Citation] -> WS [Inline]
replaceRefsOther prefix opts cits = do
  indices <- mapM (getRefIndex prefix) cits
  let
    indices' = groupBy ((==) `on` fmap fst) (sort indices)
  return $ getRefPrefix opts prefix ++ normalizeInlines (concatMap (makeIndices opts) indices')

getRefIndex :: String -> Citation -> WS (Maybe (Int, Int))
getRefIndex prefix Citation{citationId=cid}
  | prefix `isPrefixOf` cid
  = gets (fmap refIndex . M.lookup cid . getProp prop)
  | otherwise = return Nothing
  where
  prop = lookupUnsafe prefix accMap

makeIndices :: Options -> [Maybe (Int,Int)] -> [Inline]
makeIndices _ s | any isNothing s = [Str "??"]
makeIndices o s = intercalate sep $ reverse $ map f $ foldl' f2 [] $ catMaybes s
  where
  f2 [] i = [[i]]
  f2 ([]:xs) i = [i]:xs
  f2 l@(x@((_,hx):_):xs) i@(_,ni)
    | ni-hx == 0 = l        -- remove duplicates
    | ni-hx == 1 = (i:x):xs -- group sequental
    | otherwise     = [i]:l    -- new group
  f []  = []                          -- drop empty lists
  f [w] = show' w                    -- single value
  f [w1,w2] = show' w2 ++ sep ++ show' w1 -- two values
  f (x:xs) = show' (last xs) ++ rangeDelim o ++ show' x -- shorten more than two values
  sep = [Str ", "]
  show' (c,n) = if sepChapters o && c>0
    then [Str $ show c] ++ chapDelim o ++ [Str $ show n]
    else [Str $ show n]

listOf :: Options -> [Block] -> WS [Block]
listOf Options{outFormat=Just f} x | isFormat "latex" f = return x
listOf opts (Para [RawInline (Format "tex") "\\listoffigures"]:xs)
  = gets imgRefs >>= makeList (lofTitle opts) xs
listOf opts (Para [RawInline (Format "tex") "\\listoftables"]:xs)
  = gets tblRefs >>= makeList (lotTitle opts) xs
listOf _ x = return x

makeList :: [Block] -> [Block] -> M.Map String RefRec -> WS [Block]
makeList title xs refs
  = return $
      title ++
      OrderedList style (item `map` refsSorted)
      : xs
  where
    refsSorted = sortBy compare' $ M.toList refs
    compare' (_,RefRec{refIndex=i}) (_,RefRec{refIndex=j}) = compare i j
    item = (:[]) . Plain . refTitle . snd
    style = (1,DefaultStyle,DefaultDelim)

isFormat :: String -> Format -> Bool
isFormat fmt (Format f)
  | fmt == f = True
  | Just (x:_) <- stripPrefix fmt f
  , x `elem` "+-"
  = True
  | otherwise = False

-- Copied over from Text.Pandoc.Shared
normalizeInlines :: [Inline] -> [Inline]
normalizeInlines (Str x : ys) =
  case concat (x : map fromStr strs) of
        ""     -> rest
        n      -> Str n : rest
   where
     (strs, rest)  = span isStr $ normalizeInlines ys
     isStr (Str _) = True
     isStr _       = False
     fromStr (Str z) = z
     fromStr _       = error "normalizeInlines - fromStr - not a Str"
normalizeInlines (Space : ys) =
  if null rest
     then []
     else Space : rest
   where isSp Space = True
         isSp _     = False
         rest       = dropWhile isSp $ normalizeInlines ys
normalizeInlines (Emph xs : zs) =
  case normalizeInlines zs of
       (Emph ys : rest) -> normalizeInlines $
         Emph (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Emph xs' : rest
normalizeInlines (Strong xs : zs) =
  case normalizeInlines zs of
       (Strong ys : rest) -> normalizeInlines $
         Strong (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Strong xs' : rest
normalizeInlines (Subscript xs : zs) =
  case normalizeInlines zs of
       (Subscript ys : rest) -> normalizeInlines $
         Subscript (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Subscript xs' : rest
normalizeInlines (Superscript xs : zs) =
  case normalizeInlines zs of
       (Superscript ys : rest) -> normalizeInlines $
         Superscript (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Superscript xs' : rest
normalizeInlines (SmallCaps xs : zs) =
  case normalizeInlines zs of
       (SmallCaps ys : rest) -> normalizeInlines $
         SmallCaps (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> SmallCaps xs' : rest
normalizeInlines (Strikeout xs : zs) =
  case normalizeInlines zs of
       (Strikeout ys : rest) -> normalizeInlines $
         Strikeout (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Strikeout xs' : rest
normalizeInlines (RawInline _ [] : ys) = normalizeInlines ys
normalizeInlines (RawInline f xs : zs) =
  case normalizeInlines zs of
       (RawInline f' ys : rest) | f == f' -> normalizeInlines $
         RawInline f (xs ++ ys) : rest
       rest -> RawInline f xs : rest
normalizeInlines (Code _ "" : ys) = normalizeInlines ys
normalizeInlines (Code attr xs : zs) =
  case normalizeInlines zs of
       (Code attr' ys : rest) | attr == attr' -> normalizeInlines $
         Code attr (xs ++ ys) : rest
       rest -> Code attr xs : rest
-- allow empty spans, they may carry identifiers etc.
-- normalizeInlines (Span _ [] : ys) = normalizeInlines ys
normalizeInlines (Span attr xs : zs) =
  case normalizeInlines zs of
       (Span attr' ys : rest) | attr == attr' -> normalizeInlines $
         Span attr (normalizeInlines $ xs ++ ys) : rest
       rest -> Span attr (normalizeInlines xs) : rest
normalizeInlines (Note bs : ys) = Note (normalizeBlocks bs) :
  normalizeInlines ys
normalizeInlines (Quoted qt ils : ys) =
  Quoted qt (normalizeInlines ils) : normalizeInlines ys
normalizeInlines (Link ils t : ys) =
  Link (normalizeInlines ils) t : normalizeInlines ys
normalizeInlines (Image ils t : ys) =
  Image (normalizeInlines ils) t : normalizeInlines ys
normalizeInlines (Cite cs ils : ys) =
  Cite cs (normalizeInlines ils) : normalizeInlines ys
normalizeInlines (x : xs) = x : normalizeInlines xs
normalizeInlines [] = []

normalizeBlocks :: [Block] -> [Block]
normalizeBlocks (Null : xs) = normalizeBlocks xs
normalizeBlocks (Div attr bs : xs) =
  Div attr (normalizeBlocks bs) : normalizeBlocks xs
normalizeBlocks (BlockQuote bs : xs) =
  case normalizeBlocks bs of
       []    -> normalizeBlocks xs
       bs'   -> BlockQuote bs' : normalizeBlocks xs
normalizeBlocks (BulletList [] : xs) = normalizeBlocks xs
normalizeBlocks (BulletList items : xs) =
  BulletList (map normalizeBlocks items) : normalizeBlocks xs
normalizeBlocks (OrderedList _ [] : xs) = normalizeBlocks xs
normalizeBlocks (OrderedList attr items : xs) =
  OrderedList attr (map normalizeBlocks items) : normalizeBlocks xs
normalizeBlocks (DefinitionList [] : xs) = normalizeBlocks xs
normalizeBlocks (DefinitionList items : xs) =
  DefinitionList (map go' items) : normalizeBlocks xs
  where go' (ils, bs) = (normalizeInlines ils, map normalizeBlocks bs)
normalizeBlocks (RawBlock _ "" : xs) = normalizeBlocks xs
normalizeBlocks (RawBlock f x : xs) =
   case normalizeBlocks xs of
        (RawBlock f' x' : rest) | f' == f ->
          RawBlock f (x ++ ('\n':x')) : rest
        rest -> RawBlock f x : rest
normalizeBlocks (Para ils : xs) =
  case normalizeInlines ils of
       []   -> normalizeBlocks xs
       ils' -> Para ils' : normalizeBlocks xs
normalizeBlocks (Plain ils : xs) =
  case normalizeInlines ils of
       []   -> normalizeBlocks xs
       ils' -> Plain ils' : normalizeBlocks xs
normalizeBlocks (Header lev attr ils : xs) =
  Header lev attr (normalizeInlines ils) : normalizeBlocks xs
normalizeBlocks (Table capt aligns widths hdrs rows : xs) =
  Table (normalizeInlines capt) aligns widths
    (map normalizeBlocks hdrs) (map (map normalizeBlocks) rows)
  : normalizeBlocks xs
normalizeBlocks (x:xs) = x : normalizeBlocks xs
normalizeBlocks [] = []

stringify :: Walkable Inline a => a -> String
stringify = query go' . walk deNote
  where go' :: Inline -> String
        go' Space = " "
        go' (Str x) = x
        go' (Code _ x) = x
        go' (Math _ x) = x
        go' LineBreak = " "
        go' _ = ""
        deNote (Note _) = Str ""
        deNote x = x
