import Test.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Walk
import Text.Pandoc.Generic
import Util.Options
import Control.Monad.State
import References.Types
import Util.Settings
import Util.Accessor
import References.Accessors
import qualified References.Blocks
import qualified References.Refs
import qualified References.List
import qualified Util.Template
import qualified Data.Map as M
import Data.Monoid

main :: IO ()
main = hspec $ do
    describe "References.Blocks.replaceBlocks" $ do
      it "Labels images" $
        testBlocks (figure "test.jpg" [] "Test figure" "figure")
        (figure "test.jpg" "fig:" "Figure 1: Test figure" [],
          def{imgRefs=M.fromList $ refRec' "fig:figure" 1 "Test figure"})
      it "Labels equations" $
        testBlocks (equation "a^2+b^2=c^2" "equation")
        (equation "a^2+b^2=c^2\\qquad(1)" [],
          def{eqnRefs=M.fromList $ refRec'' "eq:equation" 1})
      it "Labels tables" $
        testBlocks (table' "Test table" "table")
        (table' "Table 1: Test table" [],
          def{tblRefs=M.fromList $ refRec' "tbl:table" 1 "Test table"})

    describe "References.Refs.replaceRefs" $ do
      it "References one image" $
        testRefs' "fig:" [1] [4] imgRefs' "fig.\160\&4"
      it "References multiple images" $
        testRefs' "fig:" [1..3] [4..6] imgRefs' "fig.\160\&4-6"
      it "References one equation" $
        testRefs' "eq:" [1] [4] eqnRefs' "eq.\160\&4"
      it "References multiple equations" $
        testRefs' "eq:" [1..3] [4..6] eqnRefs' "eq.\160\&4-6"
      it "References one table" $
        testRefs' "tbl:" [1] [4] tblRefs' "tbl.\160\&4"
      it "References multiple tables" $
        testRefs' "tbl:" [1..3] [4..6] tblRefs' "tbl.\160\&4-6"

    describe "References.List.listOf" $ do
      it "Generates list of tables" $
        testList (para $ rawInline "tex" "\\listoftables")
                 def{tblRefs=M.fromList $ refRec' "tbl:1" 4 "4" <> refRec' "tbl:2" 5 "5" <> refRec' "tbl:3" 6 "6"}
                 (header 1 (text "List of Tables") <> orderedList ((plain . str . show) `map` [4..6 :: Int]))
      it "Generates list of figures" $
        testList (para $ rawInline "tex" "\\listoffigures")
                 def{imgRefs=M.fromList $ refRec' "fig:1" 4 "4" <> refRec' "fig:2" 5 "5" <> refRec' "fig:3" 6 "6"}
                 (header 1 (text "List of Figures") <> orderedList ((plain . str . show) `map` [4..6 :: Int]))

    describe "Util.Template" $
      it "Applies templates" $
        let template=Util.Template.makeTemplate defaultMeta (toList $ displayMath "figureTitle" <> displayMath "i" <> displayMath "t")
        in Util.Template.applyTemplate [Str "1"] [Str "title"] template `shouldBe`
           toList (str "Figure" <> str "1" <> str "title")

citeGen :: String -> [Int] -> Inlines
citeGen p l = cite (mconcat $ map (cit . (p++) . show) l) mempty

refGen :: String -> [Int] -> [Int] -> M.Map String RefRec
refGen p l1 l2 = M.fromList $ mconcat $ zipWith refRec'' (((p++) . show) `map` l1) l2

refRec' :: String -> Int -> String -> [(String, RefRec)]
refRec' ref i tit = [(ref, RefRec{refIndex=(0,i),refTitle=toList $ text tit})]

refRec'' :: String -> Int -> [(String, RefRec)]
refRec'' ref i = refRec' ref i []

testRefs' :: String -> [Int] -> [Int] -> Accessor References (M.Map String RefRec) -> String -> Expectation
testRefs' p l1 l2 prop res = testRefs (para $ citeGen p l1) (setProp prop (refGen p l1 l2) def) (para $ text res)

testBlocks :: Blocks -> (Blocks, References) -> Expectation
testBlocks arg res = runState (walkM (f defaultOptions) arg) def `shouldBe` res
  where f = References.Blocks.replaceBlocks

testRefs :: Blocks -> References -> Blocks -> Expectation
testRefs bs st res = runState (bottomUpM (References.Refs.replaceRefs defaultOptions) (toList bs)) st `shouldBe` (toList res,st)

testList :: Blocks -> References -> Blocks -> Expectation
testList bs st res = runState (bottomUpM (References.List.listOf defaultOptions) (toList bs)) st `shouldBe` (toList res,st)

figure :: String -> String -> String -> String -> Blocks
figure src title alt ref = para (image src title (text alt) <> ref' "fig" ref)

equation :: String -> String -> Blocks
equation eq ref = para (displayMath eq <> ref' "eq" ref)

table' :: String -> String -> Blocks
table' title ref = table (text title <> ref' "tbl" ref) []
   [para $ str "H1", para $ str "H2"]
  [[para $ str "C1", para $ str "C2"]]

ref' :: String -> String -> Inlines
ref' p n | null n  = mempty
         | otherwise = space <> str ("{#"++p++":"++n++"}")

defaultOptions :: Options
defaultOptions = getOptions defaultMeta Nothing

defCit :: Citation
defCit = Citation{citationId = ""
                 ,citationPrefix = []
                 ,citationSuffix = []
                 ,citationHash = 0
                 ,citationNoteNum = 0
                 ,citationMode = NormalCitation
                 }

cit :: String -> [Citation]
cit r = [defCit{citationId=r}]
