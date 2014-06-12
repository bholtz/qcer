import Test.Hspec

import QuickChecker hiding (main)
import Data.Map.Lazy hiding (map)

main :: IO ()
main = hspec $ describe "Testing QuickChecker" $ do
  let mp = fromList [("a", "allimony"), ("b", "banana"), ("c", "crux")]
  describe "substitute" $ do
    it "empty substitution" $ 
      substitute mp "This is simple text" `shouldBe` "This is simple text"
    it "simple substitutions" $
      substitute mp "I pay %%a%% for %%b%%" `shouldBe` "I pay allimony for banana"
    it "complex substitutions" $
      substitute mp "%%a%%%%b%%%%c%%%%a%%" `shouldBe` "allimonybananacruxallimony"
      
  let fns = extractFile hsText
      (fnName, comment, typesig, props) = fns !! 2
  describe "extraction" $ do
    it "correct size" $
      length fns `shouldBe` 3
    it "function name correct" $
      fnName `shouldBe` "main"
    it "comment extracted" $
      comment `shouldBe` "-- This is a weird property %%%blah%%% and %%%blah blah%%%\n"
    it "collects all properties" $
      props `shouldBe` ["blah", "blah blah"]
    it "correct type signature" $
      typesig `shouldBe` "IO ()"
      
      
hsText :: String
hsText = unlines ["extractFile :: String -> [(FnName, Comment, TypeSig, [Property])]"
                , "extractFile str ="
                , "  let pattern = \"((^--.*$)*(^[ ]*[a-zA-z][^ \t\n\r\v\f]*[ \t]::.*$))\""
                , "      matches = map (\\(x:xs) -> x) (str =~ pattern :: [[String]])"
                , "  in map extractFn matches"  
                , ""
                , "-- The file containing a template for each test"
                , "templateFile :: String"
                , "templateFile = \"template.hs\""
                , ""
                , "-- This is a weird property %%%blah%%% and %%%blah blah%%%"
                , "main :: IO ()"
                , "main = undefined"]