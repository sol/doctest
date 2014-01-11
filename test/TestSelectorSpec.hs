module TestSelectorSpec (main, spec) where

import           Test.Hspec
import           Orphans ()
import           TestSelector
import           Extract (Module (Module))
import           Location 
                 ( Located (Located)
                 ,Location (Location,UnhelpfulLocation))
import           Parse (DocTest (Property),moduleContent)
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "extractTestSelectors" $ do
    it "should return all args when no --dt-select= options" $ 
      extractTestSelectors ["foo","bar"] `shouldBe` (Right $ Args [] ["foo","bar"])
      
    it "should return a selector and leave other args alone" $ 
      extractTestSelectors 
        [ "--dt-select=foo:21" ,"bar"] 
        `shouldBe` 
        (Right $ Args [TestSelector "foo" $ SingleLine 21] ["bar"])      

    it "should return a selector with start and end line num" $ 
      extractTestSelectors 
        [ "--dt-select=foo:21-23"] 
        `shouldBe` 
        (Right $ Args [TestSelector "foo" $ LineRange 21 23] [])              
    
    it "should return AllLines lineSelector if no line numbers given" $
      extractTestSelectors [ "--dt-select=foo" , "rest"] 
      `shouldBe`
      (Right $ Args [TestSelector "foo" AllLines] ["rest"])
      
    it "should return left if just line numbers given" $
      extractTestSelectors [ "--dt-select=21-23"]
      `shouldBe`
      (Left $ ArgParserError "Module name starting with a letter" "21-23")

    it "should return left if no module given" $
      extractTestSelectors [ "--dt-select="]
      `shouldBe`
      (Left $ ArgParserError "Module name starting with a letter" "")

  describe "filterModuleContent" $ do
    let loc1 = Located (Location "" 13) (Property " ")
        loc2 = Located (Location "" 22) (Property " ")
        loc3 = Located (Location "" 24) (Property " ")
        loc4 = Located (UnhelpfulLocation "") (Property " ")
        testModule = Module "foo" Nothing [[loc1,loc2,loc3,loc4]]

    it "should filter nothing with no selectors" $
      filterModuleContent [] testModule `shouldBe` testModule 

    it "should filter everything with a selector that doesn't apply" $ 
      filterModuleContent [TestSelector "bar" AllLines] testModule
      `shouldBe` 
      testModule { moduleContent = [] }       
      
    it "should keep the stuff that is selected" $ 
      filterModuleContent [TestSelector "foo" $ SingleLine 22] testModule
      `shouldBe` 
      testModule { moduleContent = [[loc2]] }
      
    it "should filter a range" $
      filterModuleContent [TestSelector "foo" $ LineRange 13 22] testModule
      `shouldBe`
      testModule { moduleContent = [[loc1,loc2]] }
      
    it "should include all lines of a AllLines lineselected module" $
      filterModuleContent [TestSelector "foo" AllLines] testModule
      `shouldBe`
      testModule { moduleContent = [[loc1,loc2,loc3]]} 

  describe "filterModules" $ do 
    let loc1 = Located (Location "" 13) (Property " ")
        loc2 = Located (Location "" 22) (Property " ")
        testModule1 = Module "foo" Nothing [[loc1,loc2]]
        testModule2 = Module "bar" Nothing [[loc1,loc2]]
        testModules = [testModule1,testModule2]

    it "shouldn't filter anything if there are no filters at all" $ 
      filterModules [] testModules `shouldBe` testModules

    it "should filter stuff" $  
      filterModules [TestSelector "foo" $ SingleLine 22] testModules
      `shouldBe`
      [testModule1 {moduleContent = [[loc2]] }]
      
    it "should filter fine with two selectors" $  
      filterModules [
        TestSelector "foo" $ SingleLine 22
        , TestSelector "bar" $ SingleLine 13]  testModules
      `shouldBe`
      [testModule1 {moduleContent = [[loc2]] }
      , testModule2 {moduleContent = [[loc1]] } ]

    it "should filter a range" $
      filterModules [ TestSelector "foo" $ LineRange 13 22] testModules
      `shouldBe`
      [testModule1]

    it "should remove modules which become empty" $ 
      filterModules [TestSelector "foo" $ SingleLine 22] testModules
      `shouldBe`
      [testModule1 {moduleContent = [[loc2]]}]     
