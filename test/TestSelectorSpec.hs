module TestSelectorSpec (main, spec) where

import           Test.Hspec
import           Orphans
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
    it "should return all args when no --dt-select= options" $ do
      extractTestSelectors ["foo","bar"] `shouldBe` (Right $ Args [] ["foo","bar"])
    it "should return a selector and leave other args alone" $ do
      extractTestSelectors 
        [ "--dt-select=foo:21" ,"bar"] 
        `shouldBe` 
        (Right $ Args [TestSelector "foo" 21 Nothing] ["bar"])      
    it "should return a selector with start and end line num" $ do
      extractTestSelectors 
        [ "--dt-select=foo:21-23"] 
        `shouldBe` 
        (Right $ Args [TestSelector "foo" 21 (Just 23)] [])              
        
  describe "filterModuleTests" $ do
    let loc1 = Located (Location "" 13) (Property " ")
        loc2 = Located (Location "" 22) (Property " ")
        testModule = Module "foo" Nothing [[loc1,loc2]]
        
    it "should filter nothing with no selectors" $ do
      filterModuleContent [] testModule `shouldBe` testModule 

    it "should filter everything with a selector that doesn't apply" $ do
      (filterModuleContent [TestSelector "bar" 22 Nothing] testModule)
      `shouldBe` 
      testModule { moduleContent = [] }       
      
    it "should keep the stuff that is selected" $ do
      (filterModuleContent [TestSelector "foo" 22 Nothing] testModule)
      `shouldBe` 
      testModule { moduleContent = [[loc2]] }
      
  describe "filterModules" $ do 
    let loc1 = Located (Location "" 13) (Property " ")
        loc2 = Located (Location "" 22) (Property " ")
        testModule1 = Module "foo" Nothing [[loc1,loc2]]
        testModule2 = Module "bar" Nothing [[loc1,loc2]]
        testModules = [testModule1,testModule2]

    it "should filter stuff" $ do 
      filterModules [TestSelector "foo" 22 Nothing] testModules
      `shouldBe`
      ([testModule1 {moduleContent = [[loc2]] }])

