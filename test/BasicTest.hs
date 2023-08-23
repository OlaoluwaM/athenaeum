module BasicTest where

import Basic
import Control.Monad.Except
import Control.Monad.State (runState)
import Control.Monad.Writer
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog

-- ---------------------------------------------- --
--                   Unit Tests                   --
-- ---------------------------------------------- --
-- https://hackage.haskell.org/package/hspec-expectations-0.8.4/docs/Test-Hspec-Expectations.html

spec_solution :: Spec
spec_solution = do
  describe "Unit tests" $ do
    describe "Tests for the addBook operation" $ do
      addBookUnitTest1
      addBookUnitTest2
    describe "Tests for the removeBook operation" $ do
      return ()

addBookUnitTest1 :: SpecWith ()
addBookUnitTest1 = do
  it "An admin can add a book to a library if said library doesn't already have a copy of said book" $ do
    let firstBookTitle = title bookOne
    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT $ addBook bookOne)) library
    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 copies bookLibEntryM `shouldSatisfy` (== 1)

addBookUnitTest2 :: SpecWith ()
addBookUnitTest2 = do
  it "An admin can add copies of a book to a library if said library already has at least one copy of said book" $ do
    let firstBookTitle = title bookOne
    let program = addBook bookOne >> addBook bookOne
    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library
    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 copies bookLibEntryM `shouldSatisfy` (== 2)

-- Any edge cases for the `addBook` operation?

-- removeBookUnitTest1 :: SpecWith ()
-- removeBookUnitTest1 = do
--   it "An admin can add copies of a book to a library if said library already has at least one copy of said book" $ do
--     let firstBookTitle = title bookOne
--     let program = addBook bookOne >> addBook bookOne
--     let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library
--     let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
--     bookLibEntryM `shouldSatisfy` isJust
--     maybe 0 copies bookLibEntryM `shouldSatisfy` (== 2)

-- correctInputUnitTest1 :: SpecWith ()
-- correctInputUnitTest1 = do
--   it "Should ensure that we can discern the caloric total of the Elf that has the most calories" $ calculateMaxCalories input `shouldBe` expected
--   where
--     input = joinWithEmptyLine ["400\n500\n300", "200\n300", "700"]
--     expected = 1200

-- correctInputUnitTest2 :: SpecWith ()
-- correctInputUnitTest2 = do
--   it "Should ensure that we can discern the caloric total of the Elf that has the most calories 2" $ calculateMaxCalories input `shouldBe` expected
--   where
--     input = joinWithEmptyLine ["7000000", "800\n500\n10000", "20\n100\n665", "800"]
--     expected = 7000000

-- incorrectInputUnitTest1 :: SpecWith ()
-- incorrectInputUnitTest1 = do
--   it "Should ensure that calculateMaxCalories function output 0 on empty input" $ calculateMaxCalories input `shouldBe` expected
--   where
--     input = ""
--     expected = 0

-- incorrectInputUnitTest2 :: SpecWith ()
-- incorrectInputUnitTest2 = do
--   it "Should ensure that calculateMaxCalories function output 0 if input contains only new lines" $ calculateMaxCalories input `shouldBe` expected
--   where
--     input = joinWithEmptyLine ["\n", "\n", "\n"]
--     expected = 0

-- incorrectInputUnitTest3 :: SpecWith ()
-- incorrectInputUnitTest3 = do
--   it "Should ensure that calculateMaxCalories function skips non-integers in calculation" $ calculateMaxCalories input `shouldBe` expected
--   where
--     input = joinWithEmptyLine ["aString\n4000\nfdf\n800", "200\n300\n400", "400\nyyr\n900"]
--     expected = 4800

-- -- ---------------------------------------------- --
-- --                 Property Tests                 --
-- -- ---------------------------------------------- --

-- test_propertyTests :: TestTree
-- test_propertyTests = testProperty "AOC Day 1: Property Based Tests" propertyTest

-- propertyTest :: Property
-- propertyTest =
--   property $ do
--     listOfIntLists <- forAll $ Gen.list (Range.linear 1 100) $ Gen.list (Range.linear 1 100) $ Gen.int (Range.linear 0 10000)
--     let input = joinWithEmptyLine $ map (intercalate "\n" . map show) listOfIntLists
--     let expected = maximum $ map sum listOfIntLists
--     calculateMaxCalories input === fromIntegral expected
