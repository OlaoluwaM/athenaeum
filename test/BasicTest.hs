module BasicTest where

import Basic
import Control.Monad.Except
import Control.Monad.State (runState)
import Control.Monad.Writer
import Data.Either (isLeft, isRight)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.Hedgehog

-- ---------------------------------------------- --
--                   Unit Tests                   --
-- ---------------------------------------------- --
-- https://hackage.haskell.org/package/hspec-expectations-0.8.4/docs/Test-Hspec-Expectations.html

test :: String -> IO () -> SpecWith (Arg (IO ()))
test = it

spec_tests :: Spec
spec_tests = do
  describe "Unit tests" $ do
    describe "Tests for the addBook operation" $ do
      addBookUnitTest1
      addBookUnitTest2
    describe "Tests for the removeBook operation" $ do
      removeBookUnitTest1
      removeBookUnitTest2
      removeBookUnitTest3
    describe "Tests for lookup operations" $ do
      describe "Lookup by title" $ do
        lookupByTitleUnitTest1
        lookupByTitleUnitTest2
      describe "Lookup by Author" $ do
        lookupByAuthorUnitTest1

-- lookupByAuthorUnitTest2

addBookUnitTest1 :: SpecWith ()
addBookUnitTest1 = do
  test "A book can be added to a library if said library doesn't already have a copy of said book" $ do
    let firstBookTitle = title bookOne
    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT $ addBook bookOne)) library
    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 copies bookLibEntryM `shouldSatisfy` (== 1)

addBookUnitTest2 :: SpecWith ()
addBookUnitTest2 = do
  test "A copy of a book can be added if a library already has at least one copy of a given book" $ do
    let firstBookTitle = title bookOne
    let program = addBook bookOne >> addBook bookOne
    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library
    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 copies bookLibEntryM `shouldSatisfy` (== 2)

removeBookUnitTest1 :: SpecWith ()
removeBookUnitTest1 = do
  test "A copy of a book can be removed from a library" $ do
    let firstBookTitle = title bookOne
    let program = addBook bookOne >> addBook bookOne >> removeBook firstBookTitle

    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library

    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 copies bookLibEntryM `shouldSatisfy` (== 1)

removeBookUnitTest2 :: SpecWith ()
removeBookUnitTest2 = do
  test "A book can be removed from a libraries inventory list if it has no copies" $ do
    let firstBookTitle = title bookOne
    let program = addBook bookOne >> removeBook firstBookTitle >> removeBook firstBookTitle

    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library

    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isNothing

removeBookUnitTest3 :: SpecWith ()
removeBookUnitTest3 = do
  test "An error occurs when a removal is attempted on a book that doesn't exist" $ do
    let firstBookTitle = title bookOne
    let program = removeBook firstBookTitle

    let (programOutput, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library

    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isNothing

    programOutput `shouldSatisfy` isLeft

    let isBookNotFoundError = case programOutput of
          Left (BookNotFound _) -> True
          _ -> False

    isBookNotFoundError `shouldBe` True

lookupByTitleUnitTest1 :: SpecWith ()
lookupByTitleUnitTest1 = do
  test "A user can lookup a book by it's title" $ do
    let thirdBookTitle = title bookThree
    let program = addBook bookThree

    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library
    let bookLibEntryM = lookupBookByTitle currentBookShelf thirdBookTitle

    bookLibEntryM `shouldSatisfy` isRight
    either (const "") (title . bookInfo) bookLibEntryM `shouldBe` thirdBookTitle

lookupByTitleUnitTest2 :: SpecWith ()
lookupByTitleUnitTest2 = do
  test "A user cannot lookup a book that doesn't exit" $ do
    let firstBookTitle = title bookOne
    let program = addBook bookThree

    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library
    let bookLibEntryM = lookupBookByTitle currentBookShelf firstBookTitle

    bookLibEntryM `shouldSatisfy` isLeft

    let isBookNotFoundError = case bookLibEntryM of
          Left (BookNotFound _) -> True
          _ -> False

    isBookNotFoundError `shouldBe` True

lookupByAuthorUnitTest1 :: SpecWith ()
lookupByAuthorUnitTest1 = do
  test "A user can lookup a book by the name of the author" $ do
    let firstBookAuthor = author bookOne
    let program = addBook bookOne

    let (_, Library currentBookShelf _) = runState (runExceptT (runWriterT program)) library
    let bookLibEntryM = lookupBookByAuthor currentBookShelf firstBookAuthor

    bookLibEntryM `shouldSatisfy` isRight
    either (const "") (author . bookInfo) bookLibEntryM `shouldBe` firstBookAuthor

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
