module BasicTest where

import Basic (Book, Library, Patron)
import Data.Either (isLeft, isRight)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)

import Basic qualified
import Hedgehog qualified
import Helpers qualified

import Control.Monad.Except qualified as Except
import Control.Monad.State qualified as State
import Control.Monad.Writer qualified as Writer
import Data.Map qualified as Map
import Data.Text qualified as Text
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange

import Test.Hspec
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Hedgehog

-- ---------------------------------------------- --
--                   Unit Tests                   --
-- ---------------------------------------------- --
-- https://hackage.haskell.org/package/hspec-expectations-0.8.4/docs/Test-Hspec-Expectations.html

test :: String -> IO () -> SpecWith (Arg (IO ()))
test = it

expectSuccess :: Expectation
expectSuccess = True `shouldBe` True

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
        lookupByAuthorUnitTest2
      describe "Lookup by ISBN" $ do
        lookupByISBNUnitTest1
        lookupByISBNUnitTest2
    describe "Tests for displaying book availability" $ do
      bookAvailabilityDisplayUnitTest1
      bookAvailabilityDisplayUnitTest2
    describe "Tests for displaying book availability" $ do
      bookAvailabilityDisplayUnitTest1
      bookAvailabilityDisplayUnitTest2

addBookUnitTest1 :: SpecWith ()
addBookUnitTest1 = do
  test "A book can be added to a library if said library doesn't already have a copy of said book" $ do
    let firstBookTitle = Basic.title bookOne
    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT $ Basic.addBook bookOne)) library
    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 Basic.copies bookLibEntryM `shouldSatisfy` (== 1)

addBookUnitTest2 :: SpecWith ()
addBookUnitTest2 = do
  test "A copy of a book can be added if a library already has at least one copy of a given book" $ do
    let firstBookTitle = Basic.title bookOne
    let program = Basic.addBook bookOne >> Basic.addBook bookOne
    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library
    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 Basic.copies bookLibEntryM `shouldSatisfy` (== 2)

removeBookUnitTest1 :: SpecWith ()
removeBookUnitTest1 = do
  test "A copy of a book can be removed from a library" $ do
    let firstBookTitle = Basic.title bookOne
    let program = Basic.addBook bookOne >> Basic.addBook bookOne >> Basic.removeBook firstBookTitle

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library

    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isJust
    maybe 0 Basic.copies bookLibEntryM `shouldSatisfy` (== 1)

removeBookUnitTest2 :: SpecWith ()
removeBookUnitTest2 = do
  test "A book can be removed from a libraries inventory list if it has no copies" $ do
    let firstBookTitle = Basic.title bookOne
    let program = Basic.addBook bookOne >> Basic.removeBook firstBookTitle >> Basic.removeBook firstBookTitle

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library

    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isNothing

removeBookUnitTest3 :: SpecWith ()
removeBookUnitTest3 = do
  test "An error occurs when a removal is attempted on a book that doesn't exist" $ do
    let firstBookTitle = Basic.title bookOne
    let program = Basic.removeBook firstBookTitle

    let (programOutput, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library

    let bookLibEntryM = Map.lookup firstBookTitle currentBookShelf
    bookLibEntryM `shouldSatisfy` isNothing

    programOutput `shouldSatisfy` isLeft

    let isBookNotFoundError = case programOutput of
          Left (Basic.BookNotFound _) -> True
          _ -> False

    isBookNotFoundError `shouldBe` True

lookupByTitleUnitTest1 :: SpecWith ()
lookupByTitleUnitTest1 = do
  test "A user can lookup a book by it's title" $ do
    let thirdBookTitle = Basic.title bookThree
    let program = Basic.addBook bookThree

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library
    let bookLibEntryM = Basic.lookupBookByTitle currentBookShelf thirdBookTitle

    bookLibEntryM `shouldSatisfy` isRight
    either (const "") (Basic.title . Basic.bookInfo) bookLibEntryM `shouldBe` thirdBookTitle

lookupByTitleUnitTest2 :: SpecWith ()
lookupByTitleUnitTest2 = do
  test "A user cannot lookup a book that doesn't exit (lookup by title)" $ do
    let firstBookTitle = Basic.title bookOne
    let program = Basic.addBook bookThree

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library
    let bookLibEntryM = Basic.lookupBookByTitle currentBookShelf firstBookTitle

    bookLibEntryM `shouldSatisfy` isLeft

    let isBookNotFoundError = case bookLibEntryM of
          Left (Basic.BookNotFound _) -> True
          _ -> False

    isBookNotFoundError `shouldBe` True

lookupByAuthorUnitTest1 :: SpecWith ()
lookupByAuthorUnitTest1 = do
  test "A user can lookup a book by the name of the author" $ do
    let firstBookAuthor = Basic.author bookOne
    let program = Basic.addBook bookOne

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library
    let bookLibEntryM = Basic.lookupBookByAuthor currentBookShelf firstBookAuthor

    bookLibEntryM `shouldSatisfy` isRight
    either (const "") (Basic.author . Basic.bookInfo) bookLibEntryM `shouldBe` firstBookAuthor

lookupByAuthorUnitTest2 :: SpecWith ()
lookupByAuthorUnitTest2 = do
  test "A user cannot lookup a book that doesn't exit (lookup by author)" $ do
    let bookOneAuthor = Basic.author bookOne
    let program = Basic.addBook bookThree

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library
    let bookLibEntryM = Basic.lookupBookByAuthor currentBookShelf bookOneAuthor

    bookLibEntryM `shouldSatisfy` isLeft

    let isBookNotFoundError = case bookLibEntryM of
          Left (Basic.BookNotFound _) -> True
          _ -> False

    isBookNotFoundError `shouldBe` True

lookupByISBNUnitTest1 :: SpecWith ()
lookupByISBNUnitTest1 = do
  test "A user can lookup a book its ISBN" $ do
    let bookOneISBN = Basic.isbn bookOne
    let program = Basic.addBook bookOne

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library
    let bookLibEntryM = Basic.lookupBookByISBN currentBookShelf bookOneISBN

    bookLibEntryM `shouldSatisfy` isRight
    either (const "") (Basic.isbn . Basic.bookInfo) bookLibEntryM `shouldBe` bookOneISBN

lookupByISBNUnitTest2 :: SpecWith ()
lookupByISBNUnitTest2 = do
  test "A user cannot lookup a book that doesn't exit (lookup by ISBN)" $ do
    let bookOneISBN = Basic.isbn bookOne
    let program = Basic.addBook bookThree

    let (_, Basic.Library currentBookShelf _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library
    let bookLibEntryM = Basic.lookupBookByAuthor currentBookShelf bookOneISBN

    bookLibEntryM `shouldSatisfy` isLeft

    let isBookNotFoundError = case bookLibEntryM of
          Left (Basic.BookNotFound _) -> True
          _ -> False

    isBookNotFoundError `shouldBe` True

bookAvailabilityDisplayUnitTest1 :: SpecWith ()
bookAvailabilityDisplayUnitTest1 = do
  test "A user can check the availability of a book that exists" $ do
    let bookOneTitle = Basic.title bookOne
    let program = Basic.addBook bookOne >> Basic.addBook bookOne >> Basic.addBook bookOne >> Basic.displayBookAvailability bookOneTitle
    let (programOutput, _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library

    let bookOneCopiesCount = Helpers.tshow (3 :: Integer)

    programOutput `shouldSatisfy` isRight

    case programOutput of
      Right (availabilityText, _)
        | Text.isInfixOf bookOneCopiesCount availabilityText -> expectSuccess
        | otherwise -> expectationFailure "Received wrong availability status text"
      _ -> expectationFailure "Received wrong availability status text"

bookAvailabilityDisplayUnitTest2 :: SpecWith ()
bookAvailabilityDisplayUnitTest2 = do
  test "A user cannot check the availability of a book that doesn't exist" $ do
    let titleOfBookThatDoesntExist = "Title of book that doesn't exist" :: Text
    let program = Basic.addBook bookOne >> Basic.addBook bookOne >> Basic.addBook bookOne >> Basic.displayBookAvailability titleOfBookThatDoesntExist
    let (programOutput, _) = State.runState (Except.runExceptT (Writer.runWriterT program)) library

    programOutput `shouldSatisfy` isRight

    case programOutput of
      Right (availabilityText, _)
        | Text.isInfixOf "does not exist" availabilityText -> expectSuccess
        | otherwise -> expectationFailure "Received wrong availability status text"
      _ -> expectationFailure "Received wrong availability status text"

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

-- -------------------------------------------------------------------------- --
--                                    Data                                    --
-- -------------------------------------------------------------------------- --
bookOne :: Book
bookOne = Basic.Book{Basic.title = "Production Haskell", Basic.author = "Matt Parsons", Basic.isbn = "ISBN-13: 978-3-16-148410-0"}

bookTwo :: Book
bookTwo = Basic.Book{Basic.title = "Simple Haskell", Basic.author = "Marco Sampellegrini", Basic.isbn = "ISBN-13: 978-0-262-13472-9"}

bookThree :: Book
bookThree = Basic.Book{Basic.title = "Optics By Example", Basic.author = "Chris Penner", Basic.isbn = "ISBN-13: 978-1-566-19237-5"}

patronOne :: Patron
patronOne = Basic.Patron{Basic.name = "Michael Williams", Basic.idNumber = 67890}

patronTwo :: Patron
patronTwo = Basic.Patron{Basic.name = "Olivia Wilson", Basic.idNumber = 89012}

patronThree :: Patron
patronThree = Basic.Patron{Basic.name = "Jessica Horn", Basic.idNumber = 89012}

library :: Library
library = Basic.Library{Basic.getBookShelf = Map.empty, Basic.getBorrowLog = Map.empty}
