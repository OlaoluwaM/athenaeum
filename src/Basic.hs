module Basic (addBook, removeBook, lookupBookByAuthor, lookupBookByISBN, lookupBookByTitle, displayBookAvailability) where

import Control.Error (ExceptT (ExceptT), tryRight)
import Control.Monad.Except (Except, ExceptT, liftEither)
import Control.Monad.State
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Writer
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time
import Helpers
import PyF (fmt)

data Book = Book
  { title :: Text
  , author :: Text
  , isbn :: Text
  }
  deriving (Show, Eq)

data BorrowedBook = BorrowedBook
  { infoOfBorrowedBook :: Book
  , returnDeadline :: Day
  , borrowDate :: Day
  }
  deriving (Show, Eq)

data Patron = Patron
  { name :: Text
  , cardNumber :: Integer
  , booksBorrowed :: [BorrowedBook]
  }
  deriving (Show, Eq)

data LibraryEntry = LibraryEntry
  { bookInfo :: Book
  , copies :: Integer
  , borrowedBy :: [Patron]
  }
  deriving (Show, Eq)

newtype ErrorContext = ErrorContext {context :: Text} deriving (Eq, Show)

type OptionalErrorContext = Maybe ErrorContext

data LibrarySysError = NoCopies OptionalErrorContext | BookNotFound OptionalErrorContext | CustomError ErrorContext
  deriving (Eq, Show)

type Library = Map Text LibraryEntry

type BasicStack a = StateT Library (Either LibrarySysError) a

-- ------------------------------ Athenaeum Basic API ----------------------------- --
addBook :: Book -> BasicStack Text
addBook newBook@(Book newBookTitle _ _) = do
  currentLibrary <- get
  let bookAvailabilityStatus = lookupBookByTitle currentLibrary newBookTitle

  let (updatedLibrary, msg) = case bookAvailabilityStatus of
        Right _ -> (incrementBookCopiesCount currentLibrary, newCopyMsg)
        Left _ -> (addNewBookToLibrary currentLibrary, newBookMsg)

  put updatedLibrary
  return msg
 where
  incrementBookCopiesCount = Map.adjust (modifyBookCopyCount (+ 1)) newBookTitle
  addNewBookToLibrary = Map.insert newBookTitle LibraryEntry{bookInfo = newBook, copies = 1, borrowedBy = []}

  newBookMsg = [fmt|Added the book '{newBookTitle}' to the library!|]
  newCopyMsg = [fmt|Added a new copy of the book '{newBookTitle}' to the library!|]

lookupBookByTitle :: Library -> Text -> Either LibrarySysError LibraryEntry
lookupBookByTitle lib bookTitle = lookupBookByTitleIncludingBooksWithoutCopies lib bookTitle >>= hideBooksWithNoCopies

lookupBookByAuthor :: Library -> Text -> Either LibrarySysError LibraryEntry
lookupBookByAuthor lib bookAuthor = let errorMsg = [fmt|Could not find book written by author '{bookAuthor}'|] in lookupBookByBookProperty lib author errorMsg bookAuthor

lookupBookByISBN :: Library -> Text -> Either LibrarySysError LibraryEntry
lookupBookByISBN lib bookISBN = let errorMsg = [fmt|Could not find book with ISBN code '{bookISBN}'|] in lookupBookByBookProperty lib isbn errorMsg bookISBN

removeBook :: Text -> BasicStack Text
removeBook bookTitle = do
  currentLibrary <- get
  (LibraryEntry _ copiesCount _) <- lift $ lookupBookByTitleIncludingBooksWithoutCopies currentLibrary bookTitle

  let (updatedLibrary, msg) =
        if copiesCount > 0 then (decrementBookCopiesCount currentLibrary, copyRemovedMsg) else (deleteBookFromLibrary currentLibrary, bookDeletedMsg)

  put updatedLibrary
  return msg
 where
  decrementBookCopiesCount = Map.adjust (modifyBookCopyCount (subtract 1)) bookTitle
  deleteBookFromLibrary = Map.delete bookTitle

  copyRemovedMsg = [fmt|Removed a copy of the book '{bookTitle}'|]
  bookDeletedMsg = [fmt|Deleted the book '{bookTitle}' from library|]

displayBookAvailability :: Text -> BasicStack Text
displayBookAvailability bookTitle = do
  lib <- get
  let bookAvailabilityStatus = lookupBookByTitleIncludingBooksWithoutCopies lib bookTitle

  let bookAvailabilityMsg = case bookAvailabilityStatus of
        Right (LibraryEntry _ copiesCount _) -> [fmt|We currently have, {copiesCount}, of the book, '{bookTitle}'|]
        Left (NoCopies _) -> [fmt|Sorry, it looks like all copies of the book, '{bookTitle}' have either been borrowed or deleted|]
        Left (BookNotFound _) -> [fmt|Sorry, it looks like the book, '{bookTitle}' does not exist, perhaps it was removed|]
        Left (CustomError (ErrorContext errContext)) -> [fmt|The following error occurred when attempting to display the availability of the book '{bookTitle}': {errContext}|]

  return bookAvailabilityMsg

borrowBook :: (Day, Integer) -> Text -> Patron -> BasicStack Patron
borrowBook (currentDay, randomOffsetForDeadline) bookTitle patron@(Patron _ _ booksBorrowed') = do
  _ <- removeBook bookTitle
  currentLibrary <- get
  libraryEntry <- lift $ lookupBookByTitle currentLibrary bookTitle

  modify (`trackTheBorrower` libraryEntry)
  let updatedPatron = trackTheBorrowedBook libraryEntry

  return updatedPatron
 where
  trackTheBorrower lib bookLibEntry' = Map.adjust (const $ bookLibEntry'{borrowedBy = patron : borrowedBy bookLibEntry'}) bookTitle lib
  trackTheBorrowedBook (LibraryEntry bookInfo' _ _) =
    let returnDeadline' = addDays randomOffsetForDeadline currentDay
     in patron{booksBorrowed = (BorrowedBook{infoOfBorrowedBook = bookInfo', borrowDate = currentDay, returnDeadline = returnDeadline'}) : booksBorrowed'}

returnBook :: Patron -> Book -> BasicStack Patron
returnBook patron@(Patron _ _ borrowedBooks) borrowedBook = do
  currentLibrary <- get
  let listOfBorrowedBooks = map infoOfBorrowedBook borrowedBooks
  guard (borrowedBook `elem` listOfBorrowedBooks)
  undefined

-- returnBook :: Library -> Book -> Patron -> (Patron, Library)
-- returnBook lib book borrower =
--     if List.null (booksBorrowed borrower) then (borrower, lib) else (borrower{booksBorrowed = List.filter (\(BorrowedBook bk) -> title book /= title bk) $ booksBorrowed borrower}, addBook lib book)

-- -------------------------- Athenaeum Basic API Helpers ------------------------- --
modifyBookCopyCount :: (Integer -> Integer) -> LibraryEntry -> LibraryEntry
modifyBookCopyCount copyModFn libBookEntry@(LibraryEntry _ currentCopyCount _) = libBookEntry{copies = updatedCopyCount}
 where
  updatedCopyCount = let newBookCopyCount = copyModFn currentCopyCount in max newBookCopyCount 0

lookupBookByTitleIncludingBooksWithoutCopies :: Library -> Text -> Either LibrarySysError LibraryEntry
lookupBookByTitleIncludingBooksWithoutCopies lib bookTitle = addErrorMsg (Map.lookup bookTitle lib)
 where
  addErrorMsg = maybeToEither (BookNotFound $ Just $ ErrorContext errorMsg)
  errorMsg = [fmt|Could not find book with title '{bookTitle}'|]

hideBooksWithNoCopies :: LibraryEntry -> Either LibrarySysError LibraryEntry
hideBooksWithNoCopies libEntry@(LibraryEntry (Book bookTitle _ _) copiesCount _)
  | copiesCount > 0 = Right libEntry
  | otherwise = Left $ NoCopies $ Just $ ErrorContext [fmt|Sorry, there don't seem to be any more copies of the book {bookTitle} left|]

lookupBookByBookProperty :: (Eq a) => Library -> (Book -> a) -> Text -> a -> Either LibrarySysError LibraryEntry
lookupBookByBookProperty lib bookFieldLabel errorMsg bookPropertyToSearchBy =
  addErrorMsg errorMsg (List.find bookPropertyLookup (Map.toList lib)) >>= hideBooksWithNoCopies . snd
 where
  bookPropertyLookup (_, LibraryEntry bookInfo' _ _) = bookPropertyToSearchBy == bookFieldLabel bookInfo'
  addErrorMsg = maybeToEither . BookNotFound . Just . ErrorContext

getDay :: IO Day
getDay = utctDay <$> getCurrentTime

-- ---------------------------------- Main ---------------------------------- --
firstBookInLibrary :: Book
firstBookInLibrary = Book{title = "Production Haskell", author = "Matt Parsons", isbn = "ISBN-13: 978-3-16-148410-0"}

library :: Library
library = Map.empty

-- -- Not sure why `stack runghc Solution.hs` isn't working, but using `stack ghci` and running main interactively does the trick
-- main :: IO ()
-- main = do
--     let r = removeBookFromLibrary (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary
--     let g = removeBookFromLibrary (addBook (addBook (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary) firstBookInLibrary) firstBookInLibrary
--     let o = Patron{name = "Ola", cardNumber = 6162633, booksBorrowed = [BorrowedBook firstBookInLibrary]}
--     print (returnBook g firstBookInLibrary o)
