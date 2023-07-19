module Basic (addBook, removeBook, lookupBookByAuthor, lookupBookByISBN, lookupBookByTitle, displayBookAvailability, borrowBook, returnBook) where

import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither)
import Control.Monad.State
import Control.Monad.Writer
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time
import Helpers
import PyF (fmt)

data Book = Book
  { title :: Text,
    author :: Text,
    isbn :: Text
  }
  deriving (Show, Eq)

data BorrowedBook = BorrowedBook
  { infoOfBorrowedBook :: Book,
    returnDeadline :: Day,
    borrowDate :: Day
  }
  deriving (Show, Eq)

data Patron = Patron
  { name :: Text,
    cardNumber :: Integer,
    booksBorrowed :: [BorrowedBook]
  }
  deriving (Show, Eq)

data LibraryEntry = LibraryEntry
  { bookInfo :: Book,
    copies :: Integer,
    borrowedBy :: [Patron]
  }
  deriving (Show, Eq)

newtype ErrorContext = ErrorContext {context :: Text} deriving (Eq, Show)

type OptionalErrorContext = Maybe ErrorContext

data LibrarySysError = NoCopies OptionalErrorContext | BookNotFound OptionalErrorContext | PastDue OptionalErrorContext | CustomError ErrorContext
  deriving (Eq, Show)

type Library = Map Text LibraryEntry

type BasicStack a = WriterT Text (ExceptT LibrarySysError (State Library)) a

-- ------------------------------ Athenaeum Basic API ----------------------------- --
addBook :: Book -> BasicStack LibraryEntry
addBook newBook@(Book newBookTitle _ _) = do
  currentLibrary <- get
  let bookAvailabilityStatus = lookupBookByTitle currentLibrary newBookTitle

  let (updatedLibrary, msg, bookLibEntry) = case bookAvailabilityStatus of
        Right bookLibEntry' -> (incrementBookCopiesCount currentLibrary, newCopyMsg, bookLibEntry')
        Left _ -> let (updatedLib, newLibEntry) = addNewBookToLibrary currentLibrary in (updatedLib, newBookMsg, newLibEntry)

  put updatedLibrary

  writer (bookLibEntry, msg)
  where
    incrementBookCopiesCount = Map.adjust (modifyBookCopyCount (+ 1)) newBookTitle
    addNewBookToLibrary currentLib = let newLibEntry = LibraryEntry {bookInfo = newBook, copies = 1, borrowedBy = []} in (Map.insert newBookTitle newLibEntry currentLib, newLibEntry)

    newBookMsg = [fmt|Added the book '{newBookTitle}' to the library!|]
    newCopyMsg = [fmt|Added a new copy of the book '{newBookTitle}' to the library!|]

lookupBookByTitle :: Library -> Text -> Either LibrarySysError LibraryEntry
lookupBookByTitle lib bookTitle = lookupBookByTitleIncludingBooksWithoutCopies lib bookTitle >>= hideBooksWithNoCopies

lookupBookByAuthor :: Library -> Text -> Either LibrarySysError LibraryEntry
lookupBookByAuthor lib bookAuthor = let errorMsg = [fmt|Could not find book written by author '{bookAuthor}'|] in lookupBookByBookProperty lib author errorMsg bookAuthor

lookupBookByISBN :: Library -> Text -> Either LibrarySysError LibraryEntry
lookupBookByISBN lib bookISBN = let errorMsg = [fmt|Could not find book with ISBN code '{bookISBN}'|] in lookupBookByBookProperty lib isbn errorMsg bookISBN

removeBook :: Text -> BasicStack (Maybe LibraryEntry)
removeBook bookTitle = do
  currentLibrary <- get
  bookLibEntry@(LibraryEntry _ copiesCount _) <- lift $ liftEither $ lookupBookByTitleIncludingBooksWithoutCopies currentLibrary bookTitle

  let (updatedLibrary, msg, maybeBookLibEntry) =
        if copiesCount > 0
          then (decrementBookCopiesCount currentLibrary, copyRemovedMsg, Just bookLibEntry)
          else (deleteBookFromLibrary currentLibrary, bookDeletedMsg, Nothing)

  put updatedLibrary
  writer (maybeBookLibEntry, msg)
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
        Left _ -> "An error Occurred"

  return bookAvailabilityMsg

borrowBook :: (Day, Integer) -> Text -> Patron -> BasicStack Patron
borrowBook (currentDay, randomOffsetForDeadline) bookTitle patron = do
  libraryEntry <- removeBook bookTitle >>= lift . liftEither . maybeToEither (bookNotBorrowableErr bookTitle)

  modify (`trackTheBorrower` libraryEntry)
  let updatedPatron = trackTheBorrowedBook patron libraryEntry

  writer (updatedPatron, outputMsg)
  where
    trackTheBorrower lib bookLibEntry@(LibraryEntry (Book bookTitle' _ _) _ patrons) =
      Map.adjust (const $ bookLibEntry {borrowedBy = patron : patrons}) bookTitle' lib

    trackTheBorrowedBook patron'@(Patron _ _ borrowedBooks') (LibraryEntry bookInfo' _ _) =
      patron'
        { booksBorrowed = (BorrowedBook {infoOfBorrowedBook = bookInfo', borrowDate = currentDay, returnDeadline = returnDeadline'}) : borrowedBooks'
        }

    returnDeadline' = addDays randomOffsetForDeadline currentDay
    outputMsg = [fmt|Thanks! You just borrowed the book '{bookTitle}'. Your return deadline is {tshow returnDeadline'}|]
    bookNotBorrowableErr bookTitle' = NoCopies $ Just $ ErrorContext {context = [fmt|Sorry! It looks like we don't have any copies of the book '{bookTitle' :: Text}' to give out|]}

returnBook :: Day -> Patron -> Book -> BasicStack Patron
returnBook currentDay patron@(Patron patronName _ borrowedBooks) borrowedBook@(Book bookTitle _ _) = do
  let maybeBorrowedBook = List.find (\(BorrowedBook bookInfo' _ _) -> bookInfo' == borrowedBook) borrowedBooks
  (BorrowedBook _ returnDeadline' _) <- lift $ liftEither $ maybeToEither (CustomError (ErrorContext wrongBorrowerMsg)) maybeBorrowedBook

  borrowedBookLibEntry <- addBook borrowedBook
  modify (`unTrackTheBorrower` borrowedBookLibEntry)

  let updatedPatron = unTrackTheBorrowedBook patron borrowedBookLibEntry
  let deadlineOffset = diffDays returnDeadline' currentDay

  when (deadlineOffset < 0) $ throwError $ PastDue $ Just $ ErrorContext [fmt|You are returning this book {abs deadlineOffset} days past due!|]

  writer (updatedPatron, [fmt|Thanks {patronName}! You just returned the book '{bookTitle}'|])
  where
    unTrackTheBorrower lib bookLibEntry@(LibraryEntry (Book bookTitle' _ _) _ patrons) =
      Map.adjust (const $ bookLibEntry {borrowedBy = filter (/= patron) patrons}) bookTitle' lib

    unTrackTheBorrowedBook (Patron _ _ borrowedBooks') (LibraryEntry bookInfo' _ _) =
      let filterFn (BorrowedBook borrowedBookInfo _ _) = borrowedBookInfo /= bookInfo'
       in patron {booksBorrowed = filter filterFn borrowedBooks'}

    wrongBorrowerMsg = "Oops, looks like you weren't the one who borrowed this book" :: Text

-- -------------------------- Athenaeum Basic API Helpers ------------------------- --
modifyBookCopyCount :: (Integer -> Integer) -> LibraryEntry -> LibraryEntry
modifyBookCopyCount copyModFn libBookEntry@(LibraryEntry _ currentCopyCount _) = libBookEntry {copies = updatedCopyCount}
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
firstBookInLibrary = Book {title = "Production Haskell", author = "Matt Parsons", isbn = "ISBN-13: 978-3-16-148410-0"}

library :: Library
library = Map.empty

-- main :: IO ()
-- main = do
--   currentDay <- getDay

--   undefined

-- -- Not sure why `stack runghc Solution.hs` isn't working, but using `stack ghci` and running main interactively does the trick
-- main :: IO ()
-- main = do
--     let r = removeBookFromLibrary (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary
--     let g = removeBookFromLibrary (addBook (addBook (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary) firstBookInLibrary) firstBookInLibrary
--     let o = Patron{name = "Ola", cardNumber = 6162633, booksBorrowed = [BorrowedBook firstBookInLibrary]}
--     print (returnBook g firstBookInLibrary o)
