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

data LibrarySysError = NoCopies OptionalErrorContext | BookNotFound OptionalErrorContext | CustomError ErrorContext
  deriving (Eq, Show)

type Library = Map Text LibraryEntry

-- ------------------------------ Athenaeum Basic API ----------------------------- --
addBook :: Book -> State Library Text
addBook newBook@(Book newBookTitle _ _) = do
  currentLibrary <- get
  let bookAvailabilityStatus = evalStateT (lookupBookByTitle newBookTitle) currentLibrary

  let (updatedLibrary, msg) = case bookAvailabilityStatus of
        Right _ -> (incrementBookCopiesCount currentLibrary, newCopyMsg)
        Left _ -> (addNewBookToLibrary currentLibrary, newBookMsg)

  put updatedLibrary
  return msg
  where
    incrementBookCopiesCount = Map.adjust (modifyBookCopyCount (+ 1)) newBookTitle
    addNewBookToLibrary = Map.insert newBookTitle LibraryEntry {bookInfo = newBook, copies = 1, borrowedBy = []}

    newBookMsg = [fmt|Added the book '{newBookTitle}' to the library!|]
    newCopyMsg = [fmt|Added a new copy of the book '{newBookTitle}' to the library!|]

lookupBookByTitle :: Text -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByTitle bookTitle = lookupBookByTitleIncludingBooksWithoutCopies bookTitle >>= lift . hideBooksWithNoCopies

lookupBookByAuthor :: Text -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByAuthor bookAuthor = let errorMsg = [fmt|Could not find book written by author '{bookAuthor}'|] in lookupBookByBookProperty author errorMsg bookAuthor

lookupBookByISBN :: Text -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByISBN bookISBN = let errorMsg = [fmt|Could not find book with ISBN code '{bookISBN}'|] in lookupBookByBookProperty isbn errorMsg bookISBN

removeBook :: Text -> StateT Library (Either LibrarySysError) (LibraryEntry, Text)
removeBook bookTitle = do
  currentLibrary <- get
  bookLibEntry@(LibraryEntry _ copiesCount _) <- lookupBookByTitleIncludingBooksWithoutCopies bookTitle

  let (updatedLibrary, msg) =
        if copiesCount > 0 then (decrementBookCopiesCount currentLibrary, copyRemovedMsg) else (deleteBookFromLibrary currentLibrary, bookDeletedMsg)

  put updatedLibrary
  return (bookLibEntry, msg)
  where
    decrementBookCopiesCount = Map.adjust (modifyBookCopyCount (subtract 1)) bookTitle
    deleteBookFromLibrary = Map.delete bookTitle

    copyRemovedMsg = [fmt|Removed a copy of the book '{bookTitle}'|]
    bookDeletedMsg = [fmt|Deleted the book '{bookTitle}' from library|]

displayBookAvailability :: Text -> State Library Text
displayBookAvailability bookTitle = do
  lib <- get
  let bookAvailabilityStatus = evalStateT (lookupBookByTitle bookTitle) lib

  let bookAvailabilityMsg = case bookAvailabilityStatus of
        Right (LibraryEntry _ copiesCount _) -> [fmt|We currently have, {copiesCount}, of the book, '{bookTitle}'|]
        Left mLibSysError
          | NoCopies _ <- mLibSysError -> [fmt|Sorry, it looks like all copies of the book, '{bookTitle}' have either been borrowed or deleted|]
          | BookNotFound _ <- mLibSysError -> [fmt|Sorry, it looks like the book, '{bookTitle}' does not exist, perhaps it was removed|]
          | CustomError (ErrorContext errContext) <- mLibSysError -> [fmt|The following error occurred when attempting to display the availability of the book '{bookTitle}': {errContext}|]

  return bookAvailabilityMsg

-- trackBorrower :: Library -> Patron -> Book -> Maybe LibraryEntry
-- trackBorrower lib borrower book = (\libEntry -> libEntry{borrowedBy = borrower : borrowedBy libEntry}) <$> lookupBookByTitle lib (title book)

borrowBook :: Text -> Patron -> StateT Library (ExceptT LibrarySysError IO) Patron
borrowBook bookTitle patron@(Patron _ _ booksBorrowed') = do
  currentLibrary <- get
  let mLibEntryOfBookToBorrow = evalStateT (fst <$> removeBook bookTitle) currentLibrary
  --   let updatedLibraryAndLibEntryOfBook = mLibEntryOfBookToBorrow >>= Right . trackBorrower currentLibrary

  let bar = (tryRight $ mLibEntryOfBookToBorrow <&> trackTheBorrower currentLibrary :: ExceptT LibrarySysError IO (Library, LibraryEntry))

  --   let (updatedLibrary, messageAction) = case bar of
  --         Right (updatedLib, libEntryOfBookToBorrow) -> (updatedLib, trackTheBorrowedBook libEntryOfBookToBorrow)
  --         Left _ -> (currentLibrary, return (patron, ""))

  --   put updatedLibrary
  --   liftIO messageAction
  undefined
  where
    borrowBook' libEntryForBook = undefined
    trackTheBorrower lib bookLibEntry' = (Map.adjust (const $ bookLibEntry' {borrowedBy = patron : borrowedBy bookLibEntry'}) bookTitle lib, bookLibEntry')
    trackTheBorrowedBook (LibraryEntry bookInfo' _ _) = do
      currentDay <- getDay
      let returnDeadline' = addDays 4 currentDay

      let updatedPatron = patron {booksBorrowed = (BorrowedBook {infoOfBorrowedBook = bookInfo', borrowDate = currentDay, returnDeadline = returnDeadline'}) : booksBorrowed'}

      let textCurrentDay = tshow currentDay
      let textReturnDeadline = tshow returnDeadline'

      let returnMsg = [fmt|You have borrowed the book '{bookTitle}' on {textCurrentDay}, to be returned on {textReturnDeadline}|]
      return (updatedPatron, returnMsg :: Text)

-- borrow :: Library -> Book -> Patron -> (Patron, Library)
-- borrow lib book borrower = case lookupBookByTitle lib (title book) of
--     Just _ -> (borrower{booksBorrowed = BorrowedBook book : booksBorrowed borrower}, removeBookFromLibrary lib book)
--     Nothing -> (borrower, lib)

-- returnBook :: Library -> Book -> Patron -> (Patron, Library)
-- returnBook lib book borrower =
--     if List.null (booksBorrowed borrower) then (borrower, lib) else (borrower{booksBorrowed = List.filter (\(BorrowedBook bk) -> title book /= title bk) $ booksBorrowed borrower}, addBook lib book)

-- -------------------------- Athenaeum Basic API Helpers ------------------------- --
modifyBookCopyCount :: (Integer -> Integer) -> LibraryEntry -> LibraryEntry
modifyBookCopyCount copyModFn libBookEntry@(LibraryEntry _ currentCopyCount _) = libBookEntry {copies = updatedCopyCount}
  where
    updatedCopyCount = let newBookCopyCount = copyModFn currentCopyCount in max newBookCopyCount 0

lookupBookByTitleIncludingBooksWithoutCopies :: Text -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByTitleIncludingBooksWithoutCopies bookTitle = do
  lib <- get
  lift $ addErrorMsg (Map.lookup bookTitle lib)
  where
    addErrorMsg = maybeToEither (BookNotFound $ Just $ ErrorContext errorMsg)
    errorMsg = [fmt|Could not find book with title '{bookTitle}'|]

hideBooksWithNoCopies :: LibraryEntry -> Either LibrarySysError LibraryEntry
hideBooksWithNoCopies libEntry@(LibraryEntry (Book bookTitle _ _) copiesCount _)
  | copiesCount > 0 = Right libEntry
  | otherwise = Left $ NoCopies $ Just $ ErrorContext [fmt|Sorry, there don't seem to be any more copies of the book {bookTitle} left|]

lookupBookByBookProperty :: (Eq a) => (Book -> a) -> Text -> a -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByBookProperty bookFieldLabel errorMsg bookPropertyToSearchBy = do
  lib <- get
  lift (addErrorMsg errorMsg (List.find bookPropertyLookup (Map.toList lib)) >>= hideBooksWithNoCopies . snd)
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

-- -- Not sure why `stack runghc Solution.hs` isn't working, but using `stack ghci` and running main interactively does the trick
-- main :: IO ()
-- main = do
--     let r = removeBookFromLibrary (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary
--     let g = removeBookFromLibrary (addBook (addBook (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary) firstBookInLibrary) firstBookInLibrary
--     let o = Patron{name = "Ola", cardNumber = 6162633, booksBorrowed = [BorrowedBook firstBookInLibrary]}
--     print (returnBook g firstBookInLibrary o)
