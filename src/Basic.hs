module Basic (addBook, removeBook, lookupBookByAuthor, lookupBookByISBN, lookupBookByTitle, displayBookAvailability) where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Helpers

import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import PyF (fmt)
import PyF qualified as Pyf

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

-- ------------------------------ Athenaeum API ----------------------------- --
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
    addNewBookToLibrary = Map.insert newBookTitle LibraryEntry{bookInfo = newBook, copies = 1, borrowedBy = []}

    newBookMsg = [fmt|Added the book '{newBookTitle}' to the library!|]
    newCopyMsg = [fmt|Added a new copy of the book '{newBookTitle}' to the library!|]

lookupBookByTitle :: Text -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByTitle bookTitle = do
    lib <- get
    lift $ addErrorMsg (Map.lookup bookTitle lib) >>= hideBooksWithNoCopies
  where
    addErrorMsg = maybeToEither (BookNotFound $ Just $ ErrorContext errorMsg)
    errorMsg = [fmt|Could not find book with title '{bookTitle}'|]

lookupBookByAuthor :: Text -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByAuthor bookAuthor = let errorMsg = [fmt|Could not find book written by author '{bookAuthor}'|] in lookupBookByBookProperty author errorMsg bookAuthor

lookupBookByISBN :: Text -> StateT Library (Either LibrarySysError) LibraryEntry
lookupBookByISBN bookISBN = let errorMsg = [fmt|Could not find book with ISBN code '{bookISBN}'|] in lookupBookByBookProperty isbn errorMsg bookISBN

removeBook :: Book -> State Library Text
removeBook (Book bookTitle _ _) = do
    currentLibrary <- get
    let bookAvailabilityStatus = evalStateT (lookupBookByTitle bookTitle) currentLibrary

    let (updatedLibrary, msg) = case bookAvailabilityStatus of
            Right _ -> (decrementBookCopiesCount currentLibrary, copyRemovedMsg)
            Left mLibSysError
                | NoCopies _ <- mLibSysError -> (deleteBookFromLibrary currentLibrary, bookDeletedMsg)
                | BookNotFound mContext <- mLibSysError -> (currentLibrary, context $ fromMaybe (ErrorContext bookDeletedMsg) mContext)
                | _ <- mLibSysError -> (currentLibrary, bookDeletedMsg)

    put updatedLibrary
    return msg
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

-- borrow :: Library -> Book -> Patron -> (Patron, Library)
-- borrow lib book borrower = case lookupBookByTitle lib (title book) of
--     Just _ -> (borrower{booksBorrowed = BorrowedBook book : booksBorrowed borrower}, removeBookFromLibrary lib book)
--     Nothing -> (borrower, lib)

-- returnBook :: Library -> Book -> Patron -> (Patron, Library)
-- returnBook lib book borrower =
--     if List.null (booksBorrowed borrower) then (borrower, lib) else (borrower{booksBorrowed = List.filter (\(BorrowedBook bk) -> title book /= title bk) $ booksBorrowed borrower}, addBook lib book)

-- -------------------------- Athenaeum API Helpers ------------------------- --
modifyBookCopyCount :: (Integer -> Integer) -> LibraryEntry -> LibraryEntry
modifyBookCopyCount copyModFn libBookEntry@(LibraryEntry _ currentCopyCount _) = libBookEntry{copies = updatedCopyCount}
  where
    updatedCopyCount = let newBookCopyCount = copyModFn currentCopyCount in max newBookCopyCount 0

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
