module Basic (addBook, removeBook, lookupBookByAuthor, lookupBookByISBN, lookupBookByTitle, displayBookAvailability, borrowBook, returnBook) where

import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.State
import Control.Monad.Writer
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, intercalate)
import Data.Text.IO qualified as TIO
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

type BasicStack a = WriterT [Text] (ExceptT LibrarySysError (State Library)) a

-- ------------------------------ Athenaeum Basic API ----------------------------- --
addBook :: Book -> BasicStack LibraryEntry
addBook newBook@(Book newBookTitle _ _) = do
  currentLibrary <- get
  let bookAvailabilityStatus = lookupBookByTitle currentLibrary newBookTitle

  let (updatedLibrary, msg, bookLibEntry) = case bookAvailabilityStatus of
        Right bookLibEntry' -> (incrementBookCopiesCount currentLibrary newBookTitle, newCopyMsg, bookLibEntry')
        Left _ -> let (updatedLib, newLibEntry) = addNewBookToLibrary currentLibrary in (updatedLib, newBookMsg, newLibEntry)

  put updatedLibrary

  writer (bookLibEntry, [msg])
  where
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
          then (decrementBookCopiesCount currentLibrary bookTitle, copyRemovedMsg, Just bookLibEntry)
          else (deleteBookFromLibrary currentLibrary, bookDeletedMsg, Nothing)

  put updatedLibrary
  writer (maybeBookLibEntry, [msg])
  where
    deleteBookFromLibrary = Map.delete bookTitle

    copyRemovedMsg = [fmt|Removed a copy of the book '{bookTitle}'|]
    bookDeletedMsg = [fmt|Deleted the book '{bookTitle}' from library|]

displayBookAvailability :: Text -> BasicStack ()
displayBookAvailability bookTitle = do
  lib <- get
  let bookAvailabilityStatus = lookupBookByTitleIncludingBooksWithoutCopies lib bookTitle
  let copyTense copiesCount = if copiesCount == 1 then "copy" else "copies"

  let bookAvailabilityMsg = case bookAvailabilityStatus of
        Right (LibraryEntry _ copiesCount _) -> [fmt|We currently have {copiesCount} {(copyTense copiesCount) :: Text} of the book '{bookTitle}'|]
        Left (NoCopies _) -> [fmt|Sorry, it looks like all copies of the book, '{bookTitle}' have either been borrowed or deleted|]
        Left (BookNotFound _) -> [fmt|Sorry, it looks like the book, '{bookTitle}' does not exist, perhaps it was removed or not yet added|]
        Left (CustomError (ErrorContext errContext)) -> [fmt|The following error occurred when attempting to display the availability of the book '{bookTitle}': {errContext}|]
        Left _ -> "An error Occurred"

  tell [bookAvailabilityMsg]

borrowBook :: (Day, Integer) -> Text -> Patron -> BasicStack Patron
borrowBook (currentDay, randomOffsetForDeadline) bookTitle patron = do
  currentLib <- get
  libraryEntry <- lift $ liftEither $ (`lookupBookByTitle` bookTitle) $ decrementBookCopiesCount currentLib bookTitle

  modify (`trackTheBorrower` libraryEntry)
  let updatedPatron = trackTheBorrowedBook patron libraryEntry

  writer (updatedPatron, [outputMsg])
  where
    trackTheBorrower lib bookLibEntry@(LibraryEntry (Book bookTitle' _ _) _ patrons) =
      Map.adjust (const $ bookLibEntry {borrowedBy = patron : patrons}) bookTitle' lib

    trackTheBorrowedBook patron'@(Patron _ _ borrowedBooks') (LibraryEntry bookInfo' _ _) =
      patron'
        { booksBorrowed = (BorrowedBook {infoOfBorrowedBook = bookInfo', borrowDate = currentDay, returnDeadline = returnDeadline'}) : borrowedBooks'
        }

    returnDeadline' = addDays randomOffsetForDeadline currentDay
    outputMsg = [fmt|Thanks! You just borrowed the book '{bookTitle}'. Your return deadline is {tshow returnDeadline'}|]

returnBook :: Day -> Patron -> Book -> BasicStack Patron
returnBook currentDay patron@(Patron patronName _ borrowedBooks) borrowedBook@(Book bookTitle _ _) = do
  let maybeBorrowedBook = List.find (\(BorrowedBook bookInfo' _ _) -> bookInfo' == borrowedBook) borrowedBooks
  (BorrowedBook _ returnDeadline' _) <- lift $ liftEither $ maybeToEither (CustomError (ErrorContext wrongBorrowerMsg)) maybeBorrowedBook

  borrowedBookLibEntry <- addBook borrowedBook
  modify (`unTrackTheBorrower` borrowedBookLibEntry)

  let updatedPatron = unTrackTheBorrowedBook patron borrowedBookLibEntry
  let deadlineOffset = diffDays returnDeadline' currentDay

  when (deadlineOffset < 0) $ throwError $ PastDue $ Just $ ErrorContext [fmt|You are returning this book {abs deadlineOffset} days past due!|]

  writer (updatedPatron, [[fmt|Thanks {patronName}! You just returned the book '{bookTitle}'|]])
  where
    unTrackTheBorrower lib bookLibEntry@(LibraryEntry (Book bookTitle' _ _) _ patrons) =
      Map.adjust (const $ bookLibEntry {borrowedBy = filter (/= patron) patrons}) bookTitle' lib

    unTrackTheBorrowedBook (Patron _ _ borrowedBooks') (LibraryEntry bookInfo' _ _) =
      let filterFn (BorrowedBook borrowedBookInfo _ _) = borrowedBookInfo /= bookInfo'
       in patron {booksBorrowed = filter filterFn borrowedBooks'}

    wrongBorrowerMsg = "Oops, looks like you weren't the one who borrowed this book" :: Text

-- -------------------------- Athenaeum Basic API Helpers ------------------------- --
incrementBookCopiesCount :: Library -> Text -> Library
incrementBookCopiesCount lib bookTitle = Map.adjust (modifyBookCopyCount (+ 1)) bookTitle lib

decrementBookCopiesCount :: Library -> Text -> Library
decrementBookCopiesCount lib bookTitle = Map.adjust (modifyBookCopyCount (subtract 1)) bookTitle lib

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

getLibSysErrorContext :: LibrarySysError -> Text
getLibSysErrorContext = \case
  NoCopies mErrContext -> foo mErrContext ""
  PastDue mErrContext -> foo mErrContext ""
  BookNotFound mErrContext -> foo mErrContext ""
  CustomError (ErrorContext context') -> context'
  where
    foo maybeErrContext defaultMsg = case maybeErrContext of
      Just (ErrorContext context') -> context'
      Nothing -> defaultMsg

-- ---------------------------------- Main ---------------------------------- --
getDay :: IO Day
getDay = utctDay <$> getCurrentTime

firstBookInLibrary :: Book
firstBookInLibrary = Book {title = "Production Haskell", author = "Matt Parsons", isbn = "ISBN-13: 978-3-16-148410-0"}

library :: Library
library = Map.empty

program1 :: BasicStack ()
program1 = do
  let firstBookTitle = title firstBookInLibrary
  tell [[fmt|Stocking the book {firstBookTitle}...|]]
  _ <- displayBookAvailability firstBookTitle
  _ <- addBook firstBookInLibrary
  _ <- displayBookAvailability firstBookTitle
  _ <- addBook firstBookInLibrary
  _ <- addBook firstBookInLibrary
  _ <- displayBookAvailability firstBookTitle
  _ <- addBook firstBookInLibrary
  _ <- displayBookAvailability firstBookTitle
  _ <- removeBook firstBookTitle
  _ <- removeBook firstBookTitle
  _ <- displayBookAvailability firstBookTitle
  tell ["Book Stocking complete"]

main :: IO ()
main = do
  print "Running Program 1...."
  let program = evalState (runExceptT (runWriterT program1)) library
  let programOutput = either getLibSysErrorContext (intercalate "\n" . snd) program
  TIO.putStrLn programOutput
  print "Program 1 complete"

-- -- Not sure why `stack runghc Solution.hs` isn't working, but using `stack ghci` and running main interactively does the trick
-- main :: IO ()
-- main = do
--     let r = removeBookFromLibrary (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary
--     let g = removeBookFromLibrary (addBook (addBook (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary) firstBookInLibrary) firstBookInLibrary
--     let o = Patron{name = "Ola", cardNumber = 6162633, booksBorrowed = [BorrowedBook firstBookInLibrary]}
--     print (returnBook g firstBookInLibrary o)
