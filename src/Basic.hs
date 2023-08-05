{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Basic (
  addBook,
  removeBook,
  lookupBookByAuthor,
  lookupBookByISBN,
  lookupBookByTitle,
  displayBookAvailability,
  borrowBook,
  returnBook,
  registerPatron,
) where

import Control.Error.Util (note)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.State
import Control.Monad.Writer
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, intercalate)
import Data.Text.IO qualified as TIO
import Data.Time
import Data.UUID (UUID)
import Helpers
import PyF (fmt)

type RegistrationID = UUID

data Book = Book
  { title :: Text
  , author :: Text
  , isbn :: Text
  }
  deriving (Show, Eq, Ord)

data BorrowedBook = BorrowedBook
  { infoOfBorrowedBook :: Book
  , returnDeadline :: Day
  , borrowDate :: Day
  }
  deriving (Show, Eq, Ord)

data Patron = Patron
  { name :: Text
  , idNumber :: Word
  }
  deriving (Show, Eq)

data RegisteredPatron = RegisteredPatron
  { patronInfo :: Patron
  , registrationID :: RegistrationID
  }
  deriving (Eq, Show)

data LibraryEntry = LibraryEntry
  { bookInfo :: Book
  , copies :: Word
  }
  deriving (Show, Eq)

newtype ErrorContext = ErrorContext {context :: Text} deriving (Eq, Show)

type OptionalErrorContext = Maybe ErrorContext

data LibrarySysError = NoCopies OptionalErrorContext | BookNotFound OptionalErrorContext | PastDue OptionalErrorContext | CustomError ErrorContext
  deriving (Eq, Show)

type BookShelf = Map Text LibraryEntry
type BorrowLog = Map RegistrationID (Set BorrowedBook)

data Library = Library {getBookShelf :: BookShelf, getBorrowLog :: BorrowLog} deriving (Show, Eq)

type BasicStack a = WriterT [Text] (ExceptT LibrarySysError (State Library)) a

-- ------------------------------ Athenaeum Basic API ----------------------------- --
addBook :: Book -> BasicStack LibraryEntry
addBook newBook@(Book newBookTitle _ _) = do
  currentLibrary@(Library currentBookShelf _) <- get
  let bookAvailabilityStatus = lookupBookByTitle currentBookShelf newBookTitle

  let (updatedBookShelf, msg, bookLibEntry) = case bookAvailabilityStatus of
        Right bookLibEntry' -> (incrementBookCopiesCount newBookTitle currentBookShelf, newCopyMsg, bookLibEntry')
        Left _ -> let (updatedBookShelf', newLibEntry) = addNewBookToLibrary currentBookShelf in (updatedBookShelf', newBookMsg, newLibEntry)

  put currentLibrary{getBookShelf = updatedBookShelf}

  writer (bookLibEntry, [msg])
 where
  addNewBookToLibrary currentLib = let newLibEntry = LibraryEntry{bookInfo = newBook, copies = 1} in (Map.insert newBookTitle newLibEntry currentLib, newLibEntry)

  newBookMsg = [fmt|Added the book '{newBookTitle}' to the library!|]
  newCopyMsg = [fmt|Added a new copy of the book '{newBookTitle}' to the library!|]

lookupBookByTitle :: BookShelf -> Text -> Either LibrarySysError LibraryEntry
lookupBookByTitle bookShelf bookTitle = lookupBookByTitleIncludingBooksWithoutCopies bookTitle bookShelf >>= hideBooksWithNoCopies

lookupBookByAuthor :: BookShelf -> Text -> Either LibrarySysError LibraryEntry
lookupBookByAuthor bookShelf bookAuthor = let errorMsg = [fmt|Could not find book written by author '{bookAuthor}'|] in lookupBookByBookProperty bookShelf author errorMsg bookAuthor

lookupBookByISBN :: BookShelf -> Text -> Either LibrarySysError LibraryEntry
lookupBookByISBN bookShelf bookISBN = let errorMsg = [fmt|Could not find book with ISBN code '{bookISBN}'|] in lookupBookByBookProperty bookShelf isbn errorMsg bookISBN

removeBook :: Text -> BasicStack (Maybe LibraryEntry)
removeBook bookTitle = do
  currentLibrary@(Library currentBookShelf _) <- get
  bookLibEntry@(LibraryEntry _ copiesCount) <- lift $ liftEither $ lookupBookByTitleIncludingBooksWithoutCopies bookTitle currentBookShelf

  let (updatedBookShelf, msg, maybeBookLibEntry) =
        if copiesCount > 0
          then (decrementBookCopiesCount bookTitle currentBookShelf, copyRemovedMsg, Just bookLibEntry)
          else (deleteBookFromLibrary currentBookShelf, bookDeletedMsg, Nothing)

  put currentLibrary{getBookShelf = updatedBookShelf}
  writer (maybeBookLibEntry, [msg])
 where
  deleteBookFromLibrary = Map.delete bookTitle

  copyRemovedMsg = [fmt|Removed a copy of the book '{bookTitle}'|]
  bookDeletedMsg = [fmt|Deleted the book '{bookTitle}' from library|]

displayBookAvailability :: Text -> BasicStack ()
displayBookAvailability bookTitle = do
  (Library bookShelf _) <- get
  let bookAvailabilityStatus = lookupBookByTitleIncludingBooksWithoutCopies bookTitle bookShelf
  let copyTense copiesCount = if copiesCount == 1 then "copy" else "copies"

  let bookAvailabilityMsg = case bookAvailabilityStatus of
        Right (LibraryEntry _ copiesCount) -> [fmt|We currently have {copiesCount} {(copyTense copiesCount) :: Text} of the book '{bookTitle}'|]
        Left (NoCopies _) -> [fmt|Sorry, it looks like all copies of the book, '{bookTitle}' have either been borrowed or deleted|]
        Left (BookNotFound _) -> [fmt|Sorry, it looks like the book, '{bookTitle}' does not exist, perhaps it was removed or not yet added|]
        Left (CustomError (ErrorContext errContext)) -> [fmt|The following error occurred when attempting to display the availability of the book '{bookTitle}': {errContext}|]
        Left _ -> "An error Occurred"

  tell [bookAvailabilityMsg]

registerPatron :: Patron -> BasicStack RegisteredPatron
registerPatron patronToBeRegistered@(Patron patronName patronIdNumber) = do
  currentLib@(Library _ borrowLog) <- get
  let generatedRegistrationID = randomUUID patronIdNumber

  if isAlreadyRegistered generatedRegistrationID borrowLog
    then tell [[fmt|{patronName} has already been registered|]]
    else do
      let updatedBorrowLog = Map.insert generatedRegistrationID Set.empty borrowLog
      put currentLib{getBorrowLog = updatedBorrowLog}
      tell [[fmt|{patronName} has been registered|]]

  return RegisteredPatron{patronInfo = patronToBeRegistered, registrationID = generatedRegistrationID}
 where
  isAlreadyRegistered registeredPatronId = isJust . Map.lookup registeredPatronId

-- You can't borrow the same book twice
borrowBook :: Word -> Day -> Text -> RegisteredPatron -> BasicStack RegisteredPatron
borrowBook randomOffsetForDeadline currentDay bookTitle registeredPatron@(RegisteredPatron (Patron patronName _) patronRegistrationID) = do
  currentLib@(Library currentBookShelf currentBorrowLog) <- get

  let alreadyBorrowedErrorMsg = [fmt|{patronName} has already borrowed the book '{bookTitle}'|]
  when (hasAlreadyBorrowedBook currentLib bookTitle registeredPatron) $ throwError $ PastDue $ Just $ ErrorContext alreadyBorrowedErrorMsg

  (LibraryEntry bookInfo' _) <- lift $ liftEither $ lookupBookByTitle currentBookShelf bookTitle

  let returnDeadline' = calcReturnDeadline currentDay
  let updatedBorrowLog = trackBookBorrowing patronRegistrationID bookInfo' currentDay returnDeadline' currentBorrowLog

  put currentLib{getBookShelf = decrementBookCopiesCount bookTitle currentBookShelf, getBorrowLog = updatedBorrowLog}

  writer (registeredPatron, [[fmt|{patronName} just borrowed the book '{bookTitle}'. Return deadline is {tshow returnDeadline'}|]])
 where
  trackBookBorrowing borrowerID bookToBorrow borrowDay returnDeadline' =
    let borrowedBook = BorrowedBook{infoOfBorrowedBook = bookToBorrow, returnDeadline = returnDeadline', borrowDate = borrowDay}
     in Map.adjust (Set.insert borrowedBook) borrowerID

  calcReturnDeadline = addDays (toInteger randomOffsetForDeadline)

returnBook :: Day -> Book -> RegisteredPatron -> BasicStack RegisteredPatron
returnBook currentDay borrowedBook@(Book bookTitle _ _) registeredPatron@(RegisteredPatron (Patron patronName _) patronRegistrationID) = do
  currentLib@(Library _ currentBorrowLog) <- get

  let wrongBorrowerError = CustomError $ ErrorContext [fmt|{patronName} has not borrowed the book '{bookTitle}'|]
  (BorrowedBook _ returnDeadline' _) <- lift $ liftEither $ note wrongBorrowerError (getBorrowedBook currentBorrowLog borrowedBook registeredPatron)

  let updatedBorrowLog = unTrackBookBorrowing patronRegistrationID borrowedBook currentBorrowLog
  _ <- censor (const []) (addBook borrowedBook)

  updatedBookShelf <- gets getBookShelf
  put currentLib{getBookShelf = updatedBookShelf, getBorrowLog = updatedBorrowLog}

  let deadlineOffset = diffDays returnDeadline' currentDay
  when (deadlineOffset < 0) $ throwError $ PastDue $ Just $ ErrorContext [fmt|You are returning this book {abs deadlineOffset} days past due!|]

  writer (registeredPatron, [[fmt|{patronName} returned the book '{bookTitle}' {deadlineOffset} days before due|]])
 where
  unTrackBookBorrowing borrowerID bookToReturn =
    let filterFnForBookToReturn (BorrowedBook aBorrowedBook _ _) = aBorrowedBook /= bookToReturn
     in Map.adjust (Set.filter filterFnForBookToReturn) borrowerID

  getBorrowedBook borrowLog targetBook (RegisteredPatron _ registrationID') =
    let patronsBorrowedBooks = Set.toList $ Map.findWithDefault Set.empty registrationID' borrowLog
     in List.find (\(BorrowedBook aBorrowedBook _ _) -> aBorrowedBook == targetBook) patronsBorrowedBooks

-- -------------------------- Athenaeum Basic API Helpers ------------------------- --
hasAlreadyBorrowedBook :: Library -> Text -> RegisteredPatron -> Bool
hasAlreadyBorrowedBook (Library _ borrowLog) bookTitle (RegisteredPatron _ registrationID') =
  let patronsBorrowedBooks = Map.findWithDefault Set.empty registrationID' borrowLog
   in Set.member bookTitle $ Set.map (title . infoOfBorrowedBook) patronsBorrowedBooks

incrementBookCopiesCount :: Text -> BookShelf -> BookShelf
incrementBookCopiesCount = Map.adjust (modifyBookCopyCount (+ 1))

decrementBookCopiesCount :: Text -> BookShelf -> BookShelf
decrementBookCopiesCount = Map.adjust (modifyBookCopyCount (subtract 1))

modifyBookCopyCount :: (Word -> Word) -> LibraryEntry -> LibraryEntry
modifyBookCopyCount copyModFn libBookEntry@(LibraryEntry _ currentCopyCount) = libBookEntry{copies = updatedCopyCount}
 where
  updatedCopyCount = let newBookCopyCount = copyModFn currentCopyCount in max newBookCopyCount 0

lookupBookByTitleIncludingBooksWithoutCopies :: Text -> BookShelf -> Either LibrarySysError LibraryEntry
lookupBookByTitleIncludingBooksWithoutCopies bookTitle = addErrorMsg . Map.lookup bookTitle
 where
  addErrorMsg = note (BookNotFound $ Just $ ErrorContext errorMsg)
  errorMsg = [fmt|Could not find book with title '{bookTitle}'|]

hideBooksWithNoCopies :: LibraryEntry -> Either LibrarySysError LibraryEntry
hideBooksWithNoCopies libEntry@(LibraryEntry (Book bookTitle _ _) copiesCount)
  | copiesCount > 0 = Right libEntry
  | otherwise = Left $ NoCopies $ Just $ ErrorContext [fmt|Sorry, there don't seem to be any more copies of the book {bookTitle} left|]

lookupBookByBookProperty :: (Eq a) => BookShelf -> (Book -> a) -> Text -> a -> Either LibrarySysError LibraryEntry
lookupBookByBookProperty bookShelf bookFieldLabel errorMsg bookPropertyToSearchBy =
  addErrorMsg errorMsg (List.find bookPropertyLookup (Map.toList bookShelf)) >>= hideBooksWithNoCopies . snd
 where
  bookPropertyLookup (_, LibraryEntry bookInfo' _) = bookPropertyToSearchBy == bookFieldLabel bookInfo'
  addErrorMsg = note . BookNotFound . Just . ErrorContext

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

bookOne :: Book
bookOne = Book{title = "Production Haskell", author = "Matt Parsons", isbn = "ISBN-13: 978-3-16-148410-0"}

bookTwo :: Book
bookTwo = Book{title = "Simple Haskell", author = "Marco Sampellegrini", isbn = "ISBN-13: 978-0-262-13472-9"}

bookThree :: Book
bookThree = Book{title = "Optics By Example", author = "Chris Penner", isbn = "ISBN-13: 978-1-566-19237-5"}

patronOne :: Patron
patronOne = Patron{name = "Michael Williams", idNumber = 67890}

patronTwo :: Patron
patronTwo = Patron{name = "Olivia Wilson", idNumber = 89012}

patronThree :: Patron
patronThree = Patron{name = "Jessica Horn", idNumber = 89012}

library :: Library
library = Library{getBookShelf = Map.empty, getBorrowLog = Map.empty}

setup :: BasicStack (RegisteredPatron, RegisteredPatron, RegisteredPatron)
setup = do
  let firstBookTitle = title bookOne
  let secondBookTitle = title bookTwo
  let thirdBookTitle = title bookThree

  tell [[fmt|Stocking the book {firstBookTitle}...|]]
  _ <- displayBookAvailability firstBookTitle
  _ <- displayBookAvailability secondBookTitle
  _ <- displayBookAvailability thirdBookTitle
  _ <- addBook bookOne
  _ <- addBook bookOne
  _ <- displayBookAvailability firstBookTitle
  _ <- addBook bookTwo
  _ <- addBook bookTwo
  _ <- addBook bookTwo
  _ <- displayBookAvailability secondBookTitle
  _ <- addBook bookThree
  _ <- addBook bookThree
  _ <- addBook bookThree
  _ <- addBook bookThree
  _ <- displayBookAvailability thirdBookTitle
  _ <- removeBook thirdBookTitle
  _ <- removeBook firstBookTitle
  _ <- displayBookAvailability firstBookTitle
  _ <- displayBookAvailability thirdBookTitle
  _ <- addBook bookOne
  _ <- addBook bookTwo
  _ <- addBook bookThree

  tell ["Book Stocking complete", "Registering Patrons..."]

  registeredPatrons <- traverseTupleThree registerPatron (patronOne, patronTwo, patronThree)

  tell ["Patron registration complete!"]

  return registeredPatrons

program :: Day -> BasicStack ()
program today = do
  let firstBookTitle = title bookOne
  let secondBookTitle = title bookTwo
  let thirdBookTitle = title bookThree

  (registeredPatronOne, _, _) <- setup
  _ <- displayBookAvailability firstBookTitle
  _ <- displayBookAvailability secondBookTitle
  _ <- displayBookAvailability thirdBookTitle

  updatedRegisteredPatronOne <- borrowBook 10 today firstBookTitle registeredPatronOne >>= borrowBook 5 today secondBookTitle
  tell []

main :: IO ()
main = do
  today <- getDay
  let (programOutput, finalLibState) = runState (runExceptT (runWriterT $ program today)) library

  let serializedProgramTwoOutput = either getLibSysErrorContext (intercalate "\n" . snd) programOutput
  TIO.putStrLn serializedProgramTwoOutput
  TIO.putStrLn "Program complete\n\n"
  print finalLibState
