module Basic (addBook, removeBook, lookupBookByAuthor, lookupBookByISBN, lookupBookByTitle, displayBookAvailability) where

-- There seems to be a lot of nuance here, specifically with the borrow and return logic. I am not sure the current model suffices as an efficient means to handle all the edge cases without paying an exorbitant price in complexity. Thus, I propose a different model for the library, a nested map with keys referencing the different sections of the libraries capabilities

-- The first would the usual title to libEntry map, the next addition will be a borrow log of sorts. No longer shall patrons need to keep track of the borrowing. Instead it will all be recorded by the library in the second entry. Though this would require that patrons be registered so that a borrow log might be created them for them

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
  deriving (Show, Eq)

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
-- Create a utility to check if a RegisteredPatron borrowed a certain book. Error if so, success if otherwise
borrowBook :: Word -> Day -> Text -> RegisteredPatron -> BasicStack RegisteredPatron
borrowBook randomOffsetForDeadline currentDay bookTitle registeredPatron@(RegisteredPatron (Patron patronName _) patronRegistrationID) = do
  currentLib@(Library currBookShelf currBorrowLog) <- get
  (LibraryEntry bookInfo' _) <- lift $ liftEither $ lookupBookByTitle currBookShelf bookTitle

  let returnDeadline' = calcReturnDeadline currentDay
  let updatedBorrowLog = trackBookBorrowing patronRegistrationID bookInfo' currentDay returnDeadline' currBorrowLog

  put currentLib{getBookShelf = decrementBookCopiesCount bookTitle currBookShelf, getBorrowLog = updatedBorrowLog}

  writer (registeredPatron, [[fmt|{patronName} just borrowed the book '{bookTitle}'. Return deadline is {tshow returnDeadline'}|]])
 where
  trackBookBorrowing borrowerID bookToBorrow borrowDay returnDeadline' =
    let borrowedBook = BorrowedBook{infoOfBorrowedBook = bookToBorrow, returnDeadline = returnDeadline', borrowDate = borrowDay}
     in Map.adjust (Set.insert borrowedBook) borrowerID

  calcReturnDeadline = addDays (toInteger randomOffsetForDeadline)

-- returnBook :: Day -> Book -> Patron -> BasicStack Patron
-- returnBook currentDay borrowedBook@(Book bookTitle _ _) patron@(Patron patronName _ borrowedBooks) = do
--   let maybeBorrowedBook = List.find (\(BorrowedBook bookInfo' _ _) -> bookInfo' == borrowedBook) borrowedBooks
--   (BorrowedBook _ returnDeadline' _) <- lift $ liftEither $ maybeToEither (CustomError (ErrorContext wrongBorrowerMsg)) maybeBorrowedBook

--   borrowedBookLibEntry <- addBook borrowedBook
--   let updatedPatron = unTrackTheBorrowedBook patron borrowedBookLibEntry

--   modify (unTrackTheBorrower bookTitle updatedPatron)

--   let deadlineOffset = diffDays returnDeadline' currentDay

--   when (deadlineOffset < 0) $ throwError $ PastDue $ Just $ ErrorContext [fmt|You are returning this book {abs deadlineOffset} days past due!|]

--   writer (updatedPatron, [[fmt|{patronName} returned the book '{bookTitle}' {deadlineOffset} days before due|]])
--  where
--   unTrackTheBorrower bookTitle' (Patron _ targetBorrowersCardNum _) =
--     let unTrackFn (Patron _ currentPatronCardNum _) = currentPatronCardNum /= targetBorrowersCardNum
--      in Map.adjust (\bookLibEntry@(LibraryEntry _ _ borrowers) -> bookLibEntry{borrowedBy = filter unTrackFn borrowers}) bookTitle'

--   unTrackTheBorrowedBook borrower@(Patron _ _ borrowedBooks') (LibraryEntry bookInfo' _ _) =
--     let filterFn (BorrowedBook borrowedBookInfo _ _) = borrowedBookInfo /= bookInfo'
--      in borrower{booksBorrowed = filter filterFn borrowedBooks'}

--   wrongBorrowerMsg = "Oops, looks like you weren't the one who borrowed this book" :: Text

-- -------------------------- Athenaeum Basic API Helpers ------------------------- --
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

-- bookOne :: Book
-- bookOne = Book{title = "Production Haskell", author = "Matt Parsons", isbn = "ISBN-13: 978-3-16-148410-0"}

-- bookTwo :: Book
-- bookTwo = Book{title = "Simple Haskell", author = "Marco Sampellegrini", isbn = "ISBN-13: 978-0-262-13472-9"}

-- bookThree :: Book
-- bookThree = Book{title = "Optics By Example", author = "Chris Penner", isbn = "ISBN-13: 978-1-566-19237-5"}

-- patronOne :: Patron
-- patronOne = Patron{name = "Michael Williams", cardNumber = 67890, booksBorrowed = []}

-- patronTwo :: Patron
-- patronTwo = Patron{name = "Olivia Wilson", cardNumber = 89012, booksBorrowed = []}

-- library :: Library
-- library = Map.empty

-- programOne :: BasicStack ()
-- programOne = do
--   let firstBookTitle = title bookOne
--   let secondBookTitle = title bookTwo
--   let thirdBookTitle = title bookThree

--   tell [[fmt|Stocking the book {firstBookTitle}...|]]
--   _ <- displayBookAvailability firstBookTitle
--   _ <- displayBookAvailability secondBookTitle
--   _ <- displayBookAvailability thirdBookTitle
--   _ <- addBook bookOne
--   _ <- addBook bookOne
--   _ <- displayBookAvailability firstBookTitle
--   _ <- addBook bookTwo
--   _ <- addBook bookTwo
--   _ <- addBook bookTwo
--   _ <- displayBookAvailability secondBookTitle
--   _ <- addBook bookThree
--   _ <- addBook bookThree
--   _ <- addBook bookThree
--   _ <- addBook bookThree
--   _ <- displayBookAvailability thirdBookTitle
--   _ <- removeBook thirdBookTitle
--   _ <- removeBook firstBookTitle
--   _ <- displayBookAvailability firstBookTitle
--   _ <- displayBookAvailability thirdBookTitle

--   tell ["Book Stocking complete"]

-- programTwo :: Day -> BasicStack ()
-- programTwo today = do
--   let firstBookTitle = title bookOne
--   let secondBookTitle = title bookTwo
--   let thirdBookTitle = title bookThree

--   _ <- censor (const []) programOne
--   _ <- displayBookAvailability firstBookTitle
--   _ <- displayBookAvailability secondBookTitle
--   _ <- displayBookAvailability thirdBookTitle
--   updatedPatronOne <- borrowBook 10 today firstBookTitle patronOne >>= borrowBook 5 today secondBookTitle >>= returnBook today bookOne
--   tell [tshow updatedPatronOne]

-- main :: IO ()
-- main = do
--   print "Running Program 1...."
--   let programOneOutput = let program = evalState (runExceptT (runWriterT programOne)) library in either getLibSysErrorContext (intercalate "\n" . snd) program
--   TIO.putStrLn programOneOutput
--   TIO.putStrLn "Program 1 complete\n\n"

--   today <- getDay
--   print "Running program 2...."
--   let (programTwoOutput, finalLibState) = runState (runExceptT (runWriterT $ programTwo today)) library
--   print finalLibState

--   let serializedProgramTwoOutput = either getLibSysErrorContext (intercalate "\n" . snd) programTwoOutput
--   TIO.putStrLn serializedProgramTwoOutput

-- TIO.putStrLn "Program 2 complete\n\n"

-- -- Not sure why `stack runghc Solution.hs` isn't working, but using `stack ghci` and running main interactively does the trick
-- main :: IO ()
-- main = do
--     let r = removeBookFromLibrary (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary
--     let g = removeBookFromLibrary (addBook (addBook (addBook libraryFirstBranch firstBookInLibrary) firstBookInLibrary) firstBookInLibrary) firstBookInLibrary
--     let o = Patron{name = "Ola", cardNumber = 6162633, booksBorrowed = [BorrowedBook firstBookInLibrary]}
--     print (returnBook g firstBookInLibrary o)
