{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Bible 
  ( Book 
  , Chapter
  , lookupBook
  ) where

data Chapter = Chapter
    { chapterChapter :: Int
    , chapterVerses :: Int
    , chapterParts :: [Int] 
    } deriving (Show)

data Book = Book
    { bookTitle :: String
    , bookGroup :: String
    , bookChapters :: [Chapter] 
    } deriving (Show)

data Quote = Quote
    { quoteTitle :: String
    , quoteChapter :: Int
    , quoteVerseStart :: Int
    , quoteVerseEnd :: Int
    } deriving (Show)

quoteStartEnd :: Int -> Int -> [Int] -> [(Int, Int)]
quoteStartEnd start end [] = [(start, end)]
quoteStartEnd start end (p:parts) = (start, p-1) : (quoteStartEnd p end parts)

getChapterQuotes :: String -> Chapter -> [Quote]
getChapterQuotes title (Chapter { chapterChapter=ch, chapterVerses=vs, chapterParts=ps}) = 
  map (\tpl -> Quote { quoteTitle=title, quoteChapter=ch, quoteVerseStart=(fst tpl), quoteVerseEnd=(snd tpl) }) $ quoteStartEnd 1 vs ps

getBookQuotes :: Book -> [Quote]
getBookQuotes (Book { bookTitle=t, bookGroup=gr, bookChapters=chps }) = concat . map (getChapterQuotes t) $ chps

getQuotes :: [Book] -> [Quote]
getQuotes = concat . map getBookQuotes

glewQuotes :: Quote -> Quote -> Quote
glewQuotes (Quote { quoteTitle=tL, quoteChapter=chL, quoteVerseStart=startL, quoteVerseEnd=endL }) (Quote { quoteTitle=tR, quoteChapter=chR, quoteVerseStart=startR, quoteVerseEnd=endR }) = 
  Quote { quoteTitle=tL, quoteChapter=chL, quoteVerseStart=startL, quoteVerseEnd=endR }

glewQuotesAt :: Int -> [Quote] -> [Quote]
glewQuotesAt dayOfYear qs = (\(left, right) -> (init left) ++ [glewQuotes (last left) (head right)] ++ (tail right)) $ (splitAt dayOfYear qs)

getQuotesNormalYear :: [Book] -> [Quote]
getQuotesNormalYear = getQuotes

getQuotesLeapYear :: Int -> [Book] -> [Quote]
getQuotesLeapYear dayOfYear = (glewQuotesAt dayOfYear) . getQuotes

bookOf :: String -> String -> [Chapter] -> Book
bookOf t g p = Book { bookTitle=t, bookGroup=g, bookChapters=p }

matthewChapters :: [Chapter]
matthewChapters = 
  [ Chapter 1 25 []
  , Chapter 2 23 []
  , Chapter 3 17 []
  , Chapter 4 25 []
  , Chapter 5 48 [27]
  , Chapter 6 34 [19]
  , Chapter 7 29 []
  , Chapter 8 34 [18]
  , Chapter 9 38 [18]
  , Chapter 10 42 [21]
  , Chapter 11 30 []
  , Chapter 12 50 [24]
  , Chapter 13 58 [31]
  , Chapter 14 36 [22]
  , Chapter 15 39 [21]
  , Chapter 16 28 []
  , Chapter 17 27 []
  , Chapter 18 35 [21]
  , Chapter 19 30 []
  , Chapter 20 34 [17]
  , Chapter 21 46 [23]
  , Chapter 22 46 [23]
  , Chapter 23 39 [23]
  , Chapter 24 51 [29]
  , Chapter 25 46 [31]
  , Chapter 26 75 [26, 51]
  , Chapter 27 66 [27, 51]
  , Chapter 28 20 []
  ]

markChapters :: [Chapter]
markChapters = 
  [ (Chapter 15 25 [])
  , (Chapter 16 23 [])
  ]

booksNT :: [Book]
booksNT = 
  [ bookOf "Matthew" "Gospel" matthewChapters
  , bookOf "Mark"    "Gospel" markChapters
  , bookOf "Luke"    "Gospel" markChapters
  , bookOf "John"    "Gospel" markChapters
  ]

lookupBook :: [Book] -> String -> Maybe Book
lookupBook [] _ = Nothing
lookupBook (b:[]) value
  | bookTitle b == value = Just b
  | otherwise = Nothing

lookupBook (b:bs) value
  | bookTitle b == value = Just b
  | otherwise = lookupBook bs value



