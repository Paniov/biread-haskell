{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Bible 
  ( Book 
  , Chapter
  , lookupBook
  ) where
  
import Control.Lens

data Chapter = Chapter
    { chapterChapter :: Int
    , chapterVerses :: Int
    , chapterParts :: [Int] 
    } deriving (Show)
makeLenses ''Chapter

data Book = Book
    { bookTitle :: String
    , bookGroup :: String
    , bookChapters :: [Chapter] 
    } deriving (Show)
makeLenses ''Book

data QuoteEdge = QuoteEdge
    { _quoteEdgeTitle :: String
    , _quoteEdgeChapter :: Int
    , _quoteEdgeVerse :: Int
    } deriving (Show)
makeLenses ''QuoteEdge

data Quote = Quote
    { _leftEdge :: QuoteEdge
    , _rightEdge :: QuoteEdge
    } deriving (Show)
makeLenses ''Quote

------------------ QuoteEdge Lenses ----------------------------
qeTitleL :: Quote -> String
qeTitleL = view (leftEdge . quoteEdgeTitle)

setQeTitleL :: String -> Quote -> Quote
setQeTitleL = set (leftEdge . quoteEdgeTitle)

qeTitleR :: Quote -> String
qeTitleR = view (rightEdge . quoteEdgeTitle)

setQeTitleR :: String -> Quote -> Quote
setQeTitleR = set (rightEdge . quoteEdgeTitle)

qeChapterL :: Quote -> Int
qeChapterL = view (leftEdge . quoteEdgeChapter)

setQeChapterL :: Int -> Quote -> Quote
setQeChapterL = set (leftEdge . quoteEdgeChapter)

qeChapterR :: Quote -> Int
qeChapterR = view (rightEdge . quoteEdgeChapter)

setQeChapterR :: Int -> Quote -> Quote
setQeChapterR = set (rightEdge . quoteEdgeChapter)

qeVerseL :: Quote -> Int
qeVerseL = view (leftEdge . quoteEdgeVerse)

setQeVerseL :: Int -> Quote -> Quote
setQeVerseL = set (leftEdge . quoteEdgeVerse)

qeVerseR :: Quote -> Int
qeVerseR = view (rightEdge . quoteEdgeVerse)

setQeVerseR :: Int -> Quote -> Quote
setQeVerseR = set (rightEdge . quoteEdgeVerse)

qeTitleEq :: Quote -> Quote -> Bool
qeTitleEq q1 q2 = qeTitleL q1 == qeTitleR q2

qeChapterEq :: Quote -> Quote -> Bool
qeChapterEq q1 q2 = qeChapterL q1 == qeChapterR q2
-------------------------------------------------------------

quoteEdgeOf :: String -> Int -> Int -> QuoteEdge
quoteEdgeOf title chapter verse = QuoteEdge { _quoteEdgeTitle=title, _quoteEdgeChapter=chapter, _quoteEdgeVerse=verse }

quoteStartEnd :: Int -> Int -> [Int] -> [(Int, Int)]
quoteStartEnd start end [] = [(start, end)]
quoteStartEnd start end (p:parts) = (start, p-1) : (quoteStartEnd p end parts)

getChapterQuotes :: String -> Chapter -> [Quote]
getChapterQuotes title (Chapter { chapterChapter=ch, chapterVerses=vs, chapterParts=ps}) = map (quoteOf $ quoteEdgeOf title ch) $ quoteStartEnd 1 vs ps
  where quoteOf f = \(vsL, vsR) -> Quote { _leftEdge = f vsL, _rightEdge = f vsR }

getBookQuotes :: Book -> [Quote]
getBookQuotes (Book { bookTitle=t, bookGroup=gr, bookChapters=chps }) = concat . map (getChapterQuotes t) $ chps

getQuotes :: [Book] -> [Quote]
getQuotes = concat . map getBookQuotes

settersQ :: 
settersQ = (setQeTitleR . qeTitleR) q2 . (setQeChapterR . qeChapterR) q2 . (setQeVerseR . qeVerseR)

glewQuotes :: Quote -> Quote -> Quote
glewQuotes q1 q2 
  | qeTitleEq q1 q2 && qeChapterEq q1 q2 = setQeVerseR (qeVerseR q2) q1
  | qeTitleEq q1 q2 && (not $ qeChapterEq q1 q2) = (setQeChapterR (qeChapterR q2)) . (setQeVerseR (qeVerseR q2)) $ q1
  | otherwise = (settersQ q2) $ q1
  -- | otherwise = (setQeTitleR (qeTitleR q2)) . (setQeChapterR (qeChapterR q2)) . (setQeVerseR (qeVerseR q2)) $ q1

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