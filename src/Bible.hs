{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bible 
  ( Book 
  , Chapter
  , lookupBook
  ) where
  
import Control.Lens

data Chapter = Chapter
    { _chChapter :: Int
    , _chVerses :: Int
    , _chParts :: [Int] 
    } deriving (Show)
makeLenses ''Chapter

data Book = Book
    { _bTitle :: String
    , _bGroup :: String
    , _bChapters :: [Chapter] 
    } deriving (Show)
makeLenses ''Book

data QuoteEdge = QuoteEdge
    { _qeTitle :: String
    , _qeChapter :: Int
    , _qeVerse :: Int
    } deriving (Show)
makeLenses ''QuoteEdge

data Quote = Quote
    { _leftEdge :: QuoteEdge
    , _rightEdge :: QuoteEdge
    } deriving (Show)
makeLenses ''Quote

------------------ QuoteEdge Lenses ----------------------------

--------- Title set ----------
leftTitltePath :: Functor f => (String -> f String) -> Quote -> f Quote
leftTitltePath = leftEdge . qeTitle

getQeTitleL :: Quote -> String
getQeTitleL = view leftTitltePath

setQeTitleL :: String -> Quote -> Quote
setQeTitleL = set leftTitltePath

rightTitltePath :: Functor f => (String -> f String) -> Quote -> f Quote
rightTitltePath = rightEdge . qeTitle

getQeTitleR :: Quote -> String
getQeTitleR = view rightTitltePath

setQeTitleR :: String -> Quote -> Quote
setQeTitleR = set rightTitltePath

mergeQeTitleR :: Quote -> Quote -> Quote
mergeQeTitleR = setQeTitleR . getQeTitleR

--------- Chapter set ----------

leftChapterPath :: Functor f => (Int -> f Int) -> Quote -> f Quote
leftChapterPath = leftEdge . qeChapter

getQeChapterL :: Quote -> Int
getQeChapterL = view leftChapterPath

setQeChapterL :: Int -> Quote -> Quote
setQeChapterL = set leftChapterPath

rightChapterPath :: Functor f => (Int -> f Int) -> Quote -> f Quote
rightChapterPath = rightEdge . qeChapter

getQeChapterR :: Quote -> Int
getQeChapterR = view rightChapterPath

setQeChapterR :: Int -> Quote -> Quote
setQeChapterR = set rightChapterPath

mergeQeChapterR :: Quote -> Quote -> Quote
mergeQeChapterR = setQeChapterR . getQeChapterR

--------- Verse set -----------

leftVersePath :: Functor f => (Int -> f Int) -> Quote -> f Quote
leftVersePath = leftEdge . qeVerse

getQeVerseL :: Quote -> Int
getQeVerseL = view leftVersePath

setQeVerseL :: Int -> Quote -> Quote
setQeVerseL = set leftVersePath

rightVersePath :: Functor f => (Int -> f Int) -> Quote -> f Quote
rightVersePath = rightEdge . qeVerse

getQeVerseR :: Quote -> Int
getQeVerseR = view rightVersePath

setQeVerseR :: Int -> Quote -> Quote
setQeVerseR = set rightVersePath

mergeQeVerseR :: Quote -> Quote -> Quote
mergeQeVerseR = setQeVerseR . getQeVerseR

qeTitleEq :: Quote -> Quote -> Bool
qeTitleEq q1 q2 = getQeTitleL q1 == getQeTitleR q2

qeChapterEq :: Quote -> Quote -> Bool
qeChapterEq q1 q2 = getQeChapterL q1 == getQeChapterR q2

mergeQuoteOf :: Foldable t => a -> t (a -> b -> b) -> b -> b
mergeQuoteOf = \q2 -> foldr ((.).($ q2)) id

settersQeRightVs :: [(Quote -> Quote -> Quote)]
settersQeRightVs = [mergeQeVerseR]

settersQeRightChVs :: [(Quote -> Quote -> Quote)]
settersQeRightChVs = [mergeQeChapterR, mergeQeVerseR]

settersQeRight :: [(Quote -> Quote -> Quote)]
settersQeRight = [mergeQeTitleR, mergeQeChapterR, mergeQeVerseR]

-------------------------------------------------------------

quoteStartEnd :: Int -> Int -> [Int] -> [(Int, Int)]
quoteStartEnd start end [] = [(start, end)]
quoteStartEnd start end (p:parts) = (start, p-1) : (quoteStartEnd p end parts)

getChapterQuotes :: String -> Chapter -> [Quote]
getChapterQuotes title ch = map (quoteOf $ QuoteEdge title (ch^.chChapter)) $ quoteStartEnd 1 (ch^.chVerses) (ch^.chParts)
  where quoteOf f = \(vsL, vsR) -> Quote (f vsL) (f vsR)

getBookQuotes :: Book -> [Quote]
getBookQuotes book = concat . map (getChapterQuotes $ book^.bTitle ) $ book^.bChapters 

getQuotes :: [Book] -> [Quote]
getQuotes = concat . map getBookQuotes

glewQuotes :: Quote -> Quote -> Quote
glewQuotes q1 q2 = mergeQuoteOf q2 quoteSetters q1 where
  quoteSetters
    | qeTitleEq q1 q2 && qeChapterEq q1 q2 = settersQeRightVs
    | qeTitleEq q1 q2 && (not $ qeChapterEq q1 q2) = settersQeRightChVs
    | otherwise = settersQeRight

glewQuotesAt :: Int -> [Quote] -> [Quote]
glewQuotesAt dayOfYear qs = (\(left, right) -> (init left) ++ [glewQuotes (last left) (head right)] ++ (tail right)) $ (splitAt dayOfYear qs)

getQuotesNormalYear :: [Book] -> [Quote]
getQuotesNormalYear = getQuotes

getQuotesLeapYear :: Int -> [Book] -> [Quote]
getQuotesLeapYear dayOfYear = (glewQuotesAt dayOfYear) . getQuotes

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
  [ Book "Matthew" "Gospel" matthewChapters
  , Book "Mark"    "Gospel" markChapters
  , Book "Luke"    "Gospel" markChapters
  , Book "John"    "Gospel" markChapters
  ]

lookupBook :: [Book] -> String -> Maybe Book
lookupBook [] _ = Nothing
lookupBook (b:[]) value
  | _bTitle b == value = Just b
  | otherwise = Nothing

lookupBook (b:bs) value
  | _bTitle b == value = Just b
  | otherwise = lookupBook bs value