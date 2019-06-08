{-# LANGUAGE NamedFieldPuns #-}

module Bible 
  ( Book 
  , Chapter
  ) where

data Chapter = Chapter { chapter :: Int
                       , verses :: Int
                       , parts :: [Int] 
                       } deriving (Show)

data Book = Book { title :: String
                 , group :: String
                 , chapters :: [Chapter] 
                 } deriving (Show)

bookOf :: String -> String -> [Chapter] -> Book
bookOf t g p = Book { title=t, group=g, chapters=p }

matthewChapters :: [Chapter]
matthewChapters = 
  [ (Chapter 1 25 [])
  , (Chapter 2 23 [])
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
  | title b == value = Just b
  | otherwise = Nothing

lookupBook (b:bs) value
  | title b == value = Just b
  | otherwise = lookupBook bs value



