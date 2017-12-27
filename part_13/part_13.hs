import Control.Applicative
  ( Alternative
  , empty
  , (<|>)
  , some
  , many
  )
import Data.Char 
  ( isDigit
  , isLower
  , isUpper
  , isAlpha
  , isAlphaNum
  , isSpace
  , toUpper
  )

-- Basic

newtype Parser a = Parser { parse :: String -> [(a, String)] }

item :: Parser Char
item = Parser $ \str ->
  case str of
    ""      -> []
    (c:cs)  -> [(c, cs)]

sampleItem = parse item ""
sampleItem' = parse item "abc"

-- Composition Laws

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parserA =
    Parser $ \str ->
      case parse parserA str of
        []            -> []
        [(a, str')]   -> [(f a, str')]

sampleFunctor = parse (fmap toUpper item) ""
sampleFunctor' = parse (fmap toUpper item) "abc"

instance Applicative Parser where
  -- pure :: a -> Parser a 
  pure a = Parser $ \str -> [(a, str)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  parserF <*> parserA =
    Parser $ \str ->
      case parse parserF str of
        []            -> []
        [(f, str')]   -> parse (fmap f parserA) str'

sampleApplicative = parse (pure 1) "abc"
sampleApplicative' = parse three "abcdef"
  where
    three :: Parser (Char, Char)
    three = pure f <*> item <*> item <*> item

    f x y z = (x, z)

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ \str -> []

  -- <|> :: Parser a -> Parser a -> Parser a
  parser1 <|> parser2 =
    Parser $ \str ->
      case parse parser1 str of
        []                -> parse parser2 str
        [(value, str')]   -> [(value, str')]

  -- many :: Parser a -> Parser [a]
  many parser = some parser <|> pure []

  -- some :: Parser a -> Parser [a]
  some parser = pure (:) <*> parser <*> many parser

sampleAlternative = parse empty "abc"
sampleAlternative' = parse (item <|> pure 'd') "abc"
sampleAlternative'' = parse (empty <|> pure 'd') "abc"

sampleMany = parse (many digit) "123abc"
sampleMany' = parse (many digit) "abc"
sampleSome = parse (some digit) "123abc"
sampleSome' = parse (some digit) "abc"

instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  parserA >>= f =
    Parser $ \str ->
      case parse parserA str of
        []          -> []
        [(a, str')] -> parse (f a) str'

sampleMonad = parse (return 1) "abc"
sampleMonad' = parse three "abcdef"
  where
    three :: Parser (Char, Char)
    three = do
      x <- item
      item
      z <- item
      return (x, z)

-- Primitives

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  x <- item
  if f x then return x
         else empty

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

space :: Parser Char
space = satisfy isSpace

char :: Char -> Parser Char
char c = satisfy (== c)

sampleChar = parse (char 'a') "abc"

string :: String -> Parser String
string []     = pure []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

sampleString = parse (string "abc") "abcdef"
sampleString' = parse (string "abc") "ab1234"

ident :: Parser String
ident = do
  c <- lower
  cs <- many alphanum
  return (c:cs)

nat :: Parser Int
nat = do
  cs <- some digit
  return (read cs)

spaces :: Parser ()
spaces = do 
  many space
  return ()

sampleIdent = parse ident "abc def"
sampleNat = parse nat "123 abc"
sampleSpaces = parse spaces "     abc"

int :: Parser Int
int = (do
  char '-'
  n <- nat
  return (-n)
  ) <|> nat

sampleInt = parse int "-123 abc"
sampleInt' = parse int "123 abc"

token :: Parser a -> Parser a
token parser = do
  spaces
  value <- parser
  spaces
  return value

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol sym = token (string sym)

sampleNaturals = parse listOfNaturals "    [1,   2, 3]"
sampleNaturals' = parse listOfNaturals "[1, 2,    ]"

listOfNaturals :: Parser [Int]
listOfNaturals = do
  symbol "["
  n <- natural
  ns <- many (symbol "," >> natural)
  symbol "]"
  return (n:ns)

