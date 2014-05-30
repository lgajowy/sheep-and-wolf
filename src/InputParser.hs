module InputParser where

import Data.Char
import Control.Monad

type ParserFunction a = String -> [(a, String)]

success :: a -> ParserFunction a
success value = \input -> [(value, input)]

failure :: ParserFunction a
failure = \_ -> []

item :: ParserFunction Char
item = \input -> case input of
                   []     -> []
                   (x:xs) -> [(x, xs)]

eof :: ParserFunction ()
eof = \input -> case input of
                  [] -> [((), [])]
                  _  -> []

data Parser a = Parser (ParserFunction a)

class Parsers p where parse :: p a -> ParserFunction a 

instance Parsers Parser where
   parse (Parser p) = p

bind :: Parser a -> (a -> Parser b) -> ParserFunction b
p1 `bind` p2 = \input -> case parse p1 input of
                           []            -> []
                           [(v, output)] -> parse (p2 v) output

instance Monad Parser where
	return value = Parser (success value)
	fail _       = Parser (failure)
	a >>= b      = Parser (a `bind` b)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- Parser item
           if p x then return x else fail "parse error"


char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

alt :: Parser a -> Parser a -> ParserFunction a
p1 `alt` p2 = \input -> case parse p1 input of
                          []            -> parse p2 input
                          [(v, output)] -> [(v, output)]


instance MonadPlus Parser where
  mzero       = Parser failure
  a `mplus` b = Parser (a `alt` b)


many :: Parser a -> Parser [a]
many p = some p `mplus` return []

some :: Parser a -> Parser [a]
some p = do value <- p
            values <- many p
            return (value:values)

digit :: Parser Char
digit = sat isDigit

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             value <- p
             space
             return value

num :: Parser Int
num = do digits <- some digit
         return (read digits)

number :: Parser Int
number = token num

symbol :: String -> Parser String
symbol word = token (string word)

coord :: Parser (Int, Int)
coord = do n <- number
           ns <- many (do symbol ","
                          number)
           return (n, head ns)



