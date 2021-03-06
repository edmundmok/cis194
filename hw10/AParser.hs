{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Control.Monad
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap g (Parser f) = Parser (f >=> (return . first g))

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  Parser f <*> Parser g = Parser h
    where
      h s =
        case f s of
          Nothing -> Nothing
          Just (f', s') ->
            case g s' of
              Nothing -> Nothing
              Just (x, s'') -> Just (f' x, s'')

  --(<*>) :: f (a -> b) -> f a -> f b
  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b

  -- runParser :: String -> Maybe (a -> b, String)
  -- runParser :: String -> Maybe (a, String)

type Name = String
data Employee = Emp { name :: Name, phone :: String }
  deriving (Show, Read, Eq)

parseName :: Parser Name
parseName = pure "name"

parsePhone :: Parser String
parsePhone = pure "phone"

abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> satisfy (== 'a') <*> satisfy (== 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> satisfy (== 'a') <*> satisfy (== 'b')

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> satisfy (== ' ') <*> posInt

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  Parser f <|> Parser g = Parser (\s -> f s <|> g s)

intOrUppercase :: Parser ()
intOrUppercase = fmap (\_ -> ()) posInt <|> fmap (\_ -> ()) (satisfy isUpper)
