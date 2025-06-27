{-# LANGUAGE GADTs #-}
module Main where

import Data.Foldable ( for_ )
import Text.Parsec
import Text.ParserCombinators.Parsec.Number ( int )
import Data.Either (fromRight)

data Express a where
  App :: (Express a) -> (Express a) -> Express a
  Lam :: (Express a) -> Express a
  Express :: a -> Express a
  deriving (Eq, Show)

main :: IO ()
main = for_ [0..] loop

loop n = do
  a <- getLine
  putStrLn $ deParse $ applyReduction $ fromRight (Express (-1)) $ parse myParser "" a

myParser = try $ Express <$> int <|> try lamParser <|> appParser

lamParser = Lam <$> (char 'l' >> myParser)

appParser = App <$> (char '(' >> myParser <* char ' ') <*> myParser <* char ')'

deParse (Lam exp) = "l" ++ deParse exp
deParse (App a b) = "(" ++ deParse a ++ " " ++ deParse b ++ ")"
deParse (Express a) = show a

betaReduce :: Int -> Express Int -> Express Int -> Express Int
betaReduce depth (Express leafInt) n = if depth == leafInt then n else Express leafInt
betaReduce depth (Lam a) n = Lam $ betaReduce (depth + 1) a n
betaReduce depth (App a b) n = App (betaReduce depth a n) $ betaReduce depth b n

applyReduction :: Express Int -> Express Int
applyReduction (App (Lam m) n) = applyReduction $ betaReduce 1 m n
applyReduction (App m n) = applyReduction $ App (applyReduction m) n
applyReduction a = a
