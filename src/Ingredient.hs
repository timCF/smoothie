module Ingredient
  ( ingredients
  ) where

import           Data.Char
import           Data.List

data Smoothie = Classic | ForestBerry | Freezie | Greenie | VeganDelite | JustDesserts
                deriving (Show, Read, Eq)

data Substance = Strawberry | Banana | Pineapple | Mango | Peach | Honey | Ice | Yogurt |
                 Raspberry | Blueberry |
                 Blackberry | BlackCurrant | GrapeJuice | FrozenYogurt |
                 GreenApple | Kiwi | Lime | Avocado | Spinach | AppleJuice |
                 PassionFruit | SoyMilk |
                 IceCream | Chocolate | Peanut | Cherry
                 deriving (Show, Read, Eq)

defaultSubstances :: Smoothie -> [Substance]
defaultSubstances Classic = [Strawberry, Banana, Pineapple, Mango, Peach, Honey, Ice, Yogurt]
defaultSubstances ForestBerry = [Strawberry, Raspberry, Blueberry, Honey, Ice, Yogurt]
defaultSubstances Freezie = [Blackberry, Blueberry, BlackCurrant, GrapeJuice, FrozenYogurt]
defaultSubstances Greenie = [GreenApple, Kiwi, Lime, Avocado, Spinach, Ice, AppleJuice]
defaultSubstances VeganDelite = [Strawberry, PassionFruit, Pineapple, Mango, Peach, Ice, SoyMilk]
defaultSubstances JustDesserts = [Banana, IceCream, Chocolate, Peanut, Cherry]

class (Read a, Show a) => HumanReadable a where
  showHR :: a -> String
  showHR = defaultShowHR
  readHR :: String -> a
  readHR s =
    read $ case foldr go [] s of
      []       -> []
      (x : xs) -> toUpper x : xs
    where
      go :: Char -> String -> String
      go ' ' []       = []
      go ' ' (x : xs) = toUpper x : xs
      go x xs         = x : xs

defaultShowHR :: (Show a) => a -> String
defaultShowHR s = foldr go [] (show s)
  where
    go :: Char -> String -> String
    go y []       = [y]
    go y (x : xs) = if isLower y && isUpper x then y : ' ' : x : xs else y : x : xs

instance HumanReadable Smoothie
instance HumanReadable Substance where
  showHR s = toLower <$> defaultShowHR s

data TokenKind = TokenSmoothie | TokenSubstance
data Token a = Token a String | TokenComma | TokenMinus

tokenize :: String -> [Token TokenKind]
tokenize = foldr go []
  where
    go :: Char -> [Token TokenKind] -> [Token TokenKind]
    go ',' ts               = TokenComma : ts
    go '-' ts               = TokenMinus : ts
    go x (Token tk xs : ts) = Token (infer x tk) (x : xs) : ts
    go x ts                 = Token (infer x TokenSubstance) [x]  : ts
    infer :: Char -> TokenKind -> TokenKind
    infer _ TokenSmoothie = TokenSmoothie
    infer x TokenSubstance = if isUpper x then TokenSmoothie else TokenSubstance

newtype ExpSubstance = ExpMinus Substance
data ExpSmoothie = ExpSmoothie Smoothie [ExpSubstance]

parse :: [Token TokenKind] -> [ExpSmoothie]
parse = go []
  where
    go :: [ExpSmoothie] -> [Token TokenKind] -> [ExpSmoothie]
    go es [] = es
    go es (TokenComma : ts) = go es ts
    go es (Token TokenSmoothie s : ts) = go (ExpSmoothie (readHR s) [] : es) ts
    go (ExpSmoothie x es : rest) (TokenMinus : Token TokenSubstance s : ts) = go (ExpSmoothie x (ExpMinus (readHR s) : es) : rest) ts

eval :: ExpSmoothie -> [Substance]
eval (ExpSmoothie x es) = foldl go (defaultSubstances x) es
  where
    go :: [Substance] -> ExpSubstance -> [Substance]
    go ss (ExpMinus s) = filter (/= s) ss

showIngredients :: [Substance] -> String
showIngredients ss = foldr go [] (sort $ showHR <$> ss)
  where
    go :: String -> String -> String
    go w ""  = w
    go w acc = w ++ "," ++ acc

ingredients :: String -> String
ingredients = showIngredients . eval . head . parse . tokenize
