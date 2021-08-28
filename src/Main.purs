module Main where

import Data.Array
import Data.Either
import Data.Maybe
import Data.Tuple
import ZPrelude

import Data.ArrayView as Aview
import Data.String as S
import Data.String.Pattern as P
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable

import Tables

createTerm :: Int -> Int -> Either String String
createTerm a b = err a b <| pure "a." <> basis !! a
    <> pure " * b." <> basis !! b

err :: forall a. Int -> Int -> Maybe a -> Either String a
err a b (Just x) = Right x
err a b Nothing = Left <| show a <> " " <> show b

-- Index 2D Array
ix2 :: forall x.
    Array (Array x) -> Int -> Int -> Either String x
ix2 tbl a b = err a b <| (tbl !! a) >>= (_ !! b)

data Value = E Int | None

instance showValue :: Show Value where
  show None = "None"
  show (E x) = "(E " <> show x <> ")"

mkTuple :: String -> Either String Int ->
    Either String (Tuple String Value)
mkTuple sign (Left l) = Left l
mkTuple sign (Right x) = Right (Tuple sign (E x))

interpEntry :: Either String String ->
    Either String (Tuple String Value)
interpEntry (Right x)
  | x          ==  "0" = Right <| Tuple "" None
  | x          ==  "1" = mkTuple " + "  <| Right 0
  | x          == "-1" = mkTuple " - " <| Right 0
  | S.take 1 x ==  "e" = mkTuple " + "
      <| note ("interp e = " <> x <> ", not in basis")
      <| elemIndex x basis
  | S.take 2 x == "-e" = mkTuple " - "
      <| note ("interp -e = " <> x <> ", not in basis")
      <| elemIndex (S.drop 1 x) basis
  | otherwise          = Left <| "interp other = " <> x
interpEntry (Left l) = Left <| "interp bad arg = " <> l

modifier :: Either String String ->
    Either String (Tuple String Value) ->
    Either String (Array String) ->
    Either String (Array String)
modifier (Right term) (Right (Tuple sign (E ix))) (Right r) =
  note ("modify " <> show ix)
  (modifyAt ix (_ <> (sign <> term)) r)
modifier _ (Right (Tuple _ None)) r = r
modifier a b c = Left <| show a <> " <> " <> show b
    <> " <> " <> show c

genRow :: CayleyTable -> Int -> Int ->
  Either String (Array String) ->
  Either String (Array String)
genRow _ _ _ (Left r) = Left r
genRow tbl a b res =
  if b > 15 then res
  else genRow tbl a (b + 1) <| modifier (createTerm a b)
      (interpEntry <| ix2 tbl a b) res

genMult :: CayleyTable -> Int -> Either String (Array String) -> Either String (Array String)
genMult _ _ (Left r) = Left r
genMult tbl a res =
  if a > 15 then res
  else genMult tbl (a + 1) <| genRow tbl a 0 res

-- Concatenate basis with multiply strings
-- Get rid of + or - prefix on each string
combine :: Array String -> String
combine = fold <<< zipWith append basis
  <<< map (\s -> ":" <> unPrefix s <> ",\n")

appender :: String -> String -> String
appender s1 s2 = if S.null s2 then s2 else append s1 s2

mkLine :: String -> String
mkLine s = if S.null s then s else ":" <> unPrefix s <> ",\n"

unPrefix :: String -> String
unPrefix s = let ss = S.splitAt 2 s in
  if S.null s then " 0"
  else if ss.before == " +" then ss.after
  else if ss.before == " -" then " negate" <> ss.after
  else s 

xxx :: CayleyTable -> Either String String
xxx tbl = combine <$> genMult tbl 0 (Right emptyAS)

unPostfix :: String -> String
unPostfix s = fromMaybe s <| S.stripSuffix (P.Pattern ",\n") s

main :: Effect Unit
main = do
  for_ tables \{name, tab} -> do
    case xxx tab of
      Left l -> log l
      Right r -> log <| name <> " a b = \n {"
        <> unPostfix r <> "\n}\n"


