{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

module Stream (
    false, true, (&&), (||), not,
    (<), (>), (<=), (>=), (==),
    (+), (-), (*), negate, signum, abs, 
    Stream, Int, Bool, input, output, foreach, group, (>>>),
    execute, compile, compileToFile) where

import Data.List hiding (group)
import Prelude hiding ((<),(<=),(>),(>=),(==),(&&),(||),not)
import qualified Prelude

import Stream.AbstractSyntax
import Stream.Interpreter
import Stream.Compiler

instance Num (Elem Int) where
    a + b = App (App (Symbol "+" (+)) a) b
    a - b = App (App (Symbol "-"(-)) a) b
    a * b = App (App (Symbol "*" (*)) a) b
    negate a = App (Symbol "negate" negate) a
    signum a = App (Symbol "signum" signum) a
    abs a = App (Symbol "abs" abs) a
    fromInteger a = Symbol (show a) (fromInteger a)

infix 4 <
(<) :: Elem Int -> Elem Int -> Elem Bool
a < b = App (App (Symbol "<" (Prelude.<)) a) b

infix 4 <=
(<=) :: Elem Int -> Elem Int -> Elem Bool
a <= b = App (App (Symbol "<=" (Prelude.<=)) a) b

infix 4 >
(>) :: Elem Int -> Elem Int -> Elem Bool
a > b = App (App (Symbol ">" (Prelude.>)) a) b

infix 4 >=
(>=) :: Elem Int -> Elem Int -> Elem Bool
a >= b = App (App (Symbol ">=" (Prelude.>=)) a) b

infix 5 ==
(==) :: (StreamType a) => Elem a -> Elem a -> Elem Bool
a == b = App (App (Symbol "==" (Prelude.==)) a) b

false :: Elem Bool
false = Symbol "false" False

true :: Elem Bool
true = Symbol "true" True

infixr 3 &&
(&&) :: Elem Bool -> Elem Bool -> Elem Bool
a && b = App (App (Symbol "&&" (Prelude.&&)) a) b

infixr 2 ||
(||) :: Elem Bool -> Elem Bool -> Elem Bool
a || b = App (App (Symbol "||" (Prelude.||)) a) b

not :: Elem Bool -> Elem Bool
not a = App (Symbol "!" Prelude.not) a

input :: (StreamType a) => Stream a
input = Input

output :: (StreamType a) => Stream a -> Stream Void
output = Output

foreach :: (StreamType a, StreamType b) =>
    (Elem a -> Elem b) -> Stream a -> Stream b
foreach = ForEach

group :: (StreamType a, StreamType b) =>
    Int -> Elem b -> (Elem b -> Elem a -> Elem b) -> Stream a -> Stream b
group = Group

infixl 1 >>>
(>>>) :: (StreamType a, StreamType b) =>
    Stream a -> (Stream a -> Stream b) -> Stream b
str1 >>> str2 = str2 str1
