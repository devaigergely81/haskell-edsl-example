{-# LANGUAGE GADTs, AllowAmbiguousTypes #-}
module Stream.AbstractSyntax where

data Void

instance Eq Void where
    a == b = True

instance Read Void where
    readsPrec = error "Cannot read Void"

class (Show a, Read a, Eq a) => StreamType a where    
    typeName :: String

data Elem a where
    Symbol  :: String -> a -> Elem a
    App     :: (StreamType a) => Elem (a -> b) -> Elem a -> Elem b
    Var     :: (StreamType a) => String -> Elem a

data Stream a where
    Input   :: (StreamType a) => Stream a
    Output  :: (StreamType a) => Stream a -> Stream Void
    ForEach :: (StreamType a, StreamType b) =>
        (Elem a -> Elem b) -> Stream a -> Stream b
    Group   :: (StreamType a, StreamType b) =>
        Int -> Elem b -> (Elem b -> Elem a -> Elem b) -> Stream a -> Stream b

