module Stream (
    module Prelude, module Stream)
    where

class (Read a, Show a) => StreamType a

instance StreamType Int
instance StreamType Bool

type Stream a = IO a

input :: (StreamType a) => IO a
input = do
    line <- getLine
    return $ read line

output :: (StreamType a) => IO a -> IO ()
output str = do
    a <- str
    putStrLn $ show a
    output str

infixl 1 >>>
(>>>) :: IO a -> (IO a -> IO b) -> IO b
a >>> f = f a

foreach :: (StreamType a, StreamType b) => (a -> b) -> IO a -> IO b
foreach f str = do
    a <- str
    return $ f a

group :: (StreamType a, StreamType b) => Int -> b -> (b -> a -> b) -> IO a -> IO b
group n init f str
    | n == 0    = return init
    | n > 0     = do
        a <- str
        group (n-1) (f init a) f str

true :: Bool
true = True

false :: Bool
false = False
