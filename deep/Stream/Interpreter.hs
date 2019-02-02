{-# LANGUAGE GADTs #-}
module Stream.Interpreter where

import Stream.AbstractSyntax

execute :: (StreamType a) => Stream a -> IO a
execute Input = do
    line <- getLine
    return $ read line
execute (Output str) = do
    val <- execute str
    putStrLn $ show val
    execute (Output str)
execute (ForEach f str) = do
    val <- execute str
    return $ eval $ f $ Symbol "" val
execute (Group n init f str)
    | n <= 0    = return $ eval init
    | otherwise = do
        val <- execute str
        execute $ Group (n-1) (Symbol "" $ eval $ f init $ Symbol "" val) f str

eval :: Elem a -> a
eval (Symbol _ val) = val
eval (App f arg) = (eval f) (eval arg)
eval (Var _) = error "Cannot evaluate variables."
