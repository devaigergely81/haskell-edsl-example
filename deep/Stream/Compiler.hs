{-# LANGUAGE GADTs, TypeApplications, ScopedTypeVariables #-}
module Stream.Compiler where

import Control.Monad.State

import Stream.AbstractSyntax

instance StreamType Int where
    typeName = "int"

instance StreamType Bool where
    typeName = "bool"

instance Show Void where
    show _ = error "Internal error: Showing a 'Void'."

instance StreamType Void where
    typeName = "Internal error: typeName of Void requested."

instance Show (Elem a) where
    show (Symbol name _) = name
    show (Var v) = v
    show (App (App (Symbol binop _) a) b) = "(" ++ show a ++ " " ++ binop ++ " " ++ show b ++ ")"
    show (App (Symbol f _) a) = f ++ "(" ++ show a ++ ")"
    show _ = error "Unsupported expression."

compile :: Stream Void -> IO ()
compile str = putStrLn $ compile' str

compileToFile :: Stream Void -> FilePath -> IO ()
compileToFile str fileName = writeFile fileName $ compile' str

compile' :: Stream Void -> String
compile' str =
       "#include <iostream>\n\n"
    ++ "int main() {\n"
    ++ "  while(true) {\n"
    ++ indent 2 (fst (runState (compile'' str) 0))
    ++ "  }\n"
    ++ "}\n"

var :: State Int String
var = do
    idx <- get
    return $ "var" ++ show idx

newVar :: State Int String
newVar = do
    idx <- get
    put $ idx + 1
    var

compile'' :: (StreamType a) => Stream a -> State Int String
compile'' (Input :: Stream a) = do
    v <- newVar
    return $
        (typeName @ a) ++ " " ++ v ++ ";\n" ++
        "std::cin >> " ++ v ++ ";\n"
compile'' (Output input) = do
    inputPrg <- compile'' input
    prev <- var
    return $
        inputPrg ++
        "std::cout << " ++ prev ++ " << std::endl;\n"
compile'' (ForEach (tr :: Elem a -> Elem b) input) = do
    inputPrg <- compile'' input
    prev <- var
    next <- newVar
    return $
        inputPrg ++
        (typeName @ b) ++ " " ++ next ++ ";\n" ++
        next ++ " = " ++ show (tr $ Var prev) ++ ";\n"
compile'' (Group n init (f :: Elem b -> Elem a -> Elem b) input) = do
    inputPrg <- compile'' input
    prev <- var
    i <- newVar
    next <- newVar
    return $
        (typeName @ b) ++ " " ++ next ++ " = " ++ show init ++ ";\n" ++
        "for(int " ++ i ++ " = 0; " ++ i ++ " < " ++ show n ++ "; ++" ++ i ++ ") {\n" ++
        indent 1 inputPrg ++
        "  " ++ next ++ " = " ++ show (f (Var next) (Var prev)) ++ ";\n" ++
        "}\n"

indent :: Int -> String -> String
indent n str = unlines $ map ((replicate (2*n) ' ')++) $ lines str
