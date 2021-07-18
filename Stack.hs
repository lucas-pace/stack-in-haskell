-- https://stackoverflow.com/questions/12485726/list-containing-different-types
module Stack where

    data MemberType = Int Integer | Str String | Boo Bool deriving Show
    type Collection = [MemberType]


    data Stack a = Stk [a] deriving (Show, Eq)-- representação usando listas

    push :: a -> Stack a -> Stack a
    push x (Stk xs) = Stk (x:xs)

    pop :: Stack a -> Stack a
    pop (Stk (_:xs)) = Stk xs
    pop _ = error "Stack.pop: empty stack"

    top :: Stack a -> a
    top (Stk (x:_)) = x
    top _ = error "Stack.top: empty stack"


    empty :: Stack a
    empty = Stk []

    isEmpty :: Stack a -> Bool
    isEmpty (Stk [])= True
    isEmpty (Stk _) = False

