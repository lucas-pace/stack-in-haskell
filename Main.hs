--https://stackoverflow.com/questions/22547430/haskell-readfile-line-by-line-and-put-into-list
--https://stackoverflow.com/questions/5952167/how-do-i-print-a-list-in-haskell
--https://stackoverflow.com/questions/19956221/reading-lines-and-reversing-the-text-with-haskell
--https://stackoverflow.com/questions/40566940/searching-through-list-of-tuples
--https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string/46924479
--https://stackoverflow.com/questions/20667478/haskell-string-int-type-conversion
--https://stackoverflow.com/questions/46568661/how-do-i-correctly-use-tolower-in-haskell
--https://stackoverflow.com/questions/30029029/haskell-check-if-string-is-valid-number

import System.IO
import Data.List.Split
import Data.List
import Stack
import Data.Char

main = do
        content <- readFile "programa.txt"
        let linesList = lines content
        let stack =  empty
        let vars = []
        let returned = ("","")
        percorrerLinhas linesList stack vars


percorrerLinhas inputList stack vars = do
    if null inputList
        then return ()
    else do
      let line = head inputList
      if null line
          then return ()
          else do -- processando a linha head e passando a tail para recursão.
              returned <- processarLinha line stack vars

              let newStack = (fst returned)
              let vars = (snd returned)
              percorrerLinhas (tail inputList) newStack vars


processarLinha linha stack vars = do
        let palavras = words linha

        let first = head palavras -- primeira instrucao
        let second = tail palavras  -- parametro da instrucao se houver
        let argument = head second
        if isSubsequenceOf "cint" first -- ## depois do then so podemos ter um linha, com o do ele transforma tudo em 1 linha
                then do
                        --print "cint"

                        let newStack = push ("int", argument) stack
                        return (newStack, vars)

        else if isSubsequenceOf "push" first
                then do let var = head second
                        let searchResult = search var vars
                        let newStack = push (head searchResult) stack
                        -- print newStack
                        return (newStack, vars)


        else if isSubsequenceOf "pop" first
                then do let var = head second
                        let value = top stack
                        let newStack = pop stack -- retirna valor da pilha e a atualiza
                        let searchResult = search var vars
                        let searchResult2 = search2 (head searchResult) vars

                        if (not) (searchResult == []) -- caso tenha alguma variavel com o mesmo nome ja salva, deletamos ela e inserimos a nova.
                                then do

                                        let temporaryVars = delete ((head searchResult2),(head searchResult)) vars
                                        let newVars = temporaryVars ++ [(var,(fst value,snd value))]
                                        return (newStack, newVars)
                        else do
                                let newVars = vars ++ [(var,(fst value,snd value))]
                                return (newStack, newVars)



        else if isSubsequenceOf "add" first
                then do
                        let num1 = read (snd (top stack)) :: Integer
                        let temporaryStack = pop stack
                        let num2 = read (snd (top temporaryStack)) :: Integer
                        let temporaryStack2 = pop temporaryStack


                        let sum = show  (num1 + num2)
                        let newStack = push ("int",sum) temporaryStack2

                        return (newStack, vars)

        else if isSubsequenceOf "sub" first
                then do
                        let num1 = read (snd (top stack)) :: Integer
                        let temporaryStack = pop stack
                        let num2 = read (snd (top temporaryStack)) :: Integer
                        let temporaryStack2 = pop temporaryStack


                        let sub = show  (num2 - num1)
                        let newStack = push ("int",sub) temporaryStack2

                        return (newStack, vars)

        else if isSubsequenceOf "mult" first
                then do
                        let num1 = read (snd (top stack)) :: Integer
                        let temporaryStack = pop stack
                        let num2 = read (snd (top temporaryStack)) :: Integer
                        let temporaryStack2 = pop temporaryStack


                        let mult = show  (num1 * num2)
                        let newStack = push ("int",mult) temporaryStack2

                        return (newStack, vars)

        else if isSubsequenceOf "div" first
                then do
                        let num1 = read (snd (top stack)) :: Integer
                        let temporaryStack = pop stack
                        let num2 = read (snd (top temporaryStack)) :: Integer
                        let temporaryStack2 = pop temporaryStack

                        let divResult = div num1 num2
                        let div = show  (divResult)
                        let newStack = push ("int",div) temporaryStack2

                        return (newStack, vars)

        else if isSubsequenceOf "mod" first
                then do
                        let num1 = read (snd (top stack)) :: Integer
                        let temporaryStack = pop stack
                        let num2 = read (snd (top temporaryStack)) :: Integer
                        let temporaryStack2 = pop temporaryStack

                        let modResult = mod num1 num2
                        let mod = show  (modResult)
                        let newStack = push ("int",mod) temporaryStack2

                        return (newStack, vars)


        else if isSubsequenceOf "read" first
                then do

                        input <- getLine
                        if isSubsequenceOf "true" (map toLower input)
                                then do
                                        let newStack = push ("bool", "true") stack
                                        return (newStack, vars)
                        else if isSubsequenceOf "false" (map toLower input)
                                then do
                                        let newStack = push ("bool", "false") stack
                                        return (newStack, vars)

                        else if (checkNum input)
                                then do
                                        let newStack = push ("int", input) stack
                                        return (newStack, vars)
                        else do
                                let newStack = push ("str", input) stack
                                return (newStack, vars)

        else if isSubsequenceOf "print" first
                then do
                        print(stack)
                        print(vars)
                        return (stack, vars)

        -- else if isSubsequenceOf "jump" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        -- else if isSubsequenceOf "dcond" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        else if isSubsequenceOf "cmp" first
                then do let num1 = top stack
                        let temporaryStack = pop stack
                        let num2 = top temporaryStack
                        let temporaryStack2 = pop temporaryStack

                        if (num1 == num2) then do
                                let newStack = push ("bool", "true") temporaryStack2
                                return (newStack, vars)

                        else do let newStack = push ("bool", "false") temporaryStack2
                                return (newStack, vars)


        else if isSubsequenceOf "cstr" first
                then do
                        let newStack = push ("str",argument) stack
                        return (newStack,vars)


        else if isSubsequenceOf "nop" first
                then do return (stack,vars)

        else return (stack,vars)

search :: (Eq a) => a -> [(a,b)] -> [b]
search x = map snd . filter ((==x).fst)

search2 :: (Eq b) => b -> [(a,b)] -> [a]
search2 x = map fst . filter ((==x).snd)

checkNum :: String -> Bool
checkNum = all isDigit