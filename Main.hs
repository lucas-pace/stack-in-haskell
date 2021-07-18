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
        let allLines = lines content
        let stack =  empty
        let vars = []
        let returned = ("","")
        percorrerLinhas linesList stack vars allLines


percorrerLinhas inputList stack vars allLines = do
    if null inputList
        then return ()
    else do
      let line = head inputList
      if null line
          then return ()
          else do -- processando a linha head e passando a tail para recursão.
                returned <- processarLinha line stack vars


                --nova pilha e variaveis
                let newStack = (fst returned)
                let vars = (snd returned)

                -- verificando presenca de jump e execuntando. Verifica se ha algo na memoria, caso tenha, verifica se a ultima instrução adiciona é um jump. Se for, muda a lista de linhas restantes para as linhas a partir da linha desejada.
                if (not) (vars == []) then do
                        if  (fst (head vars)) == "" then do
                                if  (fst(snd (head vars))) == "jump" then do
                                        let numberOfLine = (read (snd(snd (head vars))) :: Int ) - 1
                                        let newLines = drop' numberOfLine allLines
                                        let newVars = drop' 1 vars -- removendo da memoria o jump
                                        --print newVars
                                        percorrerLinhas newLines newStack newVars allLines

                                else do print "error?"
                        else do percorrerLinhas (tail inputList) newStack vars allLines
                else do
                        percorrerLinhas (tail inputList) newStack vars allLines


processarLinha linha stack vars = do
        let palavras = words linha

        let first = head palavras -- primeira instrucao
        let second = tail palavras  -- parametro da instrucao se houver
        let argument = head second
        if isSubsequenceOf "cint" first
                then do
                        --print "cint"

                        let newStack = push ("int", argument) stack
                        return (newStack, vars)

        else if isSubsequenceOf "push" first
                then do
                        let searchResult = search argument vars
                        let newStack = push (head searchResult) stack
                        -- print newStack
                        return (newStack, vars)


        else if isSubsequenceOf "pop" first
                then do
                        let value = top stack
                        let newStack = pop stack -- retirna valor da pilha e a atualiza
                        let searchResult = search argument vars
                        let searchResult2 = search2 (head searchResult) vars

                        if (not) (searchResult == []) -- caso tenha alguma variavel com o mesmo nome ja salva, deletamos ela e inserimos a nova.
                                then do

                                        let temporaryVars = delete ((head searchResult2),(head searchResult)) vars
                                        let newVars = temporaryVars ++ [(argument,(fst value,snd value))]
                                        return (newStack, newVars)
                        else do
                                let newVars = vars ++ [(argument,(fst value,snd value))]
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
                        let topo = top stack
                        let newStack = pop stack
                        print ( read (snd topo) :: Integer)
                        return (newStack, vars)

        else if isSubsequenceOf "jump" first
                then do
                        let newVars = ("",("jump",argument)) : vars
                        return (stack, newVars)

        else if isSubsequenceOf "dcond" first
                then do
                        let topo = top stack
                        let tipo = (fst topo)
                        let value = (snd topo)
                        let newStack = pop stack
                        if (tipo == "bool" && value == "true")
                                then do
                                        let newVars = ("",("jump",argument)) : vars
                                        return (newStack, newVars)
                        else do

                                return (newStack, vars)

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

-- dado uma lista de tupla, devolve uma lista de tuplas (a,b) em que a = argumento passado
search :: (Eq a) => a -> [(a,b)] -> [b]
search x = map snd . filter ((==x).fst)


-- dado uma lista de tupla, devolve uma lista de tuplas (a,b) em que b = argumento passado
search2 :: (Eq b) => b -> [(a,b)] -> [a]
search2 x = map fst . filter ((==x).snd)

-- checa se todos os numeros da string sao digitos
checkNum :: String -> Bool
checkNum = all isDigit



-- remove os N primeiros elementos de uma lista
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 ys = ys
drop' x ys = drop' (x-1) (tail ys)