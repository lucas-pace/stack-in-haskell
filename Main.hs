--https://stackoverflow.com/questions/22547430/haskell-readfile-line-by-line-and-put-into-list
--https://stackoverflow.com/questions/5952167/how-do-i-print-a-list-in-haskell
--https://stackoverflow.com/questions/19956221/reading-lines-and-reversing-the-text-with-haskell
--https://stackoverflow.com/questions/40566940/searching-through-list-of-tuples


import System.IO
import Data.List.Split
import Data.List
import Stack


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

        if isSubsequenceOf "cint" first -- ## depois do then so podemos ter um linha, com o do ele transforma tudo em 1 linha
                then do
                        --print "cint"
                        let argument = head second
                        let newStack = push ("int", argument) stack
                        return (newStack, vars)

        else if isSubsequenceOf "push" first
                then do let var = head second
                        let searchResult = search var vars
                        let newStack = push (head searchResult) stack
                        print newStack
                        return (newStack, vars)


        else if isSubsequenceOf "pop" first
                then do let var = head second
                        let value = top stack
                        let newStack = pop stack -- retirna valor da pilha e a atualiza
                        let searchResult = search var vars
                        let searchResult2 = search2 (head searchResult) vars

                        if (not) (searchResult == [])
                                then do
                                        -- print (head searchResult)
                                        -- print (tail searchResult)

                                        --let newStack = push (head searchResult) stack
                                        let temporaryVars = delete ((head searchResult2),(head searchResult)) vars

                                        let newVars = temporaryVars ++ [(var,(fst value,snd value))]
                                        -- let newVars = delete (head searchResult) vars

                                        return (newStack, newVars)
                                        -- if (not) (tailSearch == []) then do
                                        --         return (newStack, tailSearch)
                                        -- else do
                                        --         return (newStack, [])
                        else do
                                let newVars = vars ++ [(var,(fst value,snd value))] -- atualiza lista de variaveis
                                return (newStack, newVars)







        -- else if isSubsequenceOf "add" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        -- else if isSubsequenceOf "sub" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        -- else if isSubsequenceOf "mult" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        -- else if isSubsequenceOf "div" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        -- else if isSubsequenceOf "mod" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        -- else if isSubsequenceOf "read" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        else if isSubsequenceOf "print" first
                then do --print first
                        -- let topo = top stack
                        -- let typ = fst topo
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

        -- else if isSubsequenceOf "cmp" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)


        -- else if isSubsequenceOf "cstr" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)


        -- else if isSubsequenceOf "nop" first
        --         then do print first
        --                 print second
        --                 return (stack, vars)

        else return (stack,vars)

search :: (Eq a) => a -> [(a,b)] -> [b]
search x = map snd . filter ((==x).fst)

search2 :: (Eq b) => b -> [(a,b)] -> [a]
search2 x = map fst . filter ((==x).snd)