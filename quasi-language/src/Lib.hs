module Lib
    ( someFunc
    ) where
        
import Text.Parsec
import Parser as P

someFunc :: IO ()
someFunc = do
    content <- readFile "./app/resources/code.qs"
    let result = parse (P.parseStatement) "./app/resources/code.qs" content
    print result
