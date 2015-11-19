module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse spacesPlusString "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

spacesPlusString :: Parser LispVal
spacesPlusString = do
                     spaces
		     parseExpr

main :: IO ()
main = do 
         expr <- getLine
         putStrLn (readExpr expr)

spaces = skipMany1 space 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
	     deriving (Show)
 
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many parseStringChar
                char '"'
                return $ String x

parseStringChar :: Parser Char
parseStringChar = (parseEscapedChar <|> noneOf "\"")

parseEscapedChar :: Parser Char
parseEscapedChar = do
                     char '\\'
                     x <- char 'n' <|> char 't' <|> char '\\' <|> char '"'
                     return $
                       case x of
                         'n' -> '\n'
                         't' -> '\t'
                         '\\' -> '\\'
                         '"' -> '"'

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
-- using liftM:
--   parseNumber = liftM (Number . read) $ many1 digit
-- using do notation:
--   parseNumber = do 
--                 x <- many1 digit 
--                 return  (Number  (read x))
-- using bind
parseNumber = many1 digit >>=
                return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
