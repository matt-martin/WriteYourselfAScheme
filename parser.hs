module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char  (digitToInt)

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
                     x <- oneOf "nt\\\"" -- one of the following characters: n, t, \, "
                     return $
                       case x of
                         'n' -> '\n'
                         't' -> '\t'
                         _ -> x -- it's either \ or " so we can return it "as is"

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
parseNumber = do 
         x <- parseNumPrefix <|> parseNum
	 return  (Number  x)
-- using bind
--   parseNumber = many1 digit >>=
--                   return . Number . read

arrayToString = \x -> concat(map(show)(x))

binDigits = arrayToString [0, 1]
octalDigits = arrayToString [0..7]
decDigits = arrayToString [0..9]
hexDigits = decDigits ++ ['a'..'f']

parseNumPrefix :: Parser Integer
parseNumPrefix = do 
        char '#'
        prefix <- oneOf "bodx"
        let validDigits = case prefix of 
                            'b' -> binDigits
                            'o' -> octalDigits
                            'd' -> decDigits
                            'x' -> hexDigits
        liftM (fst . head . readInt (toInteger (length validDigits)) (`elem` validDigits) digitToInt) (many1 (oneOf validDigits))

parseNum :: Parser Integer
parseNum = liftM read (many1 digit)
          

parseExpr :: Parser LispVal
parseExpr = parseNumber
         <|> parseString
         <|> parseAtom
