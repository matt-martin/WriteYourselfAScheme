module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char  (digitToInt)
import Debug.Trace
import Test.QuickCheck
import Control.Monad.Error

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val


eval :: LispVal -> ThrowsError LispVal
eval val@(Float _) = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "if", pred, conseq, alt]) =
     do result <- eval pred
        case result of
             Bool False -> eval alt
             Bool True -> eval conseq -- deviating from tut here - throw error if not true/false 
             otherwise  -> throwError $ TypeMismatch "bool" result
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


spacesPlusString :: Parser LispVal
spacesPlusString = do
                     spaces
                     parseExpr

main :: IO ()
main = do
         expr <- getLine
         evaled <- return $ liftM show $ readExpr expr >>= eval
         putStrLn $ extractValue $ trapError evaled
         main


spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             deriving (Eq)

instance Show LispVal where show = showVal

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("bool?", isBool),
              ("number?", isNumber),
              ("string->symbol", stringToSymbol),
              ("symbol->string", symbolToString)]

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString (Atom x : _) = return $ String x
symbolToString (notAtom : _) = throwError $ TypeMismatch "atom" notAtom
symbolToString badForm = throwError $ NumArgs 1 badForm

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol (String x : _) = return $ Atom x
stringToSymbol (notSym : _) = throwError $ TypeMismatch "symbol" notSym
stringToSymbol badForm = throwError $ NumArgs 1 badForm

isBool :: [LispVal] -> ThrowsError LispVal
isBool (Bool _ : _) = return $ Bool True
isBool _            = return $ Bool False 

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber (Number _ : _) = return $ Bool True
isNumber (Float _ : _ )= return $ Bool True
isNumber notNum = return $ Bool False 

-- According to http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.3,
--   (symbol? (car '(a b)))          ===>  #t
-- However, we return false... maybe because we don't properly reduce the (car ...) expression?
isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol (Atom _ : _)  = return $ Bool True
isSymbol _             = return $ Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many parseStringChar
                char '"'
                return $ String x

parseStringChar :: Parser Char
parseStringChar = (parseEscapedChar <|> noneOf "\"")

escapeCharacters = "nt\\\""

parseEscapedChar :: Parser Char
parseEscapedChar = do
                     char '\\'
                     x <- (oneOf escapeCharacters) <?> ("valid escape character in set " ++ escapeCharacters)-- one of the following characters: n, t, \, "
                     return $
                       case x of
                         'n' -> '\n'
                         't' -> '\t'
                         _ -> x -- it's either \ or " so we can return it "as is"

parseChar :: Parser LispVal
parseChar = try parseLongChar <|> parseShortChar

parseShortChar :: Parser LispVal
parseShortChar = do
                 string "#\\"
                 x <- anyChar
                 return $ Character x

-- consider rewriting with out case match
parseLongChar :: Parser LispVal
parseLongChar = do
                  string "#\\"
                  x <- (string "space" <|> string "newline")
                  case x of
                   "space" -> return $ Character ' '
                   "newline" -> return $ Character '\n'


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

parseFloat :: Parser LispVal
parseFloat = do
        wholePart <- many1 (digit)
        char '.'
        decimalPart <- many1 (digit)
        return $ Float (fst $ head $ readFloat (wholePart ++ "." ++ decimalPart))

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
        -- based off of https://stackoverflow.com/a/5922212/4966331
        liftM (fst . head . readInt (toInteger (length validDigits)) (`elem` validDigits) digitToInt) (many1 (oneOf validDigits))

parseNum :: Parser Integer
parseNum = liftM read (many1 digit)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces
         
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- (endBy parseExpr (spaces <?> "spaces in head of dotted list")) <?> "head"
    char '.' <?> "dot in dotted list"
    spaces <?> "spaces after dot in dotted list"
    tail <- parseExpr <?> "last elem in dotted list"
--    tail <- (char '.' >> spaces >> parseExpr) <?> "tail"
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList l = unwords $ map showVal l

parseExpr :: Parser LispVal
parseExpr =
         (parseAtom <?> "atom")
         <|> ((try (parseFloat)) <?> "float")
         <|> (parseString <?> "string")
         <|> (parseNumber <?> "number")
         <|> (parseChar <?> "char")
         <|> (parseQuoted<?> "quoted")
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

prop_parseString :: String -> Bool
prop_parseString xs = let result = parse parseString "parseString quickcheck test" (show xs) in
                        case result of
                           Right(String val) -> val == xs
                           _ -> error ("Unexpected result: " ++ (show result))

-- List
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [a, b]     = return $ Bool $ a == b
eqv badArgList = throwError $ NumArgs 2 badArgList

-- Error
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
--               derving (Show)

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg  = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val



