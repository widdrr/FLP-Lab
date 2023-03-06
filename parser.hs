import Control.Applicative
import Data.Char

newtype Parser a = Parser { apply :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []   -- imposibil de parsat șirul vid
    go (c:input)
      | p c = [(c, input)]   -- dacă predicatul ține, întoarce c și restul șirului de intrare
      | otherwise = []       -- în caz contrar, imposibil de parsat

--- | Acceptă orice caracter
anychar :: Parser Char
anychar = satisfy (\_ -> True)

--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char c = satisfy ( (==) c)

--- | acceptă o cifră
digit :: Parser Char
digit = satisfy isDigit

--- | acceptă un spațiu (sau tab, sau sfârșit de linie -- vedeți funcția din Data.Char )
space :: Parser Char
space = satisfy isSpace

--- | succes doar dacă am șirul de intrare este vid 
endOfInput :: Parser ()
endOfInput  = Parser go
  where
    go "" = [((), "")]
    go _ = []

instance Functor Parser where
    fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

parse :: Parser a -> String -> Either String a
parse p s = handle (apply (p <* endOfInput) s) where
    handle [(x,"")] = Right x 
    handle _ = Left "Sirul de intrare nu a fost complet consumat sau parsare ambigua"


instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | (a, resta) <- apply pa input, (b, restb) <- apply (k a) resta])

cifraSemn :: Parser Int
cifraSemn = do
    semn <- satisfy (\x -> elem x "+-")
    d <- digit
    if semn == '-'
        then return (-(digitToInt d))
    else return (digitToInt d)

string :: String -> Parser String
string "" = return []
string (x:xs) = do
    c <- char x
    cs <- (string xs)
    return (x:cs)


instance Alternative Parser where
    empty = Parser (const [])
    p <|> p' = Parser (\input -> apply p input ++ apply p' input)

naiveNatural :: Parser Int
naiveNatural = undefined

-- | Elimină zero sau mai multe apariții ale lui `space`
whiteSpace :: Parser ()
whiteSpace = do
    _ <- many (space)
    return ()

-- | parses a natural number (one or more digits)
nat :: Parser Int
nat = read <$> some digit
--or custom function for string->int conversion

-- | aplică un parser, și elimină spațiile de după
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace
-- lexeme p = do
--     r <- p
--     _ <- whiteSpace
--     return r

-- | parses a natural number and skips the space after it
natural :: Parser Int
natural = nat <* whiteSpace

-- | Parses the string and skips whiteSpace after it
symbol :: String -> Parser String
symbol s = (string s) <* whiteSpace

-- | Parses the string, skips whiteSpace, returns unit
reserved :: String -> Parser ()
reserved s = (symbol s) *> pure ()

-- | parsează virgulă, eliminând spațiile de după
comma :: Parser ()
comma = reserved ","

-- | parsează argumentul intre paranteze rotunde
--   elimină spațiile de după paranteze
parens :: Parser a -> Parser a
parens p = (char '(') *> p <* (char ')') <* whiteSpace 

-- | parsează argumentul intre paranteze pătrate
--   elimină spațiile de după paranteze
brackets :: Parser a -> Parser a
brackets p = (char '[') *> p <* (char ']') <* whiteSpace 

-- | una sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = (:) <$> p <*> many (comma *> p)



-- | zero sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep :: Parser a -> Parser [a]
commaSep p = commaSep1 p <|> pure []

-- | date fiind parsere pentru prima literă si pentru felul literelor următoare
--   scrieți un parser pentru un identificator
ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter = (:) <$> identStart <*> many identLetter

-- | ca mai sus, dar elimină spatiile de după
identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)
