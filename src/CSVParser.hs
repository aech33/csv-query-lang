{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module CSVParser where
import Control.Monad
import Control.Applicative
import Data.List (intercalate)

isSpace '\t' = True
isSpace ' ' = True
isSpace _ = False

-- Don't care about consume
newtype Parse a = Parse { (%%) :: String -> (Maybe a, String) }
  deriving Functor

instance Applicative Parse where
  pure v = Parse $ \s -> (Just v,s)
  (<*>) = ap

instance Monad Parse where
  mx >>= f = Parse $ \s ->
    let (x, s') = mx %% s in
      case x of
        Just x -> f x %% s'
        Nothing -> (Nothing, s')

instance Alternative Parse where
  fa <|> fb = Parse $ \s ->
    case fa %% s of
      v@(Just _, _) -> v
      (Nothing, _) -> fb %% s
  empty = Parse $ \s -> (Nothing, s)

type CSV = [[String]]

lexeme :: Parse ()
lexeme = Parse $ \case
  "" -> (Just (), "")
  (c:cs)
    | isSpace c -> lexeme %% cs
    | otherwise -> (Just (), c:cs)

sat :: (Char -> Bool) -> Parse ()
sat p = Parse $ \case
  c:s | p c -> (Just (), s)
  s -> (Nothing, s)

comma :: Parse ()
comma = sat (== ',')

br :: Parse ()
br = sat (== '\n')

overline :: Parse ()
overline = void (lexeme >> br) <|> (lexeme >> eof)

peekOver :: Parse ()
peekOver = Parse $ \ s -> case s of  
  "" -> (Just (), "")
  '\n':_ -> (Just (), s)
  _ -> (Nothing, s)

cell' :: Parse String
cell' = Parse $ \s -> case s of
  "" -> (Nothing, "")
  c:cs | c == '\n' || c == ',' -> (Just "", s)
       | otherwise -> case cell' %% cs of
            (Just str, s') -> (Just (c:str), s')
            (Nothing,  s') -> (Just (c:""),  s')

cell :: Parse String
cell = reverse . dropWhile isSpace . reverse . dropWhile isSpace <$> cell'

csv :: Int -> Parse CSV
csv n = do 
  res <- lexeme >> many line <* overline
  if res == [[""]] then pure [] else pure $ filter ((== n) . length)res

many' :: Parse a -> Parse [a]
many' p = 
  (do x <- p
      xs <- many' p
      pure $ x : xs) <|>
  pure []

emptyline :: Parse [String]
emptyline = lexeme >> br >> pure []

line :: Parse [String]
line =
  (lexeme >> (cell >>= pure . (: [])) <* lexeme <* overline) <|> do
    lexeme
    c <- cell
    comma
    rest <- line
    pure $ c:rest

eof :: Parse ()
eof = Parse $ \case
  "" -> (Just (), "")
  s -> (Nothing, s)

ppCSV :: CSV -> String 
ppCSV [] = "" 
ppCSV (line:lines) = intercalate "," line ++ "\n" ++ ppCSV lines
