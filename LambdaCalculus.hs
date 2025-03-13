  import Control.Applicative
  import Data.Char (isAlpha, isAlphaNum, isSpace)

  import qualified Data.Map as M
  import Control.Monad.State

  import System.IO (readFile)
  import System.Environment (getArgs)
  import Control.Monad (forM_)

  -- PARSER
  
  newtype Parser a = P (String -> [(a, String)])

  parse :: Parser a -> String -> [(a, String)]
  parse (P f) = f

  next :: Parser Char
  next = P (\cs -> case cs of
                     [] -> []
                     (c:cs) -> [(c,cs)])

  instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser B
    fmap f p = P (\cs -> case parse p cs of
                           [] -> []
                           [(c, cs')] -> [(f c, cs')])

  instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P (\cs -> [(a, cs)])

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\cs -> case parse pf cs of
                            [] -> []
                            [(f, cs')] -> parse (fmap f px) cs')

  instance Monad Parser where
    -- return :: a -> Parser a
    -- return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\cs -> case parse p cs of
                          [] -> []
                          [(c, cs')] -> parse (f c) cs')

  instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    pf <|> pg = P (\cs -> case parse pf cs of
                              [] -> parse pg cs
                              [(c, cs')] -> [(c, cs')])

    -- many :: Parser a -> Parser [a]
    -- many x = some x <|> pure []

    -- some :: Parser a -> Parser [a]
    -- some x = pure (:) <*> x <*> many x

  sat :: (Char -> Bool) -> Parser Char
  sat f = do {
              c <- next;
              if (f c) then (return c) else empty
            }

  alpha :: Parser Char
  alpha = sat isAlpha

  alphanum :: Parser Char
  alphanum = sat isAlphaNum

  char :: Char -> Parser Char
  char c = sat (== c)

  string :: String -> Parser String
  string [] = return []
  string (c:cs) = do {
                char c;
                string cs;
                return (c:cs)
              }

  space :: Parser ()
  space = do {
            many (sat isSpace);
            return ()
          }

  token :: Parser a -> Parser a
  token p = do {
              space;
              v <- p;
              space;
              return v
            }

  symbol :: String -> Parser String
  symbol xs = token (string xs)

  paren :: Parser a -> Parser a
  paren p = do {
              char '(';
              v <- p;
              char ')';
              return v
            }

  -- LAMBDA CALCULUS

  data Term a = V a | L a (Term a) | A (Term a) (Term a)

  instance Show (Term Char) where
      show :: Term Char -> String
      show (V a) = a:[]
      show (L a m) = case m of
                       (L b n) -> "(λ" ++ [a,b] ++ "." ++ show n ++ ")"
                       _ -> "(λ" ++ [a] ++ "." ++ show m ++ ")"
      show (A m n) = show m ++ show n

  var :: Parser (Term Char)
  var = do {
          x <- alpha;
          return (V x)
        }

  lam :: Dictionary -> Parser (Term Char)
  lam table = do {
                char '\\';
                xs <- some alpha;
                char '.';
                m <- (term table);
                return (foldr L m xs)
              }

  app :: Dictionary -> Parser (Term Char)
  app table = do {
                ms <- many $ token (expr table);
                return (foldl1 A ms)
              }

  term :: Dictionary -> Parser (Term Char)
  term table = do {
                 m <- (lam table) <|> (app table) <|> (expr table);
                 return m
               }

  expr :: Dictionary -> Parser (Term Char)
  expr table = do {
                 m <- paren (term table) <|> token (name table) <|> var;
                 return m
               }

  name :: Dictionary -> Parser (Term Char)
  name table = do {
                 x <- ident;
                 case (M.lookup x table) of
                   Nothing -> empty;
                   Just m -> return m;
               }

  fv :: Term Char -> [Char]
  fv (V a) = [a]
  fv (L a m) = filter (/= a) (fv m)
  fv (A m n) = (fv m) ++ (fv n)

  newVar :: Char -> [Char] -> Char
  newVar c used = head ([c' | c' <- ['a'..'z'], notElem c' used] ++ [c])

  ren :: Term Char -> Char -> Char -> Term Char
  ren (V a) x y = V (if a == x then y else a)
  ren (L a m) x y = L (if a == x then y else a) (ren m x y)
  ren (A m n) x y = A (ren m x y) (ren n x y)

  sub :: Term Char -> Term Char -> Char -> Term Char
  sub m n x = case m of
                V a -> if a == x then n else (V a)
                L a p -> if a == x
                         then (L a p)
                         else if elem a (fv n)
                              then let a' = newVar a (a : fv n ++ fv p)
                                   in L a' (sub (ren p a a') n x)
                              else L a (sub p n x)
                A p q -> A (sub p n x) (sub q n x)

  redex :: Term Char -> Bool
  redex (V a) = False
  redex (L a m) = redex m
  redex (A m n) = case m of
                    (L a m') -> True
                    _ -> redex m || redex n

  cbn :: Term Char -> Term Char
  cbn (V a) = V a
  cbn (L a m) = L a (cbn m)
  cbn (A m n) = case m of
                  (L a m') -> sub m' n a
                  _ -> if redex m
                       then A (cbn m) n
                       else A m (cbn n)

  eval :: Term Char -> Term Char
  eval (V a) = V a
  eval (L a m) = L a (eval m)
  eval (A m n) = case m of
                   (L a m') -> eval (sub m' n a)
                   _ -> if redex m
                        then eval (A (cbn m) n)
                        else A m (eval n)


  run :: String -> Maybe (Term Char)
  run cs = case (parse script cs) of
             [] -> case (parse (term M.empty) cs) of
                     [] -> Nothing
                     [(m,_)] -> Just (eval m)
             [(table,_)] -> if (M.null table)
                            then case (parse (term table) cs) of
                                   [] -> Nothing
                                   [(m,_)] -> Just (eval m)
                            else case (M.lookup "main" table) of
                                   Nothing -> Nothing
                                   Just m -> Just (eval m)


  -- LANGUAGE

  type Dictionary = M.Map String (Term Char)
  type ParserState a = StateT Dictionary Parser a

  instance MonadPlus Parser where
    mzero = empty
    mplus p1 p2 = p1 <|> p2

  ident :: Parser String
  ident = do {
            cs <- some (sat (\x -> (not . isSpace) x && (notElem x ['(',')',';'])));
            return cs;
          }

  cmd :: ParserState ()
  cmd = do {
           env <- get;
           k <- lift $ token ident;
           lift $ token (char '=');
           v <- lift $ token (term env);
           lift $ token (char ';');
           put (M.insert k v env);
         }

  comment :: ParserState ()
  comment = do {
              env <- get;
              lift $ symbol "//";
              lift $ some (sat (/= '\n'));
              put env;
            }

  script :: Parser Dictionary
  script = execStateT (many (cmd <|> comment)) M.empty;

  main :: IO ()
  main = do {
          files <- getArgs;
          forM_ files $ (\file -> do {
                                    content <- readFile file;
                                    let result = run content in
                                      case result of
                                          Just parsed -> print parsed
                                          Nothing -> putStrLn $ "Failed to parse: " ++ file
                                  })
        }
