import System.Environment
import System.IO (stdout,stderr,hPutStr,hPutStrLn)

--
-- Simpleton Regular Expressions in Haskell (Slow!)
--

-- 1. An algebraic data type for regular expressions

data RE = Epsilon 
        | Any 
        | CC Bool [Char] 
        | Seq RE RE 
        | Alt RE RE 
        | Star RE 
        | Plus RE 
        | Group RE 
    deriving Show

-- 2. A simple match function to determine if a string matches a regular expression

match :: RE -> [Char] -> Bool
splits :: [Char] -> [([Char], [Char])]
add_to_prefixes :: Char -> [([Char], [Char])] -> [([Char], [Char])]
match_any_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_nonempty_split :: RE -> RE -> [([Char], [Char])] -> Bool

-- match :: RE -> [Char] -> Bool
match Epsilon s = s == ""
match Any "" = False
match Any (_ : more_chars) = more_chars == []
match (CC _ _) "" = False
match (CC True chars) (c : more_chars) = elem c chars && more_chars == []
match (CC False chars) (c : more_chars) = not (elem c chars) && more_chars == []
match (Alt r1 r2) string = match r1 string || match r2 string
match (Seq r1 r2) string = match_any_split r1 r2 (splits string)
match (Star r1) "" = True
match (Star r1) s = match_any_nonempty_split r1 (Star r1) (splits s)
match (Plus r) "" = match r ""
match (Plus r) s = match_any_split r (Star r) (splits s)
match (Group r1) s = match r1 s

-- splits :: [Char] -> [([Char], [Char])]
splits "" = [("", "")]
splits (c1:chars) = ("", c1:chars) : add_to_prefixes c1 (splits chars)

-- add_to_prefixes :: Char -> [([Char], [Char])] -> [([Char], [Char])]
add_to_prefixes c []  = []
add_to_prefixes c ((pfx, sfx) : more) = (c:pfx, sfx) : (add_to_prefixes c more)

-- match_any_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_split r1 r2 [] = False
match_any_split r1 r2 ((s1, s2) : more_splits) 
    | match r1 s1 && match r2 s2     = True
    | otherwise                      = match_any_split r1 r2 more_splits 

-- match_any_nonempty_split :: RE -> RE -> [([Char], [Char])] -> Bool
match_any_nonempty_split r1 r2 [] = False
match_any_nonempty_split r1 r2 ((s1, s2) : more) 
    | s1 /= "" && match r1 s1 && match r2 s2     = True
    | otherwise                                  = match_any_nonempty_split r1 r2 more 


-- 3.  A parser to convert text into regular expressions

parseRE :: [Char] -> Maybe (RE, [Char])
parseSeq :: [Char] -> Maybe (RE, [Char])
parseItem :: [Char] -> Maybe (RE, [Char])
parseElement :: [Char] -> Maybe (RE, [Char])
parseClassItems :: [Char] -> Maybe ([Char], [Char])
parseCCitem :: [Char] -> Maybe (Char, [Char])
parseChar :: [Char] -> Maybe (RE, [Char])

isMetachar :: Char -> Bool

-- parseChar :: [Char] -> Maybe (RE, [Char])
parseChar [] = Nothing
parseChar ('[':'^':more) =
    case parseClassItems(more) of
        Just (items, ']':yet_more) -> Just (CC False items, yet_more)
        _ -> Nothing
parseChar ('[':more) =
    case parseClassItems(more) of
        Just (items, ']':yet_more) -> Just (CC True items, yet_more)
        _ -> Nothing
parseChar ('\\':c:s)
    | isMetachar c   = Just (CC True [c], s)
    | otherwise      = Nothing
parseChar (c:s)
    | isMetachar c   = Nothing
    | otherwise      = Just (CC True [c], s)

-- isMetachar :: Char -> Bool
isMetachar c = elem c "|*()\\.?+["

-- parseCCitem :: [Char] -> Maybe (Char, [Char])
parseCCitem "" = Nothing
parseCCitem ('-':more) = Nothing
parseCCitem (']':more) = Nothing
parseCCitem "\\" = Nothing
parseCCitem ('\\':c:more) = Just (c, more)
parseCCitem (c: more) = Just (c, more)

-- parseClassItems :: [Char] -> Maybe ([Char], [Char])
parseClassItems [] = Nothing
parseClassItems (']':more) = Just([], ']':more)
parseClassItems s =
    case parseCCitem s of 
        Just (item, '-':more) -> 
            case parseCCitem more of
                Just (item2, yet_more) ->
                    case parseClassItems yet_more of
                        Just (items, furthermore) -> Just ([item .. item2] ++ items, furthermore)
                        _ -> Nothing
                _ -> Nothing
        Just (item, more) ->
            case parseClassItems more of
                Just (items, furthermore) -> Just (item:items, furthermore)
                _ -> Nothing
        _ -> Nothing

-- parseElement :: [Char] -> Maybe (RE, [Char])
parseElement ('.':more) = Just (Any, more)
parseElement ('(':more) =
    case parseRE(more) of
        Just (re, ')':yet_more) -> Just(Group re, yet_more)
        _ -> Nothing
parseElement s = parseChar s

-- parseItem :: [Char] -> Maybe (RE, [Char])
parseItem s =
    case parseElement(s) of
        Just (re, '*':more) -> Just (Star re, more)
        Just (re, '?':more) -> Just (Alt re Epsilon, more)
        Just (re, '+':more) -> Just (Plus re, more)
        Just (re, more) -> Just (re, more)
        _ -> Nothing

extendSeq :: (RE, [Char]) -> Maybe (RE, [Char])

-- parseSeq :: [Char] -> Maybe (RE, [Char])
parseSeq s =
    case parseItem(s) of
        Just (r, more_chars) -> extendSeq(r, more_chars)
        _ -> Nothing

-- extendSeq :: (RE, [Char]) -> Maybe (RE, [Char])
extendSeq (e1, after1) =
    case parseItem(after1) of 
        Just(e2, more) -> extendSeq(Seq e1 e2, more)
        _ -> Just(e1, after1)

extendRE :: (RE, [Char]) -> Maybe (RE, [Char])

-- parseRE :: [Char] -> Maybe (RE, [Char])
parseRE s =
    case parseSeq(s) of
        Just (r, more_chars) -> extendRE(r, more_chars)
        _ -> Nothing

-- extendRE :: (RE, [Char]) -> Maybe (RE, [Char])
extendRE (e1, []) = Just (e1, [])
extendRE (e1, '|' : after_bar) =
    case parseSeq(after_bar) of 
        Just(e2, more) -> extendRE(Alt e1 e2, more)
        _ -> Nothing
extendRE(e1, c:more) = Just (e1, c:more)

parseMain :: [Char] -> Maybe RE
parseMain s = case parseRE s of 
    Just (e, []) -> Just e
    _ -> Nothing

-- 4.  Searching for matching lines in a file

matches :: RE -> [[Char]] -> [[Char]]
matches re lines = filter (match re) lines

matching :: [Char] -> [[Char]] -> [[Char]]
matching regexp lines = case parseMain regexp of
    Just r -> matches r lines
    _ -> []

-- 5.  Command line interface

main = do
  [regExp, fileName] <- getArgs
  srcText <- readFile fileName
  hPutStr stdout (unlines (matching regExp (lines srcText)))





