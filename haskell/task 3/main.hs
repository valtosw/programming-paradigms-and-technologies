-- part 3 (III). 13
-- Виявити, чи допускає скінчений автомат хоча б одне слово, що може бути подане у
-- вигляді xxx для деякого слова x. При ствердній відповіді навести приклад відповідного слова xxx.
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import Data.List (foldl')

data Queue a = Queue [a] [a]

emptyQueue :: Queue a
emptyQueue = Queue [][]

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue inL outL) = Queue (x : inL) outL

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [][]) = Nothing
dequeue (Queue inL (x:xs)) = Just (x, Queue inL xs)
dequeue (Queue inL []) = dequeue (Queue[] (reverse inL))

data DFA q s = DFA
  { states       :: [q]
  , alphabet     :: [s]
  , transition   :: q -> s -> q
  , initialState :: q
  , acceptStates :: q -> Bool
  }

data SearchNode q = SearchNode
  { current1 :: q
  , current2 :: q
  , current3 :: q
  , guess1   :: q
  , guess2   :: q
  } deriving (Eq, Ord, Show)

bfs :: Ord node => (node -> [(label, node)]) -> [node] -> (node -> Bool) -> Maybe [label]
bfs nextNode starts isTarget = search Set.empty initialQueue
  where
    initialQueue = foldl' (flip enqueue) emptyQueue [ (s, []) | s <- starts ]

    search visited q = case dequeue q of
        Nothing -> Nothing
        Just ((curr, path), q')
          | isTarget curr -> Just (reverse path)
          | Set.member curr visited -> search visited q'
          | otherwise ->
              let visited' = Set.insert curr visited
                  neighbors =[ (n, l:path) | (l, n) <- nextNode curr, not (Set.member n visited') ]
                  q'' = foldl' (flip enqueue) q' neighbors
              in search visited' q''

findBaseWord :: (Ord q, Ord s) => DFA q s -> Maybe [s]
findBaseWord dfa =
    let qs = states dfa
        q0 = initialState dfa
        
        initialNodes =[ SearchNode q0 g1 g2 g1 g2 | g1 <- qs, g2 <- qs ]

        isTarget (SearchNode s1 s2 s3 g1 g2) =
            s1 == g1 && s2 == g2 && acceptStates dfa s3

        step (SearchNode s1 s2 s3 g1 g2) =[ (c, SearchNode (transition dfa s1 c)
                             (transition dfa s2 c)
                             (transition dfa s3 c)
                             g1 g2)
            | c <- alphabet dfa ]

    in bfs step initialNodes isTarget

findTripleWord :: (Ord q, Ord s) => DFA q s -> Maybe [s]
findTripleWord dfa = fmap (\x -> x ++ x ++ x) (findBaseWord dfa)

data TestCase = TestCase
    { testName :: String
    , testDFA  :: DFA Int Char
    , expected :: Maybe String
    }

dfaExactly010101 :: DFA Int Char
dfaExactly010101 = DFA
    { states =[0..7], alphabet =['0', '1'], transition = tr, initialState = 0, acceptStates = (== 6) }
  where
    tr 0 '0' = 1; tr 1 '1' = 2; tr 2 '0' = 3; tr 3 '1' = 4; tr 4 '0' = 5; tr 5 '1' = 6; tr _ _ = 7

dfaEndsInAba :: DFA Int Char
dfaEndsInAba = DFA
    { states =[0..3], alphabet = ['a', 'b'], transition = tr, initialState = 0, acceptStates = (== 3) }
  where
    tr 0 'a' = 1; tr 0 'b' = 0; tr 1 'a' = 1; tr 1 'b' = 2
    tr 2 'a' = 3; tr 2 'b' = 0; tr 3 'a' = 1; tr 3 'b' = 2; tr _ _ = 0

dfaLength4 :: DFA Int Char
dfaLength4 = DFA
    { states = [0..5], alphabet = ['x']
    , transition = \q _ -> if q < 5 then q + 1 else 5
    , initialState = 0, acceptStates = (== 4)
    }

dfaEmptyString :: DFA Int Char
dfaEmptyString = DFA
    { states = [0, 1], alphabet =['y']
    , transition = \_ _ -> 1
    , initialState = 0, acceptStates = (== 0)
    }

dfaOddAOddB :: DFA Int Char
dfaOddAOddB = DFA
    { states =[0..3], alphabet = ['a', 'b'], transition = tr, initialState = 0, acceptStates = (== 3) }
  where
    tr 0 'a' = 1; tr 0 'b' = 2
    tr 1 'a' = 0; tr 1 'b' = 3
    tr 2 'a' = 3; tr 2 'b' = 0
    tr 3 'a' = 2; tr 3 'b' = 1
    tr s _   = s

dfaMod3Is1 :: DFA Int Char
dfaMod3Is1 = DFA
    { states = [0..2], alphabet = ['z']
    , transition = \q _ -> if q == 2 then 0 else q + 1
    , initialState = 0, acceptStates = (== 1)
    }

testCases :: [TestCase]
testCases =[ TestCase "DFA accepts exactly '010101'"           dfaExactly010101 (Just "010101")
    , TestCase "DFA accepts strings ending in 'aba'"    dfaEndsInAba     (Just "bababa")
    , TestCase "Length exactly 4 (Impossible)"          dfaLength4       Nothing
    , TestCase "Accepts ONLY the empty string"          dfaEmptyString   (Just "")
    , TestCase "Odd 'a's and Odd 'b's"                  dfaOddAOddB      (Just "ababab")
    , TestCase "Length is 1 modulo 3 (Impossible)"      dfaMod3Is1       Nothing
    ]

formatResult :: Maybe String -> String
formatResult Nothing   = "Nothing (No valid word exists)"
formatResult (Just "") = "Just \"\" (The empty string)"
formatResult (Just s)  = "Just " ++ show s

runTest :: TestCase -> IO ()
runTest (TestCase name dfa expectedResult) = do
    let actualResult = findTripleWord dfa
    let isPass = actualResult == expectedResult
    let status = if isPass then "[PASS]" else "[FAIL]"
    
    putStrLn $ status ++ " " ++ name
    putStrLn $ "       Expected : " ++ formatResult expectedResult
    putStrLn $ "       Actual   : " ++ formatResult actualResult
    putStrLn ""

main :: IO ()
main = do
    mapM_ runTest testCases