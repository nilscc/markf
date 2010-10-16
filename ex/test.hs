import Test.QuickCheck
import Test.QuickCheck.Monadic

import MarkF.Data
import qualified MarkF.Base as B

main = do
    putStrLn "----------------------------------------"
    putStrLn "Running BASE tests:"
    putStrLn ""
    runTest "onP_1" prop_onP_1
    runTest "onP_2" prop_onP_2
    runTest "onP_3" prop_onP_3
    runTest "onP_4" prop_onP_4

runTest :: (Arbitrary a, Show a) => String -> (a -> Bool) -> IO ()
runTest d t = do
    putStr $ take (maximum [length d+1, 18]) (d ++ ":" ++ repeat ' ') ++ " "
    quickCheck $ \a -> monadicIO $ assert (t a)


--------------------------------------------------------------------------------
-- BASE testing: Function application

prop_onP_1 :: ([Int],Int,[Int]) -> Bool
prop_onP_1 (p,c,n) =
    Position p c n
    ==
    B.onP (const False) (+1) (Position p c n)

prop_onP_2 :: ([Int],Int,[Int]) -> Bool
prop_onP_2 (p,c,n) =
    Position p c n
    ==
    B.onP (const True) id (Position p c n)

prop_onP_3 :: ([Int],Int,[Int]) -> Bool
prop_onP_3 (p,c,n) =
    map (+1) (p ++ [c] ++ n)
    ==
    concPos (B.onP (const True) (+1) (Position p c n))

concPos (Position a b c) = a ++ [b] ++ c

prop_onP_4 :: ([Int],Int,[Int]) -> Bool
prop_onP_4 (p,c,n) =
    map (\i -> if i == 0 then i+1 else i) (p ++ [c] ++ n)
    ==
    concPos (B.onP (== 0) (+1) (Position p c n))
