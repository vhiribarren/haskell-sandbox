import Test.HUnit

test1 = TestCase (assertEqual "Should return true" 2 2)


tests = TestList [test1]


main :: IO Counts
main = runTestTT tests
