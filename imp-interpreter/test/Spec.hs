import           Test.Files (collatzPgm, primesPgm, sumPgm)

testfiles = [collatzPgm, primesPgm, sumPgm]

main :: IO ()
main = mapM_ print testfiles
