import Test.QuickCheck
import NextPrime
import Data.Numbers.Primes

main :: IO (Result)
main = do
    putStrLn "Running auto-generated testing for nextPrime:is_prime"
    let propis_prime x = isPrime $ nextPrime x
          where types = x :: Int
    quickCheckResult propis_prime
