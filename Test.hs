import Test.QuickCheck

main :: IO ()
main = do
  let junkProp x = x > 0 ==> x > 0 
        where types = x::Int
      strProp s = ((length s) * 2) == (length $ s ++ s)
        where types = s::String
  quickCheck junkProp
  quickCheck strProp
