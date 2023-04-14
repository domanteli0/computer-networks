import Test.Tasty
import Test.Tasty.HUnit
import Utils

main :: IO ()
main = defaultMain $
  testGroup "Tests" [
      utilsSplitUrl
  ]

utilsSplitUrl = testGroup "splitUrl" [
      testCase "normal" $
        splitURL "example.com/index.html" @?= ("example.com", "/index.html")
    , testCase "domain only" $
        splitURL "example.com" @?= ("example.com", "/")
    , testCase "multiple '/'" $
        splitURL "example.com/some/resource/index.html" @?= ("example.com", "/some/resource/index.html")
  ]