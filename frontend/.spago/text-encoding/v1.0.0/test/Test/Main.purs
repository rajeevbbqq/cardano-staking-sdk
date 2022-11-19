module Test.Main where

import Prelude       (Unit)
import Effect        (Effect)
import Test.Encoding (testEncoding)


main :: Effect Unit
main = do
  testEncoding
