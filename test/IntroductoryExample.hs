-- Example given in Text.Regex.Base

import Text.Regex.Base
import Text.Regex.Posix ((=~),(=~~))

main :: IO ()
main = do
    print b
    print c
    print d
  where
    b :: Bool
    b = ("abaca" =~ "(.)a")
    c :: [MatchArray]
    c = ("abaca" =~ "(.)a")
    d :: Maybe (String,String,String,[String])
    d = ("abaca" =~~ "(.)a")

-- will output
--
-- > True
-- > [array (0,1) [(0,(1,2)),(1,(1,1))],array (0,1) [(0,(3,2)),(1,(3,1))]]
-- > Just ("a","ba","ca",["b"])
