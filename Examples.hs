import Prelude ()
import Stream

f1 = (input :: Stream Int) >>> output

f2 = (input :: Stream Int) >>> foreach (+2) >>> output

f3 = (input :: Stream Int) >>> group 2 0 (+) >>> output

f4 = (input :: Stream Bool) >>> foreach not >>> output

f5 = (input :: Stream Int) >>> group 3 true (\b n -> b && (n < 10)) >>> output

f5' = (input :: Stream Int) >>> foreach (<10) >>> group 3 true (&&) >>> output

f6 = (input :: Stream Int) >>> group 3 0 (+) >>> foreach (==10) >>> output

