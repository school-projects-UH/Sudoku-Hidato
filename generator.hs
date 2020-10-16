import System.Random

sample :: Int -> IO [Int]
sample n = do
  gen <- newStdGen
  return $ take n $ randoms gen

main :: IO ()
main = do
    (br:_) <- sample 1
    let r = mod br 10
    print r