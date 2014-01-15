import Data.List.Split
import Data.List
import System.ShQQ

file="mPsb3Chained.log"
atomN=23 :: Int

numToElem = [("1","H"), ("6","C"), ("7","N")]

threshold fileN = do
  c <- readShell $ "grep -A4 Threshold " ++ fileN
  let c' = splitOn [["--"]] $ map words $ lines c
  return c' 

coordinates fileN = do
  b  <- readShell $ "grep -A" ++ (show $ atomN + 2) ++ " 'Coordinates (Angstroms)' " ++ fileN ++ " | grep -B" ++ (show $ atomN - 1) ++ " ' " ++ (show atomN) ++ " '"
  let b'   = splitOn [["--"]] $ map words $ lines b
      b''  = map (map (\x -> [lookUpSafe (x!!1),x!!3,x!!4,x!!5])) b'
      b''' = init $ map unlines $ map (map unwords) b''
  return b'''

energies :: FilePath -> IO [Double]
energies fileN = do
  a  <- readShell $ "grep '1)     EIGENVALUE' " ++ fileN ++ " | awk '{print $4}'"
  aa <- readShell $ "grep '2)     EIGENVALUE' " ++ fileN ++ " | awk '{print $4}'"
  return $ extractKcal a aa

lookUpSafe :: String -> String
lookUpSafe s = let Just result = lookup s numToElem
               in result

extractKcal :: String -> String -> [Double]
extractKcal a aa = let 
  valuesFirst  = lines a
  valuesSecond = lines aa
  read2 x = read x :: Double
  in zipWith (\x y -> ((read2 y) - (read2 x)) * 627.509 ) valuesFirst valuesSecond
