-- This is to extract geometries and other stuffs from gaussian conical optimization
--
--

import Data.List.Split
import Data.List
import Text.Printf
import System.ShQQ

file="65/mPsb3Chained.log"
fileZZ=["66/mPsb3Chained.log","69/mPsb3Chained.log","70/mPsb3Chained.log"]
atomN=23 :: Int

numToElem = [("1","H"), ("6","C"), ("7","N")]
read2 x       = read x :: Double
mosaic x      = map words $ lines x
unmosaic x    = unlines $ map unwords x
pr x          = map (\x -> printf "%.3f" x :: String) x

main3 file = do
  f <- forces file
  [root1,root2] <- energies file
  let force = map forceAtAtom1 f
  return $ last $ transpose $ [force,root1,root2]

forces :: FilePath -> IO [String]
forces fileN = do
  b  <- readShell $ "grep -A" ++ (show $ atomN + 2) ++ " 'Forces (Hartrees/Bohr)' " ++ fileN ++ " | grep -B" ++ (show $ atomN - 1) ++ " ' " ++ (show atomN) ++ " '"
  let b'   = splitOn [["--"]] $ map words $ lines b
      b''  = map (map (\x -> [lookUpIfSafe (x!!1),x!!2,x!!3,x!!4])) b'
      b''' = map unlines $ map (map unwords) b''
  return b'''

forceAtAtom1 :: String -> String
forceAtAtom1 forceS = let 
  uno           = head $ map words $ lines forceS -- head because I want atom 1
  modul [x,y,z] = sqrt ( x**2 + y**2 + z**2 )
  components    = map read2 [uno!!1,uno!!2,uno!!3]
  fromHarBohTonNewton = 82.387
  modulo        = modul components
  in printf "%.3f" (modulo * fromHarBohTonNewton) :: String

showMe fileN = do
  thresh        <- threshold fileN
  energyGap     <- energiesGap fileN
  let label     = map (\x -> "Geometry Number:   " ++ (show x) ++ "\n") [1..]
      labPthres = zipWith (++) label thresh
      labThrene = zipWith (\x y -> x ++ "Gap Kcal: " ++ y ++ "\n") labPthres (pr energyGap)
  writeFile (fileN ++ "ResultsFromLogFile") $ unlines labThrene
  putStrLn $ unlines labThrene

threshold fileN = do
  c <- readShell $ "grep -A4 Threshold " ++ fileN
  let c'    = splitOn [["--"]] $ map words $ lines c
      c''   = map (map (\x -> [x!!2,x!!3,x!!4])) $ map tail c'
      c'''  = map unlines $ map (map unwords) c'' 
  return c'''

coordinates :: FilePath -> IO [String]
coordinates fileN = do
  b  <- readShell $ "grep -A" ++ (show $ atomN + 2) ++ " 'Coordinates (Angstroms)' " ++ fileN ++ " | grep -B" ++ (show $ atomN - 1) ++ " ' " ++ (show atomN) ++ " '"
  let b'   = splitOn [["--"]] $ map words $ lines b
      b''  = map (map (\x -> [lookUpIfSafe (x!!1),x!!3,x!!4,x!!5])) b'
      b''' = init $ map unlines $ map (map unwords) b'' -- init because there is 1 geom more
  return b'''

writeInput :: FilePath -> Int -> Double -> IO ()
writeInput fileN geomN vincolo = do
  b  <- readShell $ "grep -A" ++ (show $ atomN + 2) ++ " 'Coordinates (Angstroms)' " ++ fileN ++ " | grep -B" ++ (show $ atomN - 1) ++ " ' " ++ (show atomN) ++ " '" 
  let b'   = splitOn [["--"]] $ map words $ lines b
      b''  = map (map (\x -> [lookUpIfSafe (x!!1),x!!3,x!!4,x!!5])) b'
      b''' = map unlines $ map (map unwords) b'' 
      b''''= b'''!!(pred geomN)
  --writeFile "mPsb3Chained.com" $ (header vincolo) ++ b''' ++ (footer vincolo) 
  putStrLn $ (header vincolo) ++ b'''' ++ (footer vincolo)

energiesGap :: FilePath -> IO [Double]
energiesGap fileN = do
  a  <- readShell $ "grep '1)     EIGENVALUE' " ++ fileN ++ " | awk '{print $4}'"
  aa <- readShell $ "grep '2)     EIGENVALUE' " ++ fileN ++ " | awk '{print $4}'"
  return $ extractKcal a aa

energies fileN = do
  a  <- readShell $ "grep '1)     EIGENVALUE' " ++ fileN ++ " | awk '{print $4}'"
  aa <- readShell $ "grep '2)     EIGENVALUE' " ++ fileN ++ " | awk '{print $4}'"
  let root1 = lines a
      root2 = lines aa
  return $ [root1,root2]


lookUpIfSafe :: String -> String
lookUpIfSafe s = let Just result = lookup s numToElem
               in result

extractKcal :: String -> String -> [Double]
extractKcal a aa = let 
  valuesFirst  = lines a
  valuesSecond = lines aa
  in zipWith (\x y -> ((read2 y) - (read2 x)) * 627.509 ) valuesFirst valuesSecond

header x = "%chk=mPsb3Chained\n%mem=2000Mb\n%scr=/scratch/alessio/\n%rwf=/scratch/alessio/\n%nProc=2\n#p CASSCF(6,6,NRoot=2,NoCPMCSCF,StateAverage)/6-31g* Nosymm\n# pop=full GFINPUT guess=read \n# SCF=(MaxCycle=300,conver=6) opt=(conical,addredundant,modredundant)\n\nthis is optconical with constraints at " ++ (show x) ++ " \n\n1 1\n" 

footer x = "\n1 9 A\n1 9 " ++ (show x) ++ " F\n\n"


