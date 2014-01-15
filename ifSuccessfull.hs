-- This is to extract geometries and other stuffs from gaussian conical optimization
--
import Data.List.Split
import Data.List
import System.ShQQ
import System.Environment

main = do
  args <- getArgs
  let [file,atomNS,vincolo] = args
      atomN = read atomNS :: Int
  lastCoordCom file atomN vincolo


--file="mPsb3Chained.log"
atomN=23 :: Int

numToElem = [("1","H"), ("6","C"), ("7","N")]

lastCoordCom :: FilePath -> Int -> String -> IO ()
lastCoordCom fileN atomN vincolo = do
  b  <- readShell $ "grep -A" ++ (show $ atomN + 2) ++ " 'Coordinates (Angstroms)' " ++ fileN ++ " | grep -B" ++ (show $ atomN - 1) ++ " ' " ++ (show atomN) ++ " '" 
  let b'   = splitOn [["--"]] $ map words $ lines b
      b''  = map (map (\x -> [lookUpSafe (x!!1),x!!3,x!!4,x!!5])) b'
      b''' = last $ map unlines $ map (map unwords) b'' 
  writeFile "mPsb3Chained.com" $ (header vincolo) ++ b''' ++ (footer vincolo) 

lookUpSafe :: String -> String
lookUpSafe s = let Just result = lookup s numToElem
               in result

header x = "%chk=mPsb3Chained\n%mem=2000Mb\n%scr=/scratch/alessio/\n%rwf=/scratch/alessio/\n%nProc=2\n#p CASSCF(6,6,NRoot=2,NoCPMCSCF,StateAverage)/6-31g* Nosymm\n# pop=full GFINPUT guess=read \n# SCF=(MaxCycle=300,conver=6) opt=(conical,addredundant,modredundant)\n\nthis is optconical with constraints at i" ++ x ++ " \n\n1 1\n"

footer x = "\n1 9 A\n1 9 i" ++ x ++ " F\n\n"


