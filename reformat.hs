import Text.Printf
import Data.List.Split
import Data.List
import System.ShQQ
import System.Process
import System.Directory
import qualified Data.Set as S


bohrToAng = 1.889725989

angToBohr = 0.529177249

mkUniq = S.toList . S.fromList

mosaic = map words . lines

unmosaic = unlines . map unwords

pr x = printf "%8.6e" x :: String

read2 x = read x :: Double

outputName = "lol.cube"

main = do 
       shellWork
       valuesAndCoor <- readFile "thisOne"
       coordsS <- readFile "coords"
       let [lineS, coords] = map mosaic [valuesAndCoor,coordsS]
           all@[xs, ys, zs, values] = transpose lineS
           a          = map mkUniq [xs, ys, zs] -- remove duplicates
           b@[xsC, ysC, zsC] = map (map show) $ map sort $ map (map read2) a -- sort the Doubles
           allCube    = [[x,y,z] | x<-xsC , y <- ysC, z<-zsC]
           ciao x     = map show $ map read2 x --dovevo ordinare NON come string e poi rendere uguali ancora le due stringhe sia di allcube che di lineSTupl
           tuplaZ   x = ([ciao x!!0, ciao x!!1, ciao x!!2],x!!3)
           lineSTupl  = map tuplaZ lineS
           resultCube = map (\x -> efficient lineSTupl x) allCube
--           strings = unmosaic $ chunksOf 6 $ map pr $ map read2 $ concat $ a
       --  HEADER            
       writeFile outputName $ "file in pseudo-Gaussian cube format\nDensity\n"
       --  Box Vectors => 22  -28.483152  -20.233584  -12.668437
       let boxDimensions = map (show . length) a
           atomN         = show $ length coords
           sortedCoordd  = map sort $ map (map read2) a
           originBox     = map show $ map head sortedCoordd
           stepBox       = show $ abs $ (sortedCoordd!!0!!0) - (sortedCoordd!!0!!1)
           boxS          = createBox atomN originBox boxDimensions stepBox
       appendFile outputName boxS 
       --  COORDINATES =>  7 0.000000  -12.683578       3.150706      -0.779499
       let rightFormatCoordinates = map changeAtomTypeToNumberFormat coords
       appendFile outputName $ unmosaic rightFormatCoordinates
       --  VALUES      =>  0.000000e0 0.000000e0 6.301501e-2 6.315708e-2 6.234798e-2 0.000000e0
       appendFile outputName $ unmosaic $ chunksOf 6 $ map pr $ map read2 resultCube
       system "rm thisOne coords"
       writeFile "vmdScript" vmdScript
       system $ "vmd " ++ outputName ++ " -e vmdScript"
       system "rm vmdScript"

createBox :: String -> [String] -> [String] -> String -> String
createBox atomN originBox boxDimensions stepBox = let
  spaces = "   "
  zeros  = "0.00000"
  line1  = intercalate spaces [atomN, (unwords originBox)]
  line2  = intercalate spaces [boxDimensions!!0, stepBox, zeros, zeros]
  line3  = intercalate spaces [boxDimensions!!1, zeros, stepBox, zeros]
  line4  = intercalate spaces [boxDimensions!!2, zeros, zeros, stepBox]
  in (intercalate "\n" [line1, line2, line3, line4]) ++ "\n"

changeAtomTypeToNumberFormat coordsLine@(hea:rest) = let
  labels = [ ('N','7'), ('C','6'), ('H','1') ]
  thisLineLabel = head hea
  Just number = lookup thisLineLabel labels
  in [number]:rest

efficient :: [([String], String)] -> [String] -> String
efficient lineS singleCube = let 
     a = lookup singleCube lineS
     in case a of Just value -> value
                  Nothing    -> "0.00000"

filePOT  = "minPOT.out"
fileGRID = "minGRID.out"
vmdScript = "menu graphics on\nmol modstyle 0 0 CPK 1.000000 0.300000 20.000000 17.000000\nmol color Name\nmol representation CPK 1.000000 0.300000 20.000000 17.000000\nmol selection all \nmol material Opaque\nmol addrep 0\nmol modcolor 1 0 Volume 0\nmol modstyle 1 0 Surf 4.400000 0.000000\nmol modmaterial 1 0 Transparent\ncolor scale midpoint 0.500000\ncolor scale min -0.290000\ncolor scale max 1.000000"

shellWork = do 
     system $ "grep -A5 \"Electric Potential:  centre no.\" " ++ filePOT ++ " | awk 'NR%7==1' | awk  -F \"(\" '{print $2}' | sed s/\\)// | awk -F \",\" '{print $1, $2, $3}' > temp1"
     system $ "grep -A5 \"Electric Potential:  centre no.\" " ++ filePOT ++ "| awk 'NR%7==6' | awk '{print $2}' > temp2"
     system "paste temp1 temp2 > thisOne"
     system $ "grep -A25 \"**** Cartesian Coordinates / Bohr, Angstrom ****\" " ++ fileGRID ++ " | tail -22 | awk '{print $2, \"0.0\", $3,$4,$5}' > coords"
     system "rm temp1 temp2"


