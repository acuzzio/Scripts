import Data.List.Split
import Data.List
import System.Environment (getArgs)
import Text.Printf

--fn  = "geom001.xyz"
fn  = "geom000.xyz"
fnO = "geom000.modif.xyz"

toBeRemoved = map show [3583,3582,3603,3581,3594,3618,3617,3616,3580,3601,3602,3599,3600,3579,3578,3597,3598,3577,3593,3592,3612,3610,3611,3613,3614,3615]
sam = map show [1,2,3,4]
toAdd = map show [3583,3582]

main = do
  gotArgs <- getArgs
  if gotArgs == []
     then do
       putStrLn "\nTo be used like this:\n\n./CancelBetaIonone gromacsGeometryfile gromacsVelocityfile\n\n"
     else do
       let [xyz,velocity] = gotArgs
       recreate xyz velocity

recreate geom velo = do
  a <- readFile geom 
  v <- readFile velo
  let [b,vel]     = map mosaic [a,v]
      atomN       = head $ head b
      noheader    = tail b 
      --firstPart   = map (\x -> fst $ splitAt 6 x) noheader 
      secondPart  = map (\x -> snd $ splitAt 6 x) noheader 
      wrongNumer  = removeAtoms toBeRemoved noheader
      wrongNumerV = removeAtoms toBeRemoved vel
      replaced    = replacecarbonaddHidro wrongNumer
      replacedV   = (take 3576 wrongNumerV) ++ [["3582","0","0","0"],["3583","0","0","0"]] ++ (drop 3576 wrongNumerV)
      hMoved      = moveatom 3578 3588 replaced
      hMovedV     = moveatom 3578 3588 replacedV
      renumbered  = renumber hMoved
      renumberedV = unmosaic $ renumbervelo hMovedV
      newName     = (reverse $ tail $ dropWhile (\x -> x /= '.') $ reverse geom) ++ ".new.xyz"
      newNameV    = (reverse $ tail $ dropWhile (\x -> x /= '.') $ reverse velo) ++ ".new.xyz"
  writeFile newName  renumbered
  writeFile newNameV renumberedV

moveatom :: Int -> Int -> [[String]] -> [[String]]
moveatom from to molecule = let
  i1 = from - 1
  i2 = to -1
  onethatImove = molecule!!i1
  listWithout  = take i1 molecule ++ drop (i1+1) molecule
  in take i2 listWithout ++ [onethatImove] ++ drop i2 listWithout

replacecarbonaddHidro wrongNumer = let
  number = "3584"
  newH1  = "3582"
  newH2  = "3583"
  lil x  = printf "%8.6f" $ (read2 x) + 0.25
  lol x  = printf "%8.6f" $ (read2 x) - 0.25
  change number lineZ = if number == lineZ!!0 then [[newH1,"HR",lineZ!!2,lil (lineZ!!3),lineZ!!4,"2014",lineZ!!0],[newH2,"HR",lineZ!!2, lol (lineZ!!3),lineZ!!4,"2014",lineZ!!0],[lineZ!!0,"C3R",lineZ!!2,lineZ!!3,lineZ!!4,"2013",newH1,newH2,lineZ!!7,lineZ!!8]] else [lineZ]
  in concat $ map (change number) wrongNumer

rimpiazza :: [(String,String)] -> String -> String
rimpiazza pairs due = let
  filtro = filter (\x -> fst x == due) pairs
  in if filtro == [] 
        then due
        else snd $ head filtro

printWithourRenumber noheader = let
  firstPart    = map (\x -> fst $ splitAt 6 x) noheader
  secondPart   = map (\x -> snd $ splitAt 6 x) noheader
  attachAgain  = reconstruc firstPart secondPart
  line         = map printXyzLineWell attachAgain
  len          = show $ length line
  lineHead     = len : line
  in unlines lineHead

renumbervelo noheader = let
  indexes      = map show [1..]
  in zipWith (:) indexes (map tail noheader)

--renumber :: [[String]] -> [[String]] 
renumber noheader = let
  firstPart    = map (\x -> fst $ splitAt 6 x) noheader
  secondPart   = map (\x -> snd $ splitAt 6 x) noheader
  indexes      = map show [1..]
  changesToDo  = zip (map (\x -> x!!0) firstPart) indexes
  noDoubles    = filter (\x -> fst x /= snd x) changesToDo
  newNumber (newI,oldLine) = newI : tail oldLine  
  changeFirst  = map newNumber $ zip indexes firstPart
  changeSecond = rimpiazzaAll noDoubles secondPart
  attachAgain  = reconstruc changeFirst changeSecond
  line         = map printXyzLineWell attachAgain
  len          = show $ length line 
  lineHead     = len : line   
  in unlines lineHead 

rimpiazzaAll :: [(String,String)] -> [[String]] -> [[String]]
rimpiazzaAll pairs wrongNumer = map (map (rimpiazza pairs)) wrongNumer 

-- let (a,b) = it
removeAtoms :: [String] -> [[String]] -> [[String]]
removeAtoms is all = filter (\x -> not $ elem (x!!0) is) all

reconstruc f s = let
  connectionToString = printConnectionsWell s
  well = zipWith (\x y -> x ++ [y]) f connectionToString
  in well

read2 :: String -> Double
read2 x = read x :: Double

readI :: String -> Int 
readI x = read x :: Int 

fromIntegral2 :: Int -> Double
fromIntegral2 x = fromIntegral x :: Double

writeF :: [[[String]]] -> String
writeF x  = intercalate "  \n"$ map unlines $ map (map unwords) x

printZ x    = (printf "%6s" x) :: String

printConnectionsWell x = map (concat . (map printZ)) x

printXyzLineWell :: [String] -> String
printXyzLineWell (x1:x2:x3:x4:x5:x6:connect:[]) = printf "%6s  %-3s%12s%12s%12s%6s%s" x1 x2 x3 x4 x5 x6 connect

compress :: Eq a => [a] -> [a] 
compress = map head . group

mosaic = map words . lines

unmosaic = unlines . map unwords

