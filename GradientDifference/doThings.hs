
import Data.List.Split
import Text.Printf

a = [[1.0,1.0,1.0],[1.0,2.0,1.0],[1.0,3.0,1.0]]
b = [[1.0,1.0,1.0],[1.0,1.0,0.0],[1.0,2.0,1.0],[1.0,3.0,1.0]]

skelN = 23 :: Int
withoutN = 37 :: Int
totalN = 39 :: Int

skel    = ("skel", skelN)
without = ("without", withoutN)
total   = ("total", totalN)

boh2ang = 0.529177249
ang2boh = 1.889725989

type Comp     = [Double]
type Grad     = [Comp]
type Atom     = [Double]
type Geom     = [Atom]
type Work     = Double
type Match    = [(Int,Int)]
type Input    = (String, Int)
type Vect     = [[Double]]

coorName x = x ++ "Coor"

gradName x = x ++ "Grad"

workName x = x ++ "Work"

main = do
  difference total without
  -- difference total skel

difference a b = do
  ((g1,g2),(c1,c2)) <- readFour a b
  c2S <- readGeomStr b
  let match           = calculateMatch (c1!!0) (c2!!0)
      aMatchIndexes   = map fst match
      bMatchIndexes   = map snd match
      -- This takes care of gradient difference vector 
      differenceG     = zipWith (\x y -> vecMatchDiff x y match) g1 g2
      -- To make a molcas file I need a matched STRING geometry (with atomtypes)
      geomString list = map (\x -> list !! x) bMatchIndexes
      geomMatchString = map (unlines . geomString) c2S 
      matchLen        = length match
      geomMatchFinal  = map (\x -> (show matchLen) ++ "\n\n" ++ x) geomMatchString
      geomMatchFinalA = map fromBohrToAngstromStringVector geomMatchFinal
      -- I also need some calculus from matched geometry, so I pass to float
      matchGeomF  = fromStringstoGeom geomMatchString 
      [outName,outNameP] = getNames a b
      -- Displacement versors
      displacementsV  = zipWith displacementVersor (map fst $ doRight matchGeomF) (map snd $ doRight matchGeomF)
--  writeMoldenForceFile geomMatchFinalA displacementsV matchLen outName
      scalProduct = scalProdMulti differenceG displacementsV 
      projection  = zipWith (multiplyVecScal) displacementsV scalProduct
  return $ (map (map (boh2ang*)) (c2 !! 1), (fromStringstoGeom2 geomMatchFinalA) !! 1)
--  return $ geomMatchFinalA !! 1
--  writeMoldenForceFile geomMatchFinalA differenceG matchLen outName
--  writeMoldenForceFile geomMatchFinalA projection matchLen outNameP

------------------------
-- insert dummy atoms --
------------------------

-- insertDummyAtoms :: Grad -> Geom -> Geom -> (Grad,Geom)
insertDummyAtoms coor template = let
  booleans = map (\x -> any (atomEqual x) coor) template
  indexes  = map snd $ filter (\x -> fst x == False) $ zip booleans [0..]
  in indexes

insertDummies :: Grad -> [Int] -> Grad
insertDummies [] _ = []
insertDummies (xs:xss) is = 

insertOne :: Grad -> [Double] -> Int -> Grad
insertOne xs x i = (take i xs) ++ [x] ++ (drop i xs)

------------
-- Vectors --
-------------

multiplyVecScal :: Vect -> Double -> Vect
multiplyVecScal xs a = chunksOf 3 $ fmap (a*) (concat xs)

scalProdMulti :: [Grad] -> [Geom] -> [Double]
scalProdMulti a b  = zipWith scalProd a b

scalProd :: Grad -> Geom -> Double
scalProd a b = sum $ zipWith (*) (concat a) (concat b)

modulVec :: Vect -> Double
modulVec xss = sqrt $ sum $ map (\x -> x ** 2) $ concat xss

displacementVersor :: Geom -> Geom -> Vect
displacementVersor a b = let
  displVector  = zipWithOverVec (-) b a
  modTot       = modulVec displVector
  in chunksOf 3 $ map (\x -> x / modTot) (concat displVector)

fromBohrToAngstromStringVector :: String -> String
fromBohrToAngstromStringVector st = let
  string = mosaic st
  transformed = bToAHelper string
  in unlines $ map unwords transformed

bToAHelper :: [[String]] -> [[String]] 
bToAHelper []     = []
bToAHelper (x:xs) = case length x of
  0 -> [] : bToAHelper xs 
  1 -> x : bToAHelper xs
  4 -> transform x : bToAHelper xs

transform [atomT,x,y,z] = [atomT, boh2angString x, boh2angString y, boh2angString z] 
  
boh2angString x = p $ boh2ang * (read2 x) 

----------------
-- Match part --
----------------

calculateMatch :: Geom -> Geom -> Match
calculateMatch a b = let
  mappa = map (calcMatchAtom a) b
  in filter (\x -> fst x /= (-1)) $ zip mappa [0..]

calcMatchAtom :: Geom -> [Double] -> Int
calcMatchAtom geom tr = let 
  tupla = zip (map (atomEqual tr) geom) [0..]
  trueS = filter (\x -> fst x == True) tupla
  head  = safeHead trueS 
  in if head == (False,0) 
       then -1
       else snd head

atomEqual :: Atom -> Atom -> Bool
atomEqual a b = atomDistance a b < 0.2

atomDistance :: Atom -> Atom -> Double
atomDistance a b = sum $ map (\x -> x ** 2) $ zipWith (-) b a

safeHead [] = (False,0)
safeHead x  = head x

vecMatchDiff :: Grad -> Grad -> Match -> Grad
vecMatchDiff xs ys matches = map (tripletMatchDiff xs ys) matches 
  
tripletMatchDiff :: [[Double]] -> [[Double]] -> (Int, Int) -> [Double]
tripletMatchDiff xs ys (x,y) = let
  x1 = xs !! x
  y1 = ys !! y
  in zipWith (-) y1 x1


----------------------
-- Work Calculation --
----------------------

calculateWork :: Input -> IO ()
calculateWork (prefix,atomN) = do
  let fileGrad     = gradName prefix 
      fileGeom     = coorName prefix
      fileOut      = workName prefix
  grad <- readGrad fileGrad atomN
  geom <- readGeom fileGeom atomN
  let allWorks  = calculateWorkForAllSteps grad geom
      allWorksS = unlines $ map show allWorks 
  putStrLn allWorksS
  writeFile fileOut allWorksS
  putStrLn "Coordinates in bohr"

calculateWorkForAllSteps :: [Grad] -> [Geom] -> [Work]
calculateWorkForAllSteps xs ys = let
  [rightTuplasGrad, rightTuplasGeom] = [doRight xs, doRight ys]
  in zipWith calculateWorkAtOneStep rightTuplasGrad rightTuplasGeom

calculateWorkAtOneStep :: (Grad,Grad) -> (Geom,Geom) -> Work
calculateWorkAtOneStep (grad1,grad2) (geom1,geom2) = let
  fAvg  = zipWithOverVec avgForce grad1 grad2
  disp  = zipWithOverVec (-) geom2 geom1 --       q1 - q0
  in (-1) * (sum $ zipWith (*) (concat disp) (concat fAvg))

avgForce :: Double -> Double -> Double
avgForce x y = (x + y) * 0.5

zipWithOverVec f a b = chunksOf 3 $ zipWith (f) (concat a) (concat b)

----------------------------
-- Readers and formatters --
----------------------------

readFour (prefixA,atomNA) (prefixB,atomNB) = do
  let [fGA,fGB] = map (gradName) [prefixA,prefixB]
      [fCA,fCB] = map (coorName) [prefixA,prefixB]
  g1 <- readGrad fGA atomNA
  g2 <- readGrad fGB atomNB
  c1 <- readGeom fCA atomNA
  c2 <- readGeom fCB atomNB
  print $ map length [g1,g2,c1,c2]
  return ((g1,g2),(c1,c2))

fromStringstoGeom st = map (map (map read2) . map tail . map words . lines) st
fromStringstoGeom2 st = map (map (map read2) . map tail . map words . tail . tail . lines) st

readGrad :: String -> Int -> IO ([Grad])
readGrad fn atomN = do
  grad <- readFile fn
  let gradM = mosaic grad
      gradN = map (map read2) gradM
      split = chunksOf atomN gradN
  return split

readGeom :: String -> Int -> IO([Geom])
readGeom fn atomN = do
  geoms <- readFile fn
  let geomsM = mosaic geoms
      geomsF = map tail geomsM
      geomsN = map (map read2) geomsF
      split  = chunksOf atomN geomsN
  return split

printTraj :: [Geom] -> IO()
printTraj xss = do
   let unmosaic = map (map (map show)) xss
       unlineZ  = map unlines $ map (map unwords) unmosaic
       addNumb  = zipWith (\x y -> "\nNumber: " ++ y ++ "\n" ++ x) unlineZ $ map show [0..]
   putStrLn $ concat addNumb    

formatGrad :: Grad -> Int -> Int -> String
formatGrad grad atomN label = let 
  stringZ = ("point    " ++ (show label)) : (show atomN) : map (unwords . map p) grad
  in unlines stringZ

readGeomStr :: Input -> IO [[String]]
readGeomStr  (prefix,atomN) = do
  geoms <- readFile (coorName prefix)
  let geomsM            = mosaic geoms
      atomTypeCorrected = map (\x -> [head (x!!0)] : tail x) geomsM
      string            = chunksOf atomN $ map unwords atomTypeCorrected
  return string

writeMoldenForceFile :: [String] -> [Grad] -> Int -> String -> IO()
writeMoldenForceFile geom grad atomN label = do 
  let first  = "[Molden Format]\n[GEOMETRIES] (XYZ)\n"
      second = concat geom 
      third  = "[FORCES] \n"
      fourth = zipWith (\x y -> formatGrad x atomN y) grad [1..]
      wholeString = first ++ second ++ third ++ concat fourth
  writeFile label wholeString

---------------------
-- Misc. Functions --
---------------------

mosaic :: String -> [[String]]
mosaic = map words . lines
    
read2 x = read x :: Double 

doRight [] = [] 
doRight (x:[]) = [] 
doRight (x:y:xs) = (x,y): doRight (y:xs)

p x = printf "%.6f" x :: String

getNames :: Input -> Input -> [String]
getNames a b = let
   [aa,bb] = map fst [a,b]
   name1   = "ForceDifference" ++ aa ++ "Minus" ++ bb ++ ".molden"
   name2   = "ForceDifference" ++ aa ++ "Minus" ++ bb ++ "Projection.molden"
   in [name1,name2]

