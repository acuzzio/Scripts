import Text.Printf

x  = (-7.0)
x2 =   8.0
dimension = 12
charge = 1.0

piastrami :: Double -> Double -> Integer -> Double -> IO()
piastrami x x2 dim totalCharge = do
  let singleCharge = totalCharge / (fromIntegral pointsN :: Double)
      points       = [[x, y, z, singleCharge, 0.0, 0.0, 0.0] | y <- map (\x -> fromInteger x :: Double) [(-dim)..dim], z <- map (\x -> fromInteger x :: Double) [(-dim)..dim], lengthInferior y z (fromInteger dim :: Double)]
      points2      = [[x2, y, z, (-singleCharge), 0.0, 0.0, 0.0] | y <- map (\x -> fromInteger x :: Double) [(-dim)..dim], z <- map (\x -> fromInteger x :: Double) [(-dim)..dim], lengthInferior y z (fromInteger dim :: Double)]
      charges      = points ++ points2
      strings      = map (map show) charges
      reformatZ    = map (replace 3) strings 
      toFile       = unlines $ map unwords $ reformatZ
      pointsN      = length points
      howMany      = show $ length charges
      toSee        = unlines $ map unwords $ map (\x -> ["H",x!!0,x!!1,x!!2]) strings
  putStrLn howMany
  putStrLn toFile
  writeFile "piastra" howMany
  appendFile "piastra" $ toFile
  writeFile "toSee.xyz" $ howMany ++ "\n\n"
  appendFile "toSee.xyz" $ toSee


reformat :: String -> String
reformat x = let float = read x :: Double
             in printf  "%3.4f" float :: String

replace :: Int -> [String] -> [String]
replace position list = let 
  first   = take (position) list
  second  = drop (position+1) list
  element = reformat (list !! position)
  in first ++ [element] ++ second

lengthInferior :: Double -> Double -> Double -> Bool
lengthInferior y z ray = let length = sqrt (y**2 + z**2)
                         in if length > ray then False else True