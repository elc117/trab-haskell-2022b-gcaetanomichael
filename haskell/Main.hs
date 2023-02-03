import Data.List
import System.Random
import Text.Printf

svgPoly :: String -> String -> String
svgPoly x style = printf "<polygon points='%s' %s />" x style

svgBegin :: Int -> Int -> String
svgBegin w h = printf "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'> \n" w h

svgEnd :: String
svgEnd = "</svg>"

randomBetween :: Int -> Int -> IO Int
randomBetween low high = getStdRandom (randomR (low, high))

randomColor :: IO String
randomColor = do
  r <- randomBetween 0 255
  g <- randomBetween 0 255
  b <- randomBetween 0 255
  return $ "fill='rgb(" ++ (show r) ++ ", " ++ (show g) ++ ", " ++ (show b) ++ ")' stroke='black' stroke-width='1' opacity='0.1'"

getRandomCoods :: Int -> Int -> IO String
getRandomCoods w h = do
  x1 <- randomBetween 0 w
  y1 <- randomBetween 0 h
  x2 <- randomBetween 0 w
  y2 <- randomBetween 0 h
  x3 <- randomBetween 0 w
  y3 <- randomBetween 0 h
  return $ (show x1) ++ ", " ++ (show y1) ++ " " ++ (show x2) ++ ", " ++ (show y2) ++ " " ++ (show x3) ++ ", " ++ (show y3)

getRandomSvgPoly :: Int -> String -> String -> String
getRandomSvgPoly n coord color = take n (svgPoly coord color)

main :: IO ()
main = do
  color <- randomColor
  pos_randgen <- newStdGen
  let width = 2316
      height = 1080
      qnt_polygon = 150
  randomCoord <- getRandomCoods width height
  let svgFig = getRandomSvgPoly qnt_polygon randomCoord color
  let polygon = svgBegin width height ++ svgFig ++ svgEnd
  writeFile "polygon.svg" polygon