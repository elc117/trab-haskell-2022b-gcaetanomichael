import Text.Printf
import System.Random

type Rect = (Int, Int, Int, Int) -- x, y, width, height

maxRGB :: Int
maxRGB = 255


--------------------------------------------------------------------------
-- Strings SVG
--------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
-- Lembrando: Rect = (Float,Float,Float,Float)
svgRect :: Rect -> String -> String 
svgRect (x,y,w,h) style = 
  printf "<rect x='%d' y='%d' width='%d' height='%d' style='%s' />\n" x y w h style

-- String inicial do SVG
-- svgBegin :: Float -> Float -> String
svgBegin :: Int -> Int -> String
svgBegin w h = 
  printf "<svg width='%d' height='%d' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b


--------------------------------------------------------------------------
-- Geração de paletas, listas de cores (R, G, B)
--------------------------------------------------------------------------

-- Paleta (R, G, B) formada por tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- Paleta formada por apenas 3 cores (Red, Green, Blue) que se repetem ciclicamente
-- O símbolo '$' é uma facilidade sintática que substitui parênteses
-- A função cycle gera uma lista infinita -- procure saber mais sobre ela :-)
rgbOnlyPalette :: Int -> [(Int,Int,Int)]
rgbOnlyPalette n = take n $ cycle [(maxRGB,0,0), (0,maxRGB,0), (0,0,maxRGB)]

-- Paleta com n cores geradas (pseudo-)aleatoriamente
-- Esta função recebe um gerador e começa produzindo uma lista de 3*n valores
-- Depois a lista é "fatiada" para gerar valores para R, G, B
-- No final os valores são combinados em uma lista de tuplas
randomPalette :: StdGen -> Int -> [(Int,Int,Int)]
randomPalette gen n = 
  let allrandoms = take (3*n) (randomRs (0, maxRGB) gen::[Int])
      rs = take n allrandoms
      gs = slice n (2*n-1) allrandoms
      bs = slice (2*n) (3*n) allrandoms
   in zipWith3 (\r g b -> (r,g,b)) rs gs bs

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

   
--------------------------------------------------------------------------
-- Geração de retângulos em suas posições
--------------------------------------------------------------------------

genRectsInLine :: Int -> Int -> Int -> [Rect]
genRectsInLine n size gap = 
  [(m*(size+gap), 0, size, size) | m <- [0..(n-1)]]

genRectGrid :: Int -> Int -> Int -> [Rect]
genRectGrid n size gap = 
  [(i*(size+gap), j*(size+gap), size, size) | i <- [0..(n-1)], j <- [0..(n-1)]]

genRandomBools :: StdGen -> Int -> [Bool]
genRandomBools gen n = take n (randomRs (True,False) gen :: [Bool])

-- Gera uma lista de retângulos aleatoriamente selecionados num grid nxn
-- Usa funções de alta ordem e lambdas 
-- Primeiro gera o grid completo com nxn retângulos
-- Depois gera uma lista aleatória com nxn Bools 
-- Termina filtrando a lista: só restam retângulos que coincidem com True
-- Implementação é pouco eficiente porque gera dados desnecessários, 
-- mas ilustra a aplicação encadeada de várias funções simples
-- Caso tenha dificuldade de entender esta função, use o GHCi para
-- testar cada parte separadamente, usando listas constantes
genRandomRects :: StdGen -> Int -> Int -> Int -> [Rect]
genRandomRects gen n size gap =
  let grid = genRectGrid n size gap
      randombools = genRandomBools gen (n*n)
   in map (\(b,r) -> r) $ filter (\(b,r) -> b) $ zip randombools grid

--------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
--------------------------------------------------------------------------

main :: IO ()
main = do
  color_randgen <- newStdGen
  pos_randgen <- newStdGen
  putStrLn "Check output in pattern.svg"
  let nrects = 10
      rsize = 50
      gap = 10
      palette = randomPalette color_randgen (nrects*nrects)
      rects = genRandomRects pos_randgen nrects rsize gap
      svgfigs = concat $ zipWith svgRect rects (map svgStyle palette)
      imagew = nrects*(rsize+gap)
      (w,h) = (imagew, imagew) -- width,height da imagem SVG
      svgstrings = svgBegin w h ++ svgfigs ++ svgEnd
   in writeFile "pattern.svg" svgstrings
