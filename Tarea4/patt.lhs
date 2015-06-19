\documentclass[11pt,fleqn]{article}

\usepackage{color}
\usepackage{tikz}
\usetikzlibrary{positioning,shapes}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}

\usepackage{amsmath}
\usepackage[spanish]{babel}
\usepackage{lmodern}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}

\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey},
   literate={á}{{\'a}}1
            {é}{{\'e}}1
            {í}{{\'i}}1
            {ó}{{\'o}}1
            {ú}{{\'u}}1
            {ñ}{{\~n}}1
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 4}

\author{Patrick Rengifo\\
09-10703\\
\href{mailto:patrick.rengifo@gmail.com}{<patrick.rengifo@gmail.com>}}

\date{Junio 15, 2015}

\maketitle

\pagebreak

\section*{Conjunto de Mandelbrot}

\noindent

\ignore{
\begin{lstlisting}

> module Main where
> 
> import qualified Control.Monad.Par as P
> import qualified Control.Parallel.Strategies as S
> import qualified Data.ByteString.Lazy as BS
> import qualified Data.Array.Repa as R
> import qualified Data.Vector.Unboxed as V
> import qualified Graphics.HGL as G
> import Control.DeepSeq
> import Control.Exception
> import Criterion.Main
> import Data.Array
> import Data.Complex
> import Data.Word

> deep :: NFData a => a -> a
> deep a = deepseq a a

\end{lstlisting}}

\noindent
La función \texttt{converge} determina si el punto en el plano $(x,y)$ pertenece
 al Conjunto de Mandelbrot.

\begin{lstlisting}

> converge :: (Double,Double) -> Word8
> converge (x,y) = mandelbrot x y 0 0 0
>   where mandelbrot x0 y0 x y iter
>             | iter > 255 = 255
>             | otherwise  = let x' = x*x - y*y + x0
>                                y' = 2*x*y + y0 
>                                in if x*x + y*y > 2*2
>                                   then iter
>                                   else mandelbrot x0 y0 x' y' (iter+1)

\end{lstlisting}

\noindent
\texttt{toColor} convierte el resultado de la función anterior en un 
\texttt{Color} para construir el dibujo final del Conjunto Mandelbrot.

\begin{lstlisting}

> toColor :: Word8 -> G.RGB
> toColor x = G.RGB (fromIntegral x) (fromIntegral x) (fromIntegral x)

\end{lstlisting}

\noindent
Para realizar la transformación necesaria de los puntos $(x,y)$ sobre el rango
[-2,2] sobre la parte interesante del Conjunto de Mandelbrot con buen detalle
se utilizan las dos siguientes funciones para hacer la correspondencia.

\begin{lstlisting}

> calculateStep :: Int -> Double
> calculateStep x = 4.0 / fromIntegral x
> 
> pixelCorrespondace :: Double -> Double -> (Int,Int) -> (Double,Double)
> pixelCorrespondace stepr stepi (x,y) = (-2 + stepr*(fromIntegral x), 
>     -2 + stepi*(fromIntegral y))

\end{lstlisting}

\noindent
La función \texttt{createMatrix} sirve para crear la matriz incial de puntos
$(x,y)$ con el tamaño de proporcionado por el usuario de resolución de imagen, 
luego aplicadno \texttt{pixelCorrespondance} para mantener el nivel de detalle 
del Conjunto interesante Mandelbrot en la resolución que se proporcione.

\begin{lstlisting}

> createMatrix :: Int -> Int -> [[(Double,Double)]]
> createMatrix x y = creator x y 0
>     where creator x y z
>             | x == z = []
>             | otherwise = map 
>                         (pixelCorrespondace (calculateStep x) (calculateStep y)) 
>                         (range ((z,0),(z,(y-1)))) : creator x y (z+1)

\end{lstlisting}


\noindent
Las tres implantaciones del cálculo de la parte interesante del
Conjunto de Mandelbrot sobre una ``ventana'' de visualización usando
\texttt{Strategies, Monad Par} y \texttt{REPA}.

\begin{lstlisting}

> mandelStrat :: Word32 -> Word32 -> [[Word8]]
> mandelStrat x y = let matrix = createMatrix (fromIntegral x) (fromIntegral y)
>                       in S.parMap S.rseq (S.parMap S.rpar converge) matrix
>
> mandelPar   :: Word32 -> Word32 -> [[Word8]]
> mandelPar x y = let matrix = createMatrix (fromIntegral x) (fromIntegral y)
>                     in map (P.runPar . P.parMap converge) matrix
>
> mandelREPA  :: Word32 -> Word32 -> [[Word8]]
> mandelREPA x y = let w = fromIntegral x
>                      h = fromIntegral y
>                      m = concat $ createMatrix w h
>                      matrix :: R.Array R.U R.DIM2 (Double,Double)
>                      matrix = R.fromListUnboxed (R.Z R.:. (w::Int) R.:. (h::Int)) m
>                      in transformar w h (V.toList 
>                                         (R.toUnboxed 
>                                             (R.computeS 
>                                                 ((R.map) converge matrix) 
>                                                 :: R.Array R.U R.DIM2 Word8)))

\end{lstlisting}

\noindent
\texttt{transformar} funciona para devolver el resultado de una dimensión de
los cálculos de la librería \texttt{REPA} a una matriz en forma de lista de listas.

\begin{lstlisting}

> transformar :: Int -> Int -> [Word8] -> [[Word8]]
> transformar x y list = creator x y 0 list
>     where creator x y z list
>             | x == z = []
>             | otherwise = take y list : creator x y (z+1) (drop y list)

\end{lstlisting}

\noindent
Las siguientes funciones se proveen para graficar el Conjunto Mandelbrot usando
cada una de las implementaciones realizadas. La función \texttt{pintar} se 
encarga de realizar el trabajo de transformar los resultados de \texttt{converge}
en gráficos para la librería \texttt{HGL}.

\begin{lstlisting}

> pintar :: Int -> Int -> G.Window -> [[Word8]] -> IO ()
> pintar w h window matrix = ready w h 0 0 matrix window
>     where 
>         ready _ _ _ _ [] _          = return ()
>         ready w h x y (m:ms) window = do 
>           G.drawInWindow window $ G.overGraphics $ go x h y m window
>           ready w h (x+1) y ms window
>         go _ _ _ [] _          = []
>         go x h y (m:ms) window = 
>           let point = G.line (x,y) (x+1, y+1)
>               in (G.withRGB (toColor (m)) $ point) : go x h (y+1) ms window
> 
> dibujarStrat :: Int -> Int -> IO ()
> dibujarStrat x y = G.runGraphics $ do
>     m <- evaluate $ deep $ mandelStrat (fromIntegral x) (fromIntegral y)
>     w <- G.openWindowEx "Conjunto Mandelbrot" Nothing (x,y) G.DoubleBuffered (Just 1)
>     G.clearWindow w
>     pintar x y w m
>     G.getKey w
>     G.closeWindow w
> 
> dibujarPar :: Int -> Int -> IO ()
> dibujarPar x y = G.runGraphics $ do
>     m <- evaluate $ deep $ mandelPar (fromIntegral x) (fromIntegral y)
>     w <- G.openWindowEx "Conjunto Mandelbrot" Nothing (x,y) G.DoubleBuffered (Just 1)
>     G.clearWindow w
>     pintar x y w m
>     G.getKey w
>     G.closeWindow w
> 
> dibujarREPA :: Int -> Int -> IO ()
> dibujarREPA x y = G.runGraphics $ do
>     m <- evaluate $ deep $ mandelREPA (fromIntegral x) (fromIntegral y)
>     w <- G.openWindowEx "Conjunto Mandelbrot" Nothing (x,y) G.DoubleBuffered (Just 1)
>     G.clearWindow w
>     pintar x y w m
>     G.getKey w
>     G.closeWindow w

\end{lstlisting}

\noindent
Finalmente, el programa principal de la implemetación, usando la librería
\texttt{Criterion} para realizar pruebas sobre las funciones de cálculo
del Conjunto Mandelbrot.

\begin{lstlisting}

> main = defaultMain [
>     bgroup "Mandelbrot" [ bench "Strategies" $ whnf mandelStrat 10
>                         , bench "Monar Par" $ whnf mandelPar 10
>                         , bench "REPA" $ whnf mandelREPA 10
>                         ]
>     ]

\end{lstlisting}

\end{document}
