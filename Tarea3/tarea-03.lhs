\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 3}

\author{Alessandro La Corte\\
09-10430\\
\href{mailto:alessandroempire@gmail.com}{<alessandroempire@gmail.com>}}

\date{Mayo 26, 2015}

\maketitle

\pagebreak


\section*{Eat all the empanadas!}

\noindent
Rafita prepara empanadas en una recóndita playa del oriente del
país. La calidad de sus empanadas de cazón es legendaria, tanto
que la contratan para servir los pasapalos de las fiestas playeras
organizadas por la banda usual de desadaptados que allí pulula en
cualquier puente o feriado largo.
\\

\noindent
Para esos eventos, Rafita se presenta con su gran paila,
que tiene capacidad para freír $m$ empanadas. Una vez fritas,
echa \emph{todas} las empanadas, simultáneamente, en un gran colador
plástico de color indeterminado, raído por el tiempo, los elementos
y el manoseo. Hecho esto, y consecuencia del calor por la paila y el
inclemente sol, Rafita se sienta a tomar cerveza.
\\

\noindent
Cuando un parroquiano está hambriento, se acerca a buscar empanadas.
Los parroquianos de la fiesta no hacen cola, sino que meten mano
y se sirven del colador, tomando y comiendo \textbf{una} empanada
a la vez, siempre y cuando haya empanadas disponibles. Cuando el
parroquiano come, vuelve a tomar cerveza por un rato, hasta que
tenga hambre nuevamente.
\\

\noindent
Si un parroquiano tiene hambre, pero no hay empanadas, le avisa a
Rafita para que prepare más.
\\

\noindent
Se desea que Ud. construya una simulación de este comportamiento
usando Haskell. En este sentido, tome en cuenta lo siguiente:

\begin{itemize}
\item
  Rafita es la única que prepara empanadas y siempre prepara
  \emph{exactamente} $m$ empanadas en lote. Naturalmente,
  Rafita será modelada con un hilo de ejecución.
\item 
  Rafita tarda un tiempo al azar en preparar las empanadas.
  A efectos de esta simulación, considere un tiempo al azar
  entre 3 y 5 segundos. La simulación debe indicar claramente
  \texttt{Rafita está cocinando} y pasado el intervalo
  \texttt{Rafita sirvió las empanadas}.
\item
  En la fiesta puede haber un número arbitrario de parroquianos.
  Su simulación debe estar parametrizada para $n > 0$ parroquianos,
  y cada parroquiano debe estar representado por un hilo
  independiente.
\item
  Los hábitos de bebida y comida de los parroquianos son muy
  variables, así que debe considerar que transcurre un tiempo
  al azar entre 1 y 7 segundos desde el momento en que empuña
  su empanada, la consume, vuelve a su bebida, y tiene hambre
  de nuevo. La simulación debe indicar claramente
  \texttt{Parroquiano N come empanada} cuando comienza a comer
  y \texttt{Parroquiano N tiene hambre} cuando vuelve a buscar
  una empanada.
\item
  Rafita tiene ingredientes infinitos para preparar las empanadas,
  y los parroquianos no tienen nada más productivo que hacer,
  de manera que una vez que comienza la fiesta, sólo termina
  cuando se interrumpe la simulación con \texttt{Ctrl-C}.
\end{itemize}


\noindent
Presente \textbf{dos} soluciones para este problema: una empleando
técnicas clásicas de concurrencia (sincronización con \texttt{MVar})
y otra empleando Memoria Transaccional (\texttt{STM}).

\begin{lstlisting}

> import System.Random
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import Data.Sequence as DS hiding (replicate, replicateM)
>
> randomSeed :: Int
> randomSeed = 42
>
> classic :: Int -> Int -> IO ()
> classic m n = undefined
> 
> transactional :: Int -> Int -> IO ()
> transactional m n = undefined

\end{lstlisting}

\noindent
En ambos casos, cuando se interrumpa la simulación, presente
un resultado sumario indicando:

\begin{verbatim}
Rafita preparó R empanadas.

Parroquiano 1:    P1
Parroquiano 2:    P2
...
Parroquiano N:    PN
Total:            T
\end{verbatim}

\noindent
Donde \texttt{R} es el total de empanadas, necesariamente
múltiplo de $m$; \texttt{P1} hasta \texttt{PN} corresponden
a la cantidad de empanadas que comió cada parroquiano,
respectivamente, y \texttt{T} resulta de la suma de esos
consumos.
\\

\noindent
Finalmente, para poder comprobar la fidelidad de su simulación
es necesario que use números pseudo-aleatorios, como los que
se proveen en \texttt{System.Random} \emph{fuera} del
monad \texttt{IO}, usando \texttt{randomSeed} como semilla.
\\

\noindent
Solucion con Memoria Transaccional

Se tiene una tupla en donde el primer elemento es:
Cantidad de empanadas disponibles para comer. El 
segundo elemento es la cantidad total de empanadas 
preparadas por Rafita
\begin{lstlisting}

> type Empanadas = TVar (Int, Int)
>
> newRafita :: IO (Empanadas)
> newRafita = do v <- newTVarIO (0,0)
>                return v
>
> cook :: Empanadas -> Int -> STM ()
> cook e n = do s <- readTVar e
>               if (fst s == 0)
>               then writeTVar e (n, (snd s) + n)
>               else retry

\end{lstlisting}

Cantidad de empanadas que comio el N-esimo parroquiano
\begin{lstlisting}
 
> type Parroquiano = TVar Int
>
> newParroquiano :: IO (Parroquiano)
> newParroquiano = do v <- newTVarIO 0
>                     return v
>
> eat :: Parroquiano -> Empanadas -> STM ()
> eat p e = do s <- readTVar p
>              t <- readTVar e
>              if (fst t == 0)
>              then retry
>              else writeTVar p (s+1)

\end{lstlisting}

\begin{lstlisting}

> type Buffer a = TVar (DS.Seq a)
>
> newBuffer :: IO (Buffer a)
> newBuffer = newTVarIO DS.empty
> 
> put :: Buffer a -> a -> STM ()
> put buffer item = do ls <- readTVar buffer
>                      writeTVar buffer (ls |> item)
> 
> get :: Buffer a -> STM a
> get buffer = do ls <- readTVar buffer
>                 case viewl ls of
>                   EmptyL       -> retry
>                   item :< rest -> do writeTVar buffer rest
>                                      return item

\end{lstlisting}

\begin{lstlisting}

> simulation n m = do parroquianos <- replicateM m newParroquiano
>                     empanadas <- newRafita
>                     outputBuffer <- newBuffer
>                     forkIO $ rafitaSim n empanadas outputBuffer
>                     forM_ [0..m-1] $ \i ->
>                       forkIO (parroquianoSim i (parroquianos!!i)
>                                 empanadas outputBuffer)
>                     output outputBuffer
> 
> rafitaSim n empanadas out = 
>   do atomically $ cook empanadas n 
>      atomically $ put out ("Rafita esta cocinando.")
>      --rafitaDelay
>      randomDelay
>      atomically $ put out ("Rafita sirvio las empanadas.")
>      rafitaSim n empanadas out
>
> parroquianoSim n parroquiano empanada out =
>   do atomically $ eat parroquiano empanada
>      atomically $ put out ("Parroquiano " ++ show n ++ "come empanada.")
>      -- parroquianoDelay
>      randomDelay
>      atomically $ put out ("Parroquiano " ++ show n ++ "tiene hambre.")
>      parroquianoSim n parroquiano empanada out
>
> output buffer = 
>     do str <- atomically $ get buffer
>        putStrLn str
>        output buffer

\end{lstlisting}

\begin{lstlisting}

> rafitaDelay = undefined
>
> parroquianosDelay = undefined
>
> randomDelay = do r <- randomRIO (100000,500000)
>                  threadDelay r

\end{lstlisting}


\end{document}
