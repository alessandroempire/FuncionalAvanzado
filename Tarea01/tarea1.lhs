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

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 1}

\author{Ernesto Hernández-Novich\\
86-17791\\
\href{mailto:emhn@usb.ve}{<emhn@usb.ve>}}

\date{Mayo 2, 2013}

\maketitle

\pagebreak

\section{Fold abstracto}

\begin{itemize}
\item
  \textbf{(1 punto)} -- considere la función \texttt{dropWhile}
  provista en el Preludio y en \texttt{Data.List}. Provea una
  implantación de \texttt{dropWhile} empleando el \texttt{fold}
  más apropiado según el caso.

\item
  \textbf{(2 puntos)} -- Provea una implantación de \texttt{foldl}
  usando \texttt{foldr}.

\begin{lstlisting}
> foldl :: (a -> b -> a) -> a -> [b] -> a
> foldl = {- Algo con foldr -}
\end{lstlisting}

  Incluya un diagrama que ilustre el funcionamiento de su implantación.

\end{itemize}

\section{Foldable y Functor}

Considere el tipo de datos

\begin{lstlisting}
> data Dream b a = Dream a
>                | Limbo (b,a) 
>                | Within a (Seq (Dream b a))
>                | Nightmare b
>                deriving (Show)
\end{lstlisting}

\begin{itemize}
\item
  \textbf{(1 puntos)} -- Construya la instancia \texttt{Functor}
  para el tipo \texttt{Dream b}.

\item
  \textbf{(2 puntos)} -- Construya la instancia \texttt{Foldable}
  para el tipo \texttt{Dream b}.

\end{itemize}

\section{Monoid}

Considere el tipo de datos \texttt{(Data.Map k v)} comentado en clase,
que tiene algún tipo de datos \texttt{k} como clave y sobre el cual
queremos permitir \emph{múltiples valores} asociados a una clave.

Proponga un tipo de datos concreto apoyado en \texttt{Data.Map}
que permita esa funcionalidad, y entonces:

\begin{itemize}
\item
  \textbf{(2 puntos)} -- Construya la instancia \texttt{Monoid}
  para este tipo de datos. En la instancia queremos que al
  combinar dos \texttt{Map}, si hay claves repetidas, se
  \emph{unan} los valores asociados.
  
\item
  \textbf{(1 punto)} -- Escriba un ejemplo de uso con al menos
  \emph{tres} tablas involucradas, que contengan claves \emph{repetidas}
  cuyos valores deban combinarse para ejercitar el \texttt{Monoid}
  a la medida.

\end{itemize}

\section{Zippers}

Considere el tipo de datos

\begin{lstlisting}
> data Tree a = Leaf a | Node a (Tree a) (Tree a) (Tree a)
\end{lstlisting}

\textbf{(3 puntos)} -- diseñe un zipper seguro para el tipo \texttt{Tree}
proveyendo todas las funciones de soporte que permitan trasladar el foco
dentro de la estructura de datos, así como la modificación de cualquier
posición dentro de la estructura.

\begin{lstlisting}
> data Breadcrumbs a = undefined
>
> type Zipper a = (Tree a, Breadcrumbs a)
>
> goLeft   ::
> goRight  ::
> goCenter ::
> goBack   ::
> tothetop :: 
> modify   ::
> focus    ::
> defocus  ::
\end{lstlisting}

\end{document}
