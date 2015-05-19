
Mostraremos los pasos para ir combinando correctamente los monads
\texttt{Reader}, \texttt{Writer}, \texttt{State} y \texttt{Error}.

\begin{itemize}    
\item 
  1) Colocamos el Monad IO ()
  \begin{lstlisting}

> type Eval1 a = IO a
>
> eval1 :: NFA -> [Char] -> Seq.Seq (DS.Set NFANode )
>              -> DS.Set NFANode -> Eval1 (Seq.Seq (DS.Set NFANode))
> eval1 nfa []     set act = return set
> eval1 nfa (x:xs) set act = eval1 nfa xs ((Seq.|>) set d) d
>   where d = getDest nfa x act
> 
> getDest :: NFA -> Char -> DS.Set NFANode -> DS.Set NFANode
> getDest nfa char set = DS.unions . map (destinations nfa char) $ DS.toList set
>
> evalInicial1 :: NFA -> [Char] -> Eval1 (Seq.Seq (DS.Set NFANode)) 
> evalInicial1 nfa word = eval1 nfa word (Seq.singleton node0) node0
>     where node0 = DS.singleton (initial nfa) 
>
> evalM1 :: Eval1 a -> IO (a)
> evalM1 e = e 

  \end{lstlisting}
\item
  2) Colocamos el transformador de error sobre el monad IO 
  \begin{lstlisting}

> type Eval2 a =  ErrorT NFAReject IO a
> 
> eval2 :: NFA -> [Char] -> Seq.Seq (DS.Set NFANode)
>              -> DS.Set NFANode -> Eval2 (Seq.Seq (DS.Set NFANode))
> eval2 nfa []     set act = checkReject nfa set act
> eval2 nfa w@(x:xs) set act = if (DS.null d)
>                              then throwError $ Stuck act w
>                              else eval2 nfa xs ((Seq.|>) set d) d  
>   where d = getDest nfa x act
>
> checkReject nfa set act = if (accepting nfa act)
>                           then return set
>                           else throwError $ Reject act
>
> evalInitial2 :: NFA -> [Char] -> Eval2 (Seq.Seq (DS.Set NFANode))
> evalInitial2 nfa word = eval2 nfa word (Seq.singleton node0) node0
>     where node0 = DS.singleton (initial nfa) 
>
> evalM2 :: Eval2 (Seq.Seq (DS.Set NFANode)) 
>           -> IO (Either NFAReject (Seq.Seq (DS.Set NFANode)))
> evalM2 = runErrorT

  \end{lstlisting}
\item
  3) Colocamos el Monad Reader sobre el monad construido en el paso 2. 
  \begin{lstlisting}

> type Eval3 a = ReaderT NFA (ErrorT NFAReject IO) a
>
> eval3 :: [Char] -> Seq.Seq (DS.Set NFANode)
>                 -> DS.Set NFANode -> Eval3 (Seq.Seq (DS.Set NFANode))
> eval3 []       set act = do nfa <- ask
>                             checkReject nfa set act
> eval3 w@(x:xs) set act = do nfa <- ask
>                             when (DS.null (d nfa)) $ throwError $ Stuck act w
>                             eval3 xs ((Seq.|>) set (d nfa)) (d nfa)
>   where d nfa = getDest nfa x act
>
> evalInitial3 :: [Char] -> Eval3 (Seq.Seq (DS.Set NFANode))
> evalInitial3 word = do nfa <- ask 
>                        eval3 word (Seq.singleton (node0 nfa)) (node0 nfa)
>     where node0 nfa = DS.singleton (initial nfa) 
>
> evalM3 :: NFA -> Eval3 (Seq.Seq (DS.Set NFANode)) 
>               -> IO (Either NFAReject (Seq.Seq (DS.Set NFANode)))
> evalM3 nfa = runErrorT . (flip runReaderT) nfa

  \end{lstlisting}
\item
  4) Agregamos el monad State a nuestra construccion. Tener encuenta que
     colocamos el StateT por debajo del monad ErrorT para poder llevar el
     estado.
  \begin{lstlisting}

> type Eval4 a = ReaderT NFA 
>                   (ErrorT NFAReject 
>                       (StateT NFARun IO)) a
>
> eval4 :: Seq.Seq (DS.Set NFANode) -> Eval4 (Seq.Seq (DS.Set NFANode))
> eval4 set = do 
>    nfa <- ask
>    s <- get
>    let word = w s
>        act  = qs s
>    if (null word ) 
>    then checkReject nfa set act
>    else do let c = head word
>            when (DS.null (dst nfa c act)) $ 
>                 throwError $ Stuck act word
>            put $ s {w = tail word, qs = dst nfa c act}
>            eval4 ((Seq.|>) set (dst nfa c act))
>   where dst nfa c act = getDest nfa c act
>
> evalInitial4 :: Eval4 (Seq.Seq (DS.Set NFANode ))
> evalInitial4 = do nfa <- ask 
>                   eval4 (Seq.singleton (node0 nfa))
>     where node0 nfa = DS.singleton (initial nfa) 
>
> evalM4 :: NFA -> NFARun 
>               -> Eval4 (Seq.Seq (DS.Set NFANode)) 
>               -> IO (Either NFAReject (Seq.Seq (DS.Set NFANode )), NFARun)
> evalM4 nfa init = (flip runStateT init) . runErrorT . 
>                   (flip runReaderT) nfa

  \end{lstlisting}
\item
  5) Agregamos el monad Transformer WriterT para llevar una bitacora.
     Lo agregamos por debajo del nivel del ErrorT para que no se pierda
     la botacora, y por encima del transformador StateT
  \begin{lstlisting}

> type NFALog = Seq.Seq (DS.Set NFANode) 
>
> type Eval5 a = ReaderT NFA 
>                   (ErrorT NFAReject 
>                       (WriterT NFALog
>                           (StateT NFARun IO))) a
>
> eval5' :: Eval5 ()
> eval5' = do tell $ Seq.singleton (DS.singleton (Node 0)) 
>             eval5
>
> eval5 :: Eval5 ()
> eval5 = do 
>    nfa <- ask
>    s <- get
>    let word = w s
>        act  = qs s
>    if (null word ) 
>    then do unless (accepting nfa act) $ throwError $ Reject act
>    else do let c = head word
>            when (DS.null (dst nfa c act)) $ 
>               throwError $ Stuck act word
>            put $ s {w = tail word, qs = dst nfa c act}
>            tell $ (Seq.singleton (dst nfa c act))
>            eval5
>   where dst nfa c act = getDest nfa c act
>
> evalM5 :: NFA -> NFARun 
>               -> Eval5 ()
>               -> IO ((Either NFAReject (), NFALog), NFARun)
> evalM5 nfa init = (flip runStateT init) . runWriterT . 
>                    runErrorT . (flip runReaderT) nfa

  \end{lstlisting}
\end{itemize}

