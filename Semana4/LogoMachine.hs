{-|
  /Máquina Virtual Logo/

  Programación Funcional Avanzada (CI4251)

  0.42 2012-05-14 
  0.41 2010-05-09


  Ernesto Hernández-Novich <emhn@usb.ve>


  Este módulo implanta una máquina virtual interpretadora de instrucciones
  básicas Logo, para demostrar la manera de aprovechar la infraestructura
  de Monad State provista por @Control.Monad.State@ y combinarlo con la
  librería gráfica @Graphics.HGL@.

  Nótese que el Monad State /no/ es construido manualmente, sino que se
  instancia gracias al sistema de tipos simplemente indicando las firmas
  adecuadas en las acciones monádicas.

  Para aprovechar este módulo en Debian GNU/Linux Squeeze
  
    @aptitude install libghc6-hgl-dev libghc6-mtl-dev@ 
-}
  
module LogoMachine (
  -- * Tipos exportados.
	-- ** Instrucciones de bajo nivel de la Máquina Logo.
  LogoProgram (..),
	-- * Funciones exportadas.
	-- ** Ejecutar instrucciones de la Máquina Logo con salida gráfica.
  runLogoProgram
) 
where

import Control.Monad.State
import Data.Char
import Data.Sequence as DS
import Data.Foldable as DF
import qualified Data.Map     as DM
import qualified Graphics.HGL as G

{-
  El tipo de datos @LogoProgram@ representa las instrucciones
  "de máquina" que es capaz de interpretar la Máquina Virtual Logo. La
  Máquina Logo dispone de una tortuga que puede avanzar o retroceder
  un número entero de "pasos" sobre un plano cartesiano, girando
  a la derecha o izquierda un número entero de grados. La tortuga
  dispone de lápices de colores los cuales pueden bajarse o subirse
  para dibujar o no a medida que se avanza, cambiando el color de los
  lápices si se desea.  La tortuga puede emitir un mensaje de texto
  arbitrario usando el color del lápiz actual, el cual aparecerá
  horizontalmente a la izquierda de la posición actual,
  independientemente de la orientación.
		
  Nótese que además de las instrucciones atómicas fundamentales,
  también se dispone de la instrucción de Secuenciación y la
  instrucción de Iteración Determinada.

  Se declara derivando de @Show@ para facilitar la depuración
  y la creación de fragmentos de código "manualmente".

  Se declara derivando de @Eq@ pues en la implantación interna
  de la máquina es necesario comparar instrucciones.
-}

data LogoProgram = Fd Int                       -- ^ Avanzar N pasos.
                 | Bk Int                       -- ^ Retroceder N pasos.
                 | Rt Int                       -- ^ Girar N grados a derecha.
                 | Lt Int                       -- ^ Girar N grados a izquierda.
                 | Pu                           -- ^ Subir el lápiz.
                 | Pd                           -- ^ Bajar el lápiz.
                 | Pc String                    -- ^ Cambiar el color del lápiz.
                 | Say String                   -- ^ Emitir un mensaje de texto.
                 | Home                         -- ^ Regresar al origen.
                 | Seq (DS.Seq LogoProgram)     -- ^ Secuencia de instrucciones.
                 | Rep Int (DS.Seq LogoProgram) -- ^ Repetir N veces.
                 deriving (Show, Eq)

{-
   Catamorfismo (fold) sobre el tipo de datos de las instrucciones
   de la Máquina Virtual Logo, a ser aprovechado en la fase de
   conversión de instrucción hacia acciones monádicas.
 -}
foldLP a b c d e f g h i j k inst =
  case inst of 
    (Fd n)    -> a n
    (Bk n)    -> b n
    (Rt n)    -> c n
    (Lt n)    -> d n
    Pu        -> e
    Pd        -> f
    (Pc s)    -> g s
    (Say s)   -> h s
    Home      -> i
    (Seq l)   -> j (fmap (foldLP a b c d e f g h i j k) l)
    (Rep n l) -> k n (fmap (foldLP a b c d e f g h i j k) l)

{-
  @validColors@ es un valor constante que produce una tabla con los
  colores válidos de la Máquina Virtual Logo, utilizando cadenas
  alfanuméricas como clave de búsqueda.
-}

validColors :: DM.Map String G.Color
validColors = DM.fromList [
                ( "black",   G.Black   ),
                ( "red",     G.Red     ),
                ( "green",   G.Green   ),
                ( "blue",    G.Blue    ),
                ( "cyan",    G.Cyan    ),
                ( "magenta", G.Magenta ),
                ( "yellow",  G.Yellow  ),
                ( "white",   G.White   )
              ]

{-
   @toColor@ es utilizada para convertir una cadena que especifica un
   color en el tipo de datos Color necesario para dibujar. En caso
   que la cadena a buscar no corresponda a un color definido por la
   Máquina Virtual Logo, la ejecución aborta con un error.
 -} 
toColor :: String -> G.Color
toColor s =
  case f of
    Just c  -> c
    Nothing -> error $ "'" ++ s ++ "' es un color invalido"
  where f = DM.lookup (map toLower s) validColors

{-
   @Figure@ persigue modelar la geometría generada por la
   interpretación apoyándose solamente en los polígonos y texto.
   El constructor @Empty@ es utilizado como centinela para detectar
   cuando debe comenzar y cuando termina un nuevo polígono o
   primitiva de texto.
 -}
data Figure = Poly G.Color [G.Point] 
			| Text G.Color G.Point String
            | Empty
            deriving (Show,Eq)

{-
   Modelo de Estado para la Máquina Virtual Logo, aprovechado
   en la instanaciación automática del Monad State
-}
type Direction   = Int
data PenStatus   = Up | Down
                 deriving (Show,Eq)

data LogoState = LogoState {
   pos :: G.Point,            -- (x,y)
   dir :: Direction,          -- En grados
   pns :: PenStatus,
   pnc :: G.Color,
   drw :: DS.Seq Figure
} deriving (Show)

{- @noop@ -- Transformación de estado que no hace nada -}
noop :: State LogoState ()
noop = return ()

{- @pu@ -- Transformación de estado para subir el lápiz -}
pu :: State LogoState ()
pu = do
  s <- get
  case pns s of
    Down -> put $ s { pns = Up, drw = drw' }
            where drw' = case d of
                           (ds :> Empty) -> ds
                           _             -> drw s
                         where d = DS.viewr $ drw s
  
    Up   -> put $ s

{- @pd@ -- Transformación de estado para bajar el lápiz -}
pd :: State LogoState ()
pd = do
  s <- get
  case pns s of
    Down -> put $ s
    Up   -> put $ s { pns = Down, drw = (drw s) |> Empty }

{- @pd@ -- Transformación de estado para cambiar el color del lápiz -}
pc :: String -> State LogoState ()
pc c = do
  s <- get
  put $ s { pnc = toColor c }

{- @say@ -- Transformación de estado para emitir mensaje de texto -}
say :: String -> State LogoState ()
say m = do
  s <- get
  case pns s of
    Down -> case d of 
              (ds :> Empty) -> put $ s { drw = ds      |> t }
              _             -> put $ s { drw = (drw s) |> t }
            where d = DS.viewr $ drw s
                  t = Text (pnc s) (pos s) m
    Up   -> put $ s

{- @fd@ -- Transformación de estado para avanzar @n@ pasos -}
fd :: Int -> State LogoState ()
fd n = get >>= put . moveForward n

{- @bk@ -- Transformación de estado para retroceder @n@ pasos -}
bk :: Int -> State LogoState ()
bk n = get >>= put . moveForward (negate n)

{- @moveForward@ -- Función auxiliar para @fd@ y @bk@ encargada
   de calcular el desplazamiento en la dirección actual, posiblemente
   generando la geometría asociada. -}
moveForward :: Int -> LogoState -> LogoState
moveForward n s | pns s == Up = s { pos = move (pos s) n (dir s) }
moveForward n s =
  case d of 
    (ds :> Empty)     -> s { pos = np, drw = ds |> t}
    (ds :> Poly pc l) -> if (pc == cc) 
                         then s { pos = np, drw = ds |> Poly cc (np:l)  }
                         else s { pos = np, drw = drw s |> t  }
    _                 -> s { pos = np, drw = drw s |> t  }
    where cc = pnc s
          cp = pos s
          d  = DS.viewr $ drw s
          np = move cp n (dir s)
          t  = Poly cc [ np, cp ]
    
move :: G.Point -> Int -> Direction -> G.Point
move (x,y) n d =
  let direc  = (pi * (fromIntegral d)) / 180
      nn     = fromIntegral n
      nx     = x + (round (nn * (cos direc)))
      ny     = y + (round (nn * (sin direc)))
  in (nx,ny)

{- @lt@ -- Transformación de estado para girar @n@ grados a la izquierda -}
lt :: Int -> State LogoState ()
lt n = get >>= put . turnLeft n

{- @rt@ -- Transformación de estado para girar @n@ grados a la derecha -}
rt :: Int -> State LogoState ()
rt n = get >>= put . turnLeft (negate n)

{- @turnLeft@ -- Función auxiliar para @lt@ y @rt@ encargada de calcular
   la rotación en grados, manteniendo los valores entre 0 y 359. -}
turnLeft :: Int -> LogoState -> LogoState
turnLeft n s = s { dir = (dir s + n) `mod` 360 }

{- @home@ -- Transformación de estado para regresar al estado inicial -}
home :: State LogoState ()
home = put $ initial

{- @goHome@ -- Transformación de estado para regrear al origen -}
goHome :: State LogoState ()
goHome = do
  s <- get
  put $ s { pos = (0,0), dir = 90 }

{- @initial@ -- Estado inicial de la Máquina Virtual Logo -}
initial :: LogoState
initial = LogoState { pos = (0,0),
                      dir = 90,
                      pns = Up,
                      pnc = G.White,
                      drw = DS.empty }

{- @repN@ -- Transformación de estado para repetir @n@ veces
   una transformación de estado particular. -}
repN :: Int -> State LogoState () -> State LogoState ()
repN 0 p = noop
repN n p = p >> (repN (pred n) p)

{- @monadicPlot@ -- Aplica un catamorfismo (fold) sobre la estructura
   de datos que representa un programa para la Máquina Virtual Logo,
   de manera que lo transforma en la secuencia de transformaciones de
   estado correspondientes a su interpretación. -}
monadicPlot = foldLP fd bk rt lt pu pd pc say home seq rep
  where seq s   = if DS.null s then noop else DF.sequence_ s
        rep n s = repN n (seq s)

{-|
  La función @runLogoProgram@ interpreta un programa escrito con
  las instrucciones de la Máquina Logo, produciendo la salida
  gráfica asociada.

  El programa abrirá una ventana con las dimensiones y el título
  suministrados como argumentos, se posicionará la tortuga
  en el centro de la ventana correspondiente a la coordenada (0,0),
  apuntando hacia el tope de la pantalla (90 grados), con el lápiz
  blanco levantado.

  Se convertirá el programa en la secuencia de transformación de
  estado correspondiente, siendo interpretado hasta obtener el
  estado final. La geometría calculada como parte de esta transformación
  será extraída, convertida a las primitivas gráficas correspondientes
  en HGL y aplicadas sobre la ventana. Una vez representadas las
  acciones gráficas, el programa espera hasta que se oprima cualquier
  tecla para terminar la ejecución cerrando la ventana.
 -}
runLogoProgram :: Int         -- ^ Anchura en pixels de la ventana.
               -> Int         -- ^ Altura en pixels de la ventana.
               -> String      -- ^ Título para la ventana.
               -> LogoProgram -- ^ Instrucciones de la Máquina Logo.
               -> IO ()
runLogoProgram w h t p = 
    G.runGraphics $ do
      window <- G.openWindow t (w,h)
      G.drawInWindow window $ G.overGraphics (
        let f (Poly c p)   = G.withColor c $ G.polyline (map fix p)
            f (Text c p s) = G.withColor c $ G.text (fix p) s
            (x0,y0)        = origin w h
            fix (x,y)      = (x0 + x, y0 - y)
        in DF.toList $ fmap f (drw (execState (monadicPlot p) initial))
        )
      G.getKey window
      G.closeWindow window

origin w h = (half w, half h)
             where
               half i = round ((fromIntegral i)/2)

