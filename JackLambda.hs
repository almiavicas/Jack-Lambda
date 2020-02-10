import Cartas
import qualified System.Random as Random
import System.IO  
import Control.Monad  
import Data.Char
import qualified System.Exit as Sys

data GameState = GameState {
    juegosJugados   :: Int,
    victoriasLambda :: Int,
    nombre          :: String,
    generador       :: Random.StdGen,
    dinero          :: Int,
    objetivo        :: Int,
    apuesta         :: Int
}

instance Show GameState where
    show GameState {juegosJugados = a
                , victoriasLambda = b
                , nombre = c
                , generador = d
                , dinero = e
                , objetivo = f
                , apuesta = g
                } = (show "Juegos jugados: " ++ show a ++ "\n"
                ++ show "victorias de Jack Lambda: " ++ show b ++ "\n"
                ++ show "Jugador: " ++ show c ++ "\n"
                ++ show "Dinero actual: " ++ show e ++ "\n"
                ++ show "Objetivo: " ++ show f ++ "\n"
                ++ show "Apuesta inicial: " ++ show g
                )


main :: IO ()
-- Ver si la persona quiere cargar una partida o es un nuevo usuario  
main = do  
    putStr "Desea cargar partida: Si o No "
    s <- getLine
    let gsBuilder =
            case s of "Si" -> cargar_partida
                      "si" -> cargar_partida
                      "No" -> nueva_partida
                      "no" -> nueva_partida
    gs <- gsBuilder
    
    menu gs


menu :: GameState -> IO ()
menu gs = do
    putStrLn $ ""
    putStrLn $ show gs
    putStrLn $ "Seleccione una opcion"
    putStrLn $ "1. Jugar Ronda"
    putStrLn $ "2. Guardar Partida"
    putStrLn $ "3. Cargar Partida"
    putStrLn $ "4. Salir"
    l <- getLine
    let gsBuilder =
            case l of "1" -> jugar_ronda gs
                      "2" -> do
                          guardar_partida gs
                          return gs
                      "3" -> cargar_partida
                      "4" -> Sys.exitSuccess
                      _   -> do
                          putStrLn $ "Por favor, intenta de nuevo"
                          return gs
    gs <- gsBuilder
    putStr "(Presiona enter para continuar!)"
    a <- getLine
    menu gs

-- Cuando no esta el archivo indicar que no existe
cargar_partida :: IO GameState
cargar_partida = do
    putStr "Coloque el nombre del archivo: "  
    l <- getLine 
    contents <- readFile l
    let linesOfFile = lines contents
        a = linesOfFile!!0
        b = linesOfFile!!1
        c = linesOfFile!!2
        d = linesOfFile!!3
        e = linesOfFile!!4
        f = linesOfFile!!5
    gen <- Random.getStdGen
    return GameState { 
        juegosJugados   = read a :: Int,
        victoriasLambda = read b :: Int,
        nombre          = c,
        generador       = gen,
        dinero          = read d :: Int,
        objetivo        = read e :: Int,
        apuesta         = read f :: Int
        }

--Intento de ver como verifico que el monto incial es mayor a cero
montoValido :: Int -> IO Int
montoValido minimo = do
    valor <- getLine
    let parsed = read valor :: Int
    if parsed < minimo
        then do
            putStrLn $ "Monto invalido"
            montoValido minimo
        else return parsed

montoMaximo :: Int -> Int -> IO Int
montoMaximo mini maxi = do
    valor <- getLine
    let parsed = read valor :: Int
    if parsed < mini || parsed > maxi
        then do
            putStrLn $ "Monto invalido"
            (montoMaximo mini maxi)
        else return parsed

-- Ver que el monto inciial es mayor a cero, ver que monto para ganar es mayor al inicial
-- ver que las acuertas sean menor igual al monto inicial pero mayor a 0
nueva_partida :: IO GameState
nueva_partida = do
    putStr "Nombre del jugador: "
    a <- getLine
    putStr "Monto inicial: "
    b <- montoValido 0
    putStr "Cantidad de dinero para ganar la partida: "
    c <- montoValido b
    putStr "Apuestas por ronda: "
    d <- montoMaximo 0 b
    gen <- Random.getStdGen
    return GameState { 
        juegosJugados   = 0,
        victoriasLambda = 0,
        nombre          = a,
        generador       = gen,
        dinero          = b,
        objetivo        = c,
        apuesta         = d
        }
    

guardar_partida :: GameState -> IO GameState
guardar_partida GameState { 
    juegosJugados   = jj,
    victoriasLambda = vl,
    nombre          = n,
    generador       = g,
    dinero          = d,
    objetivo        = o,
    apuesta         = a
    } = do
        return GameState {
            juegosJugados   = jj,
            victoriasLambda = vl,
            nombre          = n,
            generador       = g,
            dinero          = d,
            objetivo        = o,
            apuesta         = a
            }

-- Menu que aparece al empezar a jugar 
jugar_ronda :: GameState -> IO GameState
jugar_ronda GameState { 
    juegosJugados   = jj,
    victoriasLambda = vl,
    nombre          = n,
    generador       = g,
    dinero          = d,
    objetivo        = o,
    apuesta         = a
    } = do
        let manoCompleta = barajar g (baraja)
            nuevo_dinero = d - a
        let (Mano manoJack, manoCompleta2) = inicialLambda manoCompleta
        putStr $ n ++ "esta es mi primera carta: "
        putStrLn $ show (manoJack!!0)
        let (_, gen) = Random.split g
        if blackjack (Mano manoJack)
        then do
            putStrLn $ n ++ ", he sacado blackjack. Yo gano"
            if nuevo_dinero < a
            then do
                putStrLn $ n ++ ", no te queda dinero. Es el fin del juego para ti."
                Sys.exitSuccess
            else
                return GameState {
                    juegosJugados   = jj + 1,
                    victoriasLambda = vl + 1,
                    nombre          = n,
                    generador       = gen,
                    dinero          = nuevo_dinero,
                    objetivo        = o,
                    apuesta         = a
                }
        else do
            let (Mitad carta mazo1 mazo2 ) = desdeMano manoCompleta2
            let manoJugador = Mano [carta]
            choice <- pedirLado n False
            let Just (mazoResultado, manoJugador) = robar (Mitad carta mazo1 mazo2) manoJugador choice
            mostrarMano n manoJugador
            if blackjack manoJugador
            then putStrLn $ n ++ ", tu mano es un blackjack"
            else putStrLn $ "Suma " ++ show (valor manoJugador)
            let money = nuevo_dinero
                bid   = a
            finalAcciones <- accion manoJugador mazoResultado n money bid

            case finalAcciones of Nothing -> if nuevo_dinero < a
                                             then do
                                                 putStrLn $ n ++ ", no te queda dinero. es el fin del juego para ti."
                                                 Sys.exitSuccess
                                             else
                                                return GameState {
                                                        juegosJugados   = jj + 1,
                                                        victoriasLambda = vl + 1,
                                                        nombre          = n,
                                                        generador       = gen,
                                                        dinero          = nuevo_dinero,
                                                        objetivo        = o,
                                                        apuesta         = a
                                                    }
                                  _       -> do
                                             let Just (manoFinalJugador, cartasRestantes, apuestaFinal) = finalAcciones
                                             return GameState {
                                                 juegosJugados   = jj,
                                                 victoriasLambda = vl,
                                                 nombre          = n,
                                                 generador       = gen,
                                                 dinero          = d,
                                                 objetivo        = o,
                                                 apuesta         = a
                                                 }


pedirLado :: String -> Bool -> IO Eleccion
pedirLado n True
    = do
        putStrLn $ "Por favor, ingrese una opcion valida"
        pedirLado n False
pedirLado n False
    = do
        putStrLn $ n ++ ", ¿robarás de la izquierda o de la derecha?"
        putStrLn $ "Izquierda (i)"
        putStrLn $ "Derecha (d)"
        putStrLn $ "Ingrese su opcion: "
        choice <- getLine
        if choice == "i" || choice == "I"
        then return Izquierdo
        else if choice == "d" || choice == "D"
        then return Derecho
        else pedirLado n True
        
mostrarMano :: String -> Mano -> IO ()
mostrarMano n (Mano cartas) = putStrLn $ n ++ ", tu mano es " ++ show cartas

accion :: Mano -> Mazo -> String -> Int -> Int -> IO (Maybe (Mano, Mano, Int))
accion mano mazo id dinero apuesta = accion_aux mano mazo (Mano []) id dinero apuesta

accion_aux :: Mano -> Mazo -> Mano -> String -> Int -> Int -> IO (Maybe (Mano, Mano, Int))
accion_aux mano mazo mano2 id dinero apuesta
    | valor mano > 21 = return Nothing
    | otherwise       =
                      do
                      putStrLn $ "Ingrese una accion:"
                      putStrLn $ "1. Hit"
                      
                      a <- getLine
                      case a of "1" -> do
                                       let (Mitad _ izq der) = mazo
                                       choice <- pedirLado id False
                                       let Just (mazoRes, manoRes) = robar mazo mano choice
                                    --    if choice == Izquierdo
                                    --    then let (Mano botada) = aplanar der
                                    --    else let (Mano botada) = aplanar izq
                                       mostrarMano id manoRes
                                       if valor manoRes > 21
                                       then do
                                           putStrLn $ "Suma " ++ show (valor manoRes) ++ ". Perdiste"
                                           return Nothing
                                       else accion_aux manoRes mazoRes mano2 id dinero apuesta
                                _   -> do
                                       putStrLn $ "Por favor, intenta de nuevo"
                                       accion_aux mano mazo mano2 id dinero apuesta

        