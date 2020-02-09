module Cartas where
import qualified System.Random as Random
import System.IO  
import Control.Monad  
import Data.Char
import qualified System.Exit as Sys

data GameState = GameState {
    juegosJugados   :: Int,
    virtoriasLambda :: Int,
    nombre          :: String,
    generador       :: Random.StdGen,
    dinero          :: Int,
    objetivo        :: Int,
    apuesta         :: Int
}

main :: IO ()
-- Ver si la persona quiere cargar una partida o es un nuevo usuario  
main = do  
    putStr "Desea cargar partida: Si o No "
    s <- getLine  
    case s of "Si" -> cargar_partida
              "si" -> cargar_partida
              "No" -> nueva_partida
              "no" -> nueva_partida

    forever $ do 
    putStr "Seleccione una opcion: " 
    let a = "\n" ++ "1. Jugar ronda"  
        b = "2. Guardar partida"  
        c = "3. Cargar partida"
        d = "4. Salir"
    putStrLn $ a
    putStrLn $ b
    putStrLn $ c
    putStrLn $ d    
    l <- getLine
    case l of "1" -> jugar_ronda 
              --"2" -> print nuevo_juego
              "3" -> cargar_partida
              "4" -> Sys.exitSuccess
              _   -> print "Intenta de nuevo"

-- Cuando no esta el archivo indicar que no existe
cargar_partida = do
    putStr "Coloque el nombre del archivo: "  
    l <- getLine 
    handle <- openFile l ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle

--Intento de ver como verifico que el monto inciial es mayor a cero
montoini :: Integer -> Integer -> String
montoini a b
    | a < b  = "Monto incorrecto"
    | a > b  = "A is greater than B"

-- Ver que el monto inciial es mayor a cero, ver que monto para ganar es mayor al inicial
-- ver que las acuertas sean menor igual al monto inicial pero mayor a 0
nueva_partida = do
    putStr "Nombre del jugador: "
    a <- getLine
    putStr "Monto inicial: "  
    b <- getLine
    putStr "Cantidad de dinero para ganar la partida: "
    c <- getLine
    putStr "Apuestas por ronda: "
    d <- getLine
    let state = GameState { juegosJugados = "0", virtoriasLambda = "0", nombre = a, generador = "nose", dinero = b, objetivo = c, apuesta =d        
    }
    
-- Menu que aparece al empezar a jugar 
jugar_ronda =
    forever $ do 
    putStr "Empiezo el juego!" 
    putStr "Que deseas hacer: "
    let a = "\n" ++ "Hit"  
        b = "2. Stand"  
        c = "3. Doble Down"
        d = "4. Surrender"
    putStrLn $ a
    putStrLn $ b
    putStrLn $ c
    putStrLn $ d
    case l of "1" -> Hit 
              --"2" -> Stand
              "3" -> Doble Down
              "4" -> Surrender
              _   -> print "Intenta de nuevo"



