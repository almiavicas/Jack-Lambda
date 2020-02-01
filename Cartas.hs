import qualified System.Random as Random
import Data.List (splitAt)

-- -- -- -- -- -- Funciones Auxiliares -- -- -- -- -- --
valorMano :: [Int] -> Int
valorMano [] = 0
valorMano valores
    | sum valores > 21 = sum ([x | x <- valores, x < 11] ++ [1 | x <- valores, x >= 11])
    | otherwise        = sum valores

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Palo  = Treboles | Diamantes | Picas | Corazones

instance Show Palo where
    show Treboles  = "\9827"
    show Diamantes = "\9830"
    show Picas     = "\9828"
    show Corazones = "\9829"

data Rango = N Int | Jack | Queen | King | Ace

instance Show Rango where
    show (N a) = show a
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

instance Eq Rango where
    Jack == Jack = True
    Queen == Queen = True
    King == King = True
    Ace == Ace = True
    N a == N b = a == b
    _ == _ = False

-- Notacion de registro
data Carta = Carta {
    palo  :: Palo,
    rango :: Rango
}

instance Show Carta where
    show Carta {palo = a, rango = b} = show a ++ show b

data Jugador = Dealer | Player

-- Como data pero 1 solo tipo algebraico
newtype Mano = Mano [Carta] deriving Show

-- Funciones de Construccion

-- Produce una Mano vacia
vacia :: Mano
vacia = Mano []

-- Produce una Mano que contenga las 52 cartas
baraja :: Mano
baraja = Mano [
            Carta {
                palo  = x,
                rango = y
            } | x <- [Treboles, Diamantes, Picas, Corazones],
                y <- [N z | z <- [2..10]] ++ [Jack, Queen, King, Ace]
        ]

-- Funciones de Acceso

-- Determina la cantidad de cartas en una mano
cantidad_cartas :: Mano -> Int
cantidad_cartas (Mano (list)) = length list



-- Recibe una mano y devuelve un entero con el valor de la misma
-- La funcion convierte cada carta al numero de su valor correspondiente
-- Cuando termina de hacer esto, calcula la suma de los valores convertidos
-- Si la suma excede 21, convierte el valor de aces de 11 a 1
valor :: Mano -> Int
valor (Mano []) = 0
valor (Mano cartas) = valorMano $ valores where
        valores = [num | Carta palo (N num) <- cartas]
                    ++ [10 | Carta palo rango <- cartas, rango == Jack || rango == Queen || rango == King]
                    ++ [11 | Carta palo rango <- cartas, rango == Ace]
    


-- Devuelve true si el valor de la mano excede 21, y False de otra forma
busted :: Mano -> Bool
busted mano = valor mano > 21

-- Devuelve true si la mano es un blackjack y False de otra forma
blackjack :: Mano -> Bool
blackjack mano = valor mano == 21 && cantidad_cartas mano == 2

-- Recibe la mano del dealer primero, la mano del jugador de segundo y devuelve
-- el ganador, segun las reglas del juego
ganador :: Mano -> Mano -> Jugador
ganador dealer player
    | busted player                = Dealer
    | blackjack dealer             = Dealer
    | busted dealer                = Player
    | valor dealer >= valor player = Dealer
    | otherwise                    = Player

-- Recibe una mano y la separa en una tupla (l, c, r) de la siguiente manera:
-- - Si la mano es de longitud impar, c sera el elemento medio de la lista, y
--   l, r seran respectivamente las mitades izquierda y derecha restantes.
-- - Si la mano es de longitud par, l sera la mitad izquierda, c sera el primer
--   elemento de la mitad derecha, y r sera el resto de la mitad derecha
separar :: Mano -> (Mano, Carta, Mano)
separar (Mano cartas) = (\(left, (mid:right)) -> (Mano left, mid, Mano right))
                        $ splitAt half cartas 
                        where
                            half = div (length cartas) 2

-- -- Funciones de Modificacion

-- -- Esta funcion sera usada para barajar las cartas al inicio de cada ronda.
-- -- Para ello, es necesario el tipo de datos StdGen, incluido en el modulo
-- -- System.Random. El segundo argumento es la Mano a barajar, y debe devolverse
-- -- la mano ya barajada. Para ello, se debe empezar por una Mano vacia para
-- -- acumular
-- barajar :: Random.StdGen -> Mano -> Mano
-- barajar gen mano = Mano []

-- -- Recibe la baraja inicial barajada como Mano, y devuelve la Mano inicial de
-- -- Lambda tomando las dos primeras cartas, y la baraja resultante de retirar
-- -- dichas cartas.
-- inicialLambda :: Mano -> (Mano, Mano)
-- inicialLambda mano = (Mano [], Mano [])

-- data Mazo = Vacio | Mitad Carta Mazo Mazo

-- data Eleccion = Izquierdo | Derecho

-- -- Funciones de Construccion

-- -- Recibe una Mano y produce un Mazo. El mazo generado debe ser un arbol
-- -- binario balanceado por altura. El arbol construido es tal que su recorrido
-- -- inorden forme la Mano usada para construirlo. Una de las funciones
-- -- propuestas de Mano es util para esto
-- desdeMano :: Mano -> Mazo
-- desdeMano mano = Vacio

-- -- Funciones de Acceso

-- -- Recibe un mazo y devuelve True si no es vacio y ninguno de sus hijos es
-- -- vacio
-- puedePicar :: Mazo -> Bool
-- puedePicar mazo = False

-- -- Funciones de Modificacion

-- -- Recibe un Mazo y produce una Mano tal que si se le aplicara desdeLista,
-- -- deberia producir el mismo Mazo
-- aplanar :: Mazo -> Mano
-- aplanar mazo = Mano []

-- -- Recibe el Mazo original y una Mano con las cartas jugadas en la ronda. Debe
-- -- eliminar todas las cartas jugadas del Mazo, y luego debe reconstruirlo,
-- -- produciendo un Mazo que siga las mismas reglas de construcicion que
-- -- desdeMazo. 
-- reconstruir :: Mazo -> Mano -> Mazo
-- reconstruir mazo mano = mazo

-- -- Recibe el Mazo actual, la mano del jugador y una Eleccion. Debe devolver una
-- -- tupla con el Mazo resultante (la eleccion indica que hijo del Mazo se usara)
-- -- y la mano resultante. Devolvera Nothing si no quedan cartas que sacar (
-- -- aunque esto no deba ocurrir)
-- -- robar :: Mazo -> Mano -> Eleccion -> Maybe (Mazo, Mano)

-- -- Recibe el mazo actual, lo vuelve a convertir en una Mano en el orden
-- -- apropiado, recibe la Mano del dealer, y devuelve la mano resultante de robar
-- -- hasta que supere un valor de 16. Devolvera Nothing si no quedan cartas que
-- -- sacar (aunque esto no deba ocurrir)
-- -- jugarLambda :: Mazo -> Mano -> Maybe Mano