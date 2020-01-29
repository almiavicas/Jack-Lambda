import System.Random as Random

data Palo  = Treboles | Diamantes | Picas | Corazones

instance Show

data Rango = N Int | Jack | Queen | King | Ace deriving Show

-- Notacion de registro
data Carta = Carta {
    palo  :: Palo,
    rango :: Rango
} deriving Show

data Jugador = Dealer | Player

-- Como data pero 1 solo tipo algebraico
newtype Mano = Mano [Carta] deriving Show

-- Funciones de Construccion

-- Produce una Mano vacia
vacia :: Mano
vacia = Mano []

-- Produce una Mano que contenga las 52 cartas
-- Buscar listas de compresion IMPORTANTEEEEEEEEEEEEEEEEEEEE
baraja :: Mano
baraja = Mano [
            Carta {
                palo  = Treboles,
                rango = N 2
                },
            Carta {
                palo  = Treboles,
                rango = Jack
                }
            ]
-- baraja = Mano [Carta Treboles 2, Carta Treboles 3, Carta Treboles 4,
--           Carta Treboles 5, Carta Treboles 6, Carta Treboles 7,
--           Carta Treboles 8, Carta Treboles 9, Carta Treboles 10,
--           Carta Treboles Jack, Carta Treboles Queen, Carta Treboles King,
--           Carta Treboles Ace, Carta Diamantes 2, Carta Diamantes 3,
--           Carta Diamantes 4, Carta Diamantes 5, Carta Diamantes 6,
--           Carta Diamantes 7, Carta Diamantes 8, Carta Diamantes 9,
--           Carta Diamantes 10, Carta Diamantes Jack, Carta Diamantes Queen,
--           Carta Diamantes King, Carta Diamantes Ace, Carta Picas 2,
--           Carta Picas 3, Carta Picas 4, Carta Picas 5, Carta Picas 6,
--           Carta Picas 7, Carta Picas 8, Carta Picas 9, Carta Picas 10,
--           Carta Picas Jack, Carta Picas Queen, Carta Picas King,
--           Carta Picas Ace, Carta Corazones 2, Carta Corazones 4,
--           Carta Corazones 3, Carta Corazones 5, Carta Corazones 6,
--           Carta Corazones 7, Carta Corazones 8, Carta Corazones 9,
--           Carta Corazones 10, Carta Corazones Jack, Carta Corazones Queen,
--           Carta Corazones King, Carta Corazones Ace]

-- Funciones de Acceso

-- Determina la cantidad de cartas en una mano
-- cantidad_cartas :: Mano -> Int
-- cantidad_cartas (Mano (list)) = length list

-- -- Recibe una mano y devuelve un entero con el valor de la misma
-- valor :: Mano -> Int
-- valor mano = 0

-- -- Devuelve true si el valor de la mano excede 21, y False de otra forma
-- busted :: Mano -> Bool
-- busted mano = (valor mano) > 21

-- -- Devuelve true si la mano es un blackjack y False de otra forma
-- blackjack :: Mano -> Bool
-- blackjack mano = False

-- -- Recibe la mano del dealer primero, la mano del jugador de segundo y devuelve
-- -- el ganador, segun las reglas del juego
-- ganador :: Mano -> Mano -> Jugador
-- ganador dealer player = Player

-- -- Recibe una mano y la separa en una tupla (l, c, r) de la siguiente manera:
-- -- - Si la mano es de longitud impar, c sera el elemento medio de la lista, y
-- --   l, r seran respectivamente las mitades izquierda y derecha restantes.
-- -- - Si la mano es de longitud par, l sera la mitad izquierda, c sera el primer
-- --   elemento de la mitad derecha, y r sera el resto de la mitad derecha
-- separar :: Mano -> (Mano, Carta, Mano)
-- separar mano = (Mano [], Carta Picas Ace, Mano [])

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