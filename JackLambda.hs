data GameState = GS {
    juegosJugados   :: Int,
    virtoriasLambda :: Int,
    nombre          :: String,
    generador       :: StdGen,
    dinero          :: Int,
    objetivo        :: Int,
    apuesta         :: Int
}

main :: IO ()