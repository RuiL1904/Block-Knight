module Assets where

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG, loadJuicyJPG)

-- | O conjunto das imagens inerentes ao programa.
data Imagens = Imagens {
    background1 :: Picture,
    background2 :: Picture,
    background3 :: Picture,
    background4 :: Picture,
    mpJogar :: Picture,
    mpOpcoes :: Picture,
    eBackground1 :: Picture,
    eBackground2 :: Picture,
    eBackground3 :: Picture,
    eBackground4 :: Picture,
    mpCreditos :: Picture,
    mpSair :: Picture,
    creditos :: Picture,
    mjEscolhe :: Picture,
    mjArcade :: Picture,
    mjCarregar :: Picture,
    mj1 :: Picture,
    mj2 :: Picture,
    mj3 :: Picture,
    mj4 :: Picture,
    mj5 :: Picture,
    mj6 :: Picture,
    mj7 :: Picture,
    mj8 :: Picture,
    mj9 :: Picture,
    mj10 :: Picture,
    setaBaixo :: Picture,
    setaCima :: Picture,  
    bloco :: Picture,
    caixa :: Picture,
    porta :: Picture,
    knightLeft :: Picture,
    knightRight :: Picture,
    victory :: Picture
    -- Adicionar as imagens necessárias aqui no futuro.
}

-- Carrega todas as imagens para o data type 'Imagens'.
carregaImagens :: IO Imagens
carregaImagens = do
    Just background1 <- loadJuicyJPG "assets/background1.jpg"
    Just background2 <- loadJuicyJPG "assets/background2.jpg"
    Just background3 <- loadJuicyJPG "assets/background3.jpg"
    Just background4 <- loadJuicyJPG "assets/background4.jpg"
    -- MenuPrincipal.
    Just mpJogar <- loadJuicyJPG "assets/mpJogar.jpg"
    Just mpOpcoes <- loadJuicyJPG "assets/mpOpcoes.jpg"
    Just eBackground1 <- loadJuicyJPG "assets/eBackground1.jpg"
    Just eBackground2 <- loadJuicyJPG "assets/eBackground2.jpg"
    Just eBackground3 <- loadJuicyJPG "assets/eBackground3.jpg"
    Just eBackground4 <- loadJuicyJPG "assets/eBackground4.jpg"
    Just mpCreditos <- loadJuicyJPG "assets/mpCreditos.jpg"
    Just mpSair <- loadJuicyJPG "assets/mpSair.jpg"
    -- Opções do MenuPrincipal.
    Just creditos <- loadJuicyJPG "assets/creditos.jpg"
    -- MenuJogar.
    Just mjEscolhe <- loadJuicyJPG "assets/mjEscolhe.jpg"
    Just mjArcade <- loadJuicyJPG "assets/mjArcade.jpg"
    Just mjCarregar <- loadJuicyJPG "assets/mjCarregar.jpg"
    Just mj1 <- loadJuicyJPG "assets/mjMapa1.jpg"
    Just mj2 <- loadJuicyJPG "assets/mjMapa2.jpg"
    Just mj3 <- loadJuicyJPG "assets/mjMapa3.jpg"
    Just mj4 <- loadJuicyJPG "assets/mjMapa4.jpg"
    Just mj5 <- loadJuicyJPG "assets/mjMapa5.jpg"
    Just mj6 <- loadJuicyJPG "assets/mjMapa6.jpg"
    Just mj7 <- loadJuicyJPG "assets/mjMapa7.jpg"
    Just mj8 <- loadJuicyJPG "assets/mjMapa8.jpg"
    Just mj9 <- loadJuicyJPG "assets/mjMapa9.jpg"
    Just mj10 <- loadJuicyJPG "assets/mjMapa10.jpg"
    Just setaBaixo <- loadJuicyPNG "assets/setaBaixo.png"
    Just setaCima <- loadJuicyPNG "assets/setaCima.png"   
    -- Outros.
    Just bloco <- loadJuicyPNG "assets/bloco.png"
    Just caixa <- loadJuicyPNG "assets/caixa.png"
    Just porta <- loadJuicyPNG "assets/porta.png"
    Just knightLeft <- loadJuicyPNG "assets/knightLeft.png"
    Just knightRight <- loadJuicyPNG "assets/knightRight.png"
    Just victory <- loadJuicyJPG "assets/victory.jpg"
    
    return (Imagens background1 background2 background3 background4 mpJogar mpOpcoes eBackground1 eBackground2 eBackground3 eBackground4 mpCreditos mpSair creditos mjEscolhe mjArcade mjCarregar mj1 mj2 mj3 mj4 mj5 mj6 mj7 mj8 mj9 mj10 setaBaixo setaCima bloco caixa porta knightLeft knightRight victory)
