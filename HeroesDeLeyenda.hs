

-- commit incial

data Heroe = UnHeroe {

    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [(String, Int)],
    tareas :: [Tarea]

}

pasaHistoria :: Heroe -> Heroe
pasaHistoria unHeroe
    | reconocimiento unHeroe > 1000                                = modificarEpiteto "El mitico" unHeroe
    | reconocimiento unHeroe >= 500                                = modificarArtefacto "Lanza del olimpo" 100 . modificarEpiteto "el magnifico" $ unHeroe 
    | reconocimiento unHeroe < 500 && reconocimiento unHeroe > 100 = modificarArtefacto "Xiphos" 50 . modificarEpiteto "Hoplita" $ unHeroe
    | otherwise                                                    = unHeroe



modificarEpiteto :: String -> Heroe -> Heroe
modificarEpiteto nuevoEpiteto unHeroe = unHeroe { epiteto = nuevoEpiteto}

modificarArtefacto :: String -> Int -> Heroe -> Heroe
modificarArtefacto nuevoArtefacto nuevaRareza unHeroe = unHeroe {
    artefactos = (nuevoArtefacto, nuevaRareza) : artefactos unHeroe
}

type Tarea = Heroe -> Heroe
type Artefacto = (String, Int)

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto (nombre, rareza) = modificarReconocimientoSegunRareza rareza . modificarArtefacto nombre rareza

modificarReconocimientoSegunRareza :: Int -> Tarea
modificarReconocimientoSegunRareza rareza unHeroe = unHeroe {reconocimiento = reconocimiento unHeroe + rareza}

escalarElOlimpo :: Tarea
escalarElOlimpo = modificarArtefacto "El relámpago de Zeus" 500 . desecharArtefactos 1000 . triplicarRareza . sumarReconocimiento 500 

