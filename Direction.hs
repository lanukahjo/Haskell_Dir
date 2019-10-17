module Direction
where

data Direction = L | R | S
                 deriving(Show)

dir :: (Float,Float)->(Float,Float)->(Float,Float)->Direction
dir a b c = let p = crossProduct (fst b - fst a , snd b - snd a) (fst c - fst b , snd c - snd b)
            in
            if (p > 0)
            then
            L
            else if (p < 0)
                 then
                 R 
                 else
                 S

crossProduct :: (Float,Float)->(Float,Float)->Float
crossProduct a b = (-) ((*) (fst a) (snd b)) ((*) (snd a) (fst b))