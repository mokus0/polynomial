module TestUtils where

-- arbitrary function mapping the real line to the unit interval
onUnitInterval x = x - fromIntegral (floor x)

onInterval a b x = a + w * onUnitInterval x
    where
        w = b - a

distinct [] = True
distinct (x:xs) = all (/= x) xs && distinct xs

relErr 0 0 = 0
relErr x y = abs (x - y) / max (abs x) (abs y)
