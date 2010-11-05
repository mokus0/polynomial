module TestUtils where

-- arbitrary function mapping the real line to the unit interval
onUnitInterval x = x - fromIntegral (floor x)

onInterval a b x = a + w * onUnitInterval x
    where
        w = b - a

