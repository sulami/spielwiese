-- Some Mathematical Haskell
-- This is in German, in case you are wondering.
-- As usual, ISC licensed by Robin 'sulami' Schroer

-- Ein Versuch mit einer Anzahl von Durchführungen n und einer
-- Wahrscheinlichkeit n, wobei 0 <= n <= 1.
data Versuch = Versuch {
    n :: Int,
    p :: Float
    } deriving (Show)

newVersuch :: Int -> Float -> Versuch
newVersuch n p = Versuch { n=n, p=p }

-- Errechne den Erwartungswert μ für einen Versuch.
erwWrt :: Versuch -> Float
erwWrt (Versuch {n=n, p=p}) = fromIntegral n * p

-- Errechne die Standardabweichung σ für einen Versuch.
stdAbw :: Versuch -> Float
stdAbw (Versuch {n=n, p=p}) = sqrt $ fromIntegral n * p * (1-p)

-- Errechne die obere und untere Signifikanzgrenze für einen Versuch. Benutzt P
-- direkt entsprechend der Sigmaregel (P(95%) = 1.96, P(99%) = 2.58)
sigGrn :: Versuch -> Float -> (Float, Float)
sigGrn v p = (erwWrt v - p * stdAbw v, erwWrt v + p * stdAbw v)

