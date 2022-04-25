module Lab1 where


add :: Integer -> Integer -> Integer
add x y =  x + y

type Number = Double
type Vector = [Number]
type Row = [Number]
type Matrix = [Row]

mapMatrix :: Matrix -> Vector -> Vector
mapMatrix rows v = [ sum (zipWith (*) row v) | row <- rows ]

rotatePivot :: Matrix -> Matrix
rotatePivot (row : rows) | (head row) /= 0 = (row : rows)
                         | otherwise       = rotatePivot (rows ++ [row])

triangular :: Matrix -> Matrix
triangular [] = []
triangular m  = row : (triangular rows')
  where
    (row : rows) = rotatePivot m   
    rows'        = map f rows
    f bs | (head bs) == 0 = drop 1 bs
         | otherwise      = drop 1 $ zipWith (-) (map (* c) bs) row
        where c = (head row) / (head bs) 

resubstitute :: Matrix -> Vector
resubstitute = reverse . resubstitute' . reverse . map reverse

resubstitute' :: Matrix -> Vector
resubstitute' []           = []
resubstitute' (row : rows) = x : (resubstitute' rows')
  where
    x     = (head row) / (last row)
    rows' = map substituteUnknown rows
    substituteUnknown (a1 : (a2 : as')) = ((a1 - x * a2) : as')

gauss :: Matrix -> Vector -> Vector
gauss a b = x
  where
    b' = map (\y -> [y]) b
    a' = zipWith (++) a b' 
    x  = resubstitute $ triangular a'

-- based on https://haskellicious.wordpress.com/2012/11/26/the-gauss-algorithm-in-haskell/
-- with improvements and reading the docs about what each piece means
