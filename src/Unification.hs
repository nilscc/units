{-# OPTIONs -fno-warn-name-shadowing #-}

import Data.List

type UnitName = String
type UnitVar  = String

data Units
  = UnitName   UnitName
  | UnitVar    UnitVar
  | UnitIdent
  | UnitProd   Units Units
  | UnitInvers Units
  deriving Eq

instance Show Units where
  show uns = let (us,bs) = powers uns
              in if null us && null bs
                    then "1"
                    else intercalate " * " $
                           map (\(uv,p) ->        uv ++ if p == 1 then "" else ("^" ++ show p)       ) us
                           ++
                           map (\(un,p) -> "<" ++ un ++ if p == 1 then "" else ("^" ++ show p) ++ ">") bs


---------------------------------------------------
--              Unit unification
---------------------------------------------------

unify :: Units -> Units -> Units
unify u v = unifyOne (UnitProd u (UnitInvers v))

unifyOne :: Units -> Units
unifyOne mu =
  let (us, bs)     = powers mu
      m            = length us
      n            = length bs
      ys           = map snd bs

      ((u1,x1):ur) = us
      step         = replaceU (UnitVar u1) $ foldr UnitProd (UnitVar u1)
                                                   (   map (\(uv,p) -> UnitVar  uv `powerU` (- gauss p x1)) ur
                                                    ++ map (\(un,p) -> UnitName un `powerU` (- gauss p x1)) bs)
      result       = replaceU (UnitVar u1) $ foldr UnitProd UnitIdent
                                                   (map (\(un,p) -> UnitName un `powerU` (- quot p x1)) bs)

   in      if m == 0 && n == 0                then UnitIdent
      else if m == 0 && n /= 0                then error "Unification error: No more unit variables left!"
      else if m == 1 && all (x1 `divides`) ys then result mu
      else if m == 1                          then error "Unification error: Dimension mismatch"
      else unifyOne (step mu)
 where
  x `divides` y = y `mod` x == 0

  gauss :: Int -> Int -> Int
  gauss x y = floor (fromIntegral x / fromIntegral y :: Double)


---------------------------------------------------
--                  Helpers
---------------------------------------------------

replaceU :: Units -> Units -> Units -> Units
replaceU u u' us
  | u == us = u'
  | otherwise = case us of
    UnitName _     -> us
    UnitVar  _     -> us
    UnitIdent      -> us
    UnitProd pu pv -> UnitProd   (replaceU u u' pu) (replaceU u u' pv)
    UnitInvers  pv -> UnitInvers (replaceU u u' pv)

powerU :: Units -> Int -> Units
powerU u p
  | p < 0     = UnitInvers $ powerU u (negate p)      -- x^(-y) = 1/(x^y)
  | p == 0    = UnitIdent                             -- x^0    = 1
  | p == 1    = u                                     -- x^1    = x
  | otherwise = foldr UnitProd u (replicate (p-1) u)  -- x^y    = x*x*x*x..*x (y times)

powers :: Units -> ( [(UnitVar, Int)], [(UnitName, Int)] )
powers u =
  ( sortOn snd $ foldr count [] (groupOn fst . sortOn fst $ unitVars  u)
  , sortOn snd $ foldr count [] (groupOn fst . sortOn fst $ unitNames u) )
 where
  count []          r = r
  count l@((x,_):_) r
    | total l == 0    = r
    | otherwise       =  (x, total l) : r

  total l = length (filter (not . snd) l) - length (filter (snd) l)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> f a == f b)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (\a b -> compare (f a) (f b))

unitVars :: Units -> [(UnitVar, Bool)]
unitVars us = unitVars' us False
 where
  unitVars' us' b = case us' of
    UnitName   _   -> []
    UnitVar    v   -> [(v,b)]
    UnitIdent      -> []
    UnitInvers u   -> unitVars' u (not b)
    UnitProd   u v -> unitVars' u b ++ unitVars' v b

unitNames :: Units -> [(UnitName, Bool)]
unitNames us = unitNames' us False
 where
  unitNames' us' b = case us' of
    UnitName   v   -> [(v, b)]
    UnitVar    _   -> []
    UnitIdent      -> []
    UnitInvers u   -> unitNames' u (not b)
    UnitProd   u v -> unitNames' u b ++ unitNames' v b


---------------------------------------------------
--                  Examples
---------------------------------------------------


kg, m, s :: Units
kg = UnitName "kg"
m  = UnitName "m"
s  = UnitName "s"

u, v :: Units
u = UnitVar "u"
v = UnitVar "v"

-- Unification example, see
--
--   http://research.microsoft.com/en-us/um/people/akenn/units/MLWorkshop2008.pdf
--
-- page 25, unify the term:
--
--          u^3 * v^2         = kg^6         | * kg^-6
-- =>       u^3 * v^2 * kg^-6 = 1            v -> v * u^-1 * kg^3
-- =>       u   * v^2         = 1            u -> u * v^-2
-- =>       u                 = 1            u -> 1
-- =>                       1 = 1

-- The variables
kg_6, u_3, v_2 :: Units

kg_6 = kg `powerU` 6
u_3  = u  `powerU` 3
v_2  = v  `powerU` 2

-- Step for step by hand
line2, line3, line4, line5 :: Units
line2 = u_3 `UnitProd` v_2 `UnitProd` UnitInvers kg_6
line3 = replaceU v (v `UnitProd` UnitInvers u `UnitProd` (kg `powerU` 3)) line2
line4 = replaceU u (u `UnitProd` UnitInvers (v `powerU` 2)) line3
line5 = replaceU u UnitIdent line4

-- the big example
exampleResult :: Units
exampleResult = unify (u_3 `UnitProd` v_2) kg_6
