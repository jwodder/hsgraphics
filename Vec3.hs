module Vec3 where
 import Data.List (intercalate)

 infix  8 <.>
 infixl 6 <+>, <->

 newtype Vec3 f = Vec3 (f, f, f) deriving (Eq, Ord, Read, Show)

 instance Functor Vec3 where
  fmap f (Vec3 (x,y,z)) = Vec3 (f x, f y, f z)

 stdunits  :: Num f => [Vec3 f]
 stdunits = [Vec3 (1, 0, 0), Vec3 (0, 1, 0), Vec3 (0, 0, 1)]

 vecToList :: Vec3 f -> [f]
 vecToList (Vec3 (a,b,c)) = [a,b,c]

 listToVec :: [f] -> Vec3 f
 listToVec [a,b,c] = Vec3 (a,b,c)
 listToVec _ = undefined

 (<.>) :: Num f => Vec3 f -> Vec3 f -> f
 {-# INLINE (<.>) #-}
 Vec3 (a, b, c) <.> Vec3 (d, e, f) = a*d + b*e + c*f

 (<+>) :: Num f => Vec3 f -> Vec3 f -> Vec3 f
 {-# INLINE (<+>) #-}
 Vec3 (a, b, c) <+> Vec3 (d, e, f) = Vec3 (a+d, b+e, c+f)

 (<->) :: Num f => Vec3 f -> Vec3 f -> Vec3 f
 {-# INLINE (<->) #-}
 Vec3 (a, b, c) <-> Vec3 (d, e, f) = Vec3 (a-d, b-e, c-f)

 scale :: Num f => f -> Vec3 f -> Vec3 f
 {-# INLINE scale #-}
 scale c (Vec3 (x, y, z)) = Vec3 (c*x, c*y, c*z)

 magn2 :: Num f => Vec3 f -> f  -- square of magnitude
 magn2 v = v <.> v

 magnitude :: Floating f => Vec3 f -> f
 magnitude = sqrt . magn2

 normalize :: Floating f => Vec3 f -> Vec3 f
 normalize v = scale (recip $ magnitude v) v

 angleTwixt :: Floating f => Vec3 f -> Vec3 f -> f
 angleTwixt u v = acos $ u <.> v / magnitude u / magnitude v

 proj :: Fractional f => Vec3 f -> Vec3 f -> Vec3 f
 proj u v = scale (u <.> v / v <.> v) v  -- projects `u' onto `v'

 projFactor :: Fractional f => Vec3 f -> Vec3 f -> f
 projFactor u v = u <.> v / v <.> v

 component :: Fractional f => Vec3 f -> Vec3 f -> Vec3 f
 component u v = u <-> proj u v

 showVector :: Show f => Vec3 f -> String
 showVector v = '⟨' : intercalate ", " (map show $ vecToList v) ++ "⟩"

 vec3 :: f -> f -> f -> Vec3 f
 vec3 x y z = Vec3 (x, y, z)

 cross :: Num f => Vec3 f -> Vec3 f -> Vec3 f
 cross (Vec3 (a, b, c)) (Vec3 (d, e, f)) = Vec3 (b*f-c*e, c*d-a*f, a*e-b*d)

 mkCylindrical :: Floating a => a -> a -> a -> Vec3 a
 mkCylindrical r θ z = Vec3 (r * cos θ, r * sin θ, z)

 cylindrical :: RealFloat a => Vec3 a -> (a, a, a)
 cylindrical (Vec3 (x, y, z)) = (sqrt $ x * x + y * y, atan2 y x, z)

 -- Confirm the order & interpretation of the coordinates for these two:
 mkSpherical :: Floating a => a -> a -> a -> Vec3 a
 mkSpherical ρ θ φ = Vec3 (ρ * cos φ * cos θ, ρ * cos φ * sin θ, ρ * sin φ)

 spherical :: RealFloat a => Vec3 a -> (a, a, a)
 spherical v@(Vec3 (x,y,z)) = (magnitude v, atan2 y x, atan2 z $ sqrt $ x*x+y*y)
