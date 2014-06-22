{- TODO:
 * Implement the following objects:
  * solid balls
  * filled cylinders
  * unfilled cylinders with bases
  * sections of spheres?
  * discs?
  * parallelipipeds
   * blocks (rectangular prisms), filled and unfilled
  * tori
 * Implement surfaces for which coloring is a function of location
 * How should polygons handle rays lying within their plane?
 * 'sphere' and 'cylinderShell': Handle 'orig' being on the surface
 * How should insideness be determined for rays lying within a plane or
   cylinderShell?
 * Figure out why 'plane' seems to return a negated normal when both the input
   normal and `d' are negated (cf. the differences in shadowing when the latter
   two vector arguments to 'parallelogram' or 'triangle' in scene01.txt are
   transposed or when the `n' value passed to 'regular' is negated)
 * Continue following along with
   <http://www.devmaster.net/articles/raytracing_series/part3.php> (stopped
   while trying to figure out why his refraction results are slightly different
   from mine)
-}

module Raytrace (
  -- * Types
  Ray, Intersectable, Material(..), Object, Scene,
  -- * Raytracing
  traceRay, intersections,
  -- * Intersectables
  -- ** 3D objects
  sphere, cylinderShell,
  -- ** Polygons
  polygon, regularPoly, triangle, parallelogram,
  -- ** Other
  plane,
  -- * Lights
  mkLight, lights, isLight,
  -- * Colors
  RTColor, rtcolor, (%+), (%%), (*%)
 ) where
 import Control.Monad (guard)
 import Math (quadratic')
 import Math.Vector
 import MoreData.Lists (pairNext')
 import Raster.Color

 type Ray a = (Vec3 a, Vec3 a)  -- origin and direction

 type Intersectable a = Ray a -> Maybe (a, Vec3 a, Bool)
  -- The 'a' value is the factor by which the direction vector is scaled so
  -- that adding it to the the origin vector yields the closest point on the
  -- object.  The Vec3 is the unit vector normal to the surface at the point of
  -- intersection.  The Bool is True iff the ray hit the object from "inside."

 data Material a = Material {
  color       :: RTColor a,
  diffusion   :: a,
  specularity :: a,
  reflection  :: a,
  refracRatio :: a,  -- outer index of refraction to inner index of refraction
  lightCenter :: Maybe (Vec3 a)  -- center of a spherical light
 } deriving (Eq, Ord, Read, Show)

 type Object a = (Intersectable a, Material a)

 type Scene a = [Object a]

-- Raytracing: ----------------------------------------------------------------

 intersections :: RealFloat a => Scene a -> Ray a
  -> [(a, Vec3 a, Bool, Material a)]
 intersections scene ray = [(ð, n, s, matter) | (f, matter) <- scene,
  Just (ð, n, s) <- [f ray]]

 traceRay :: RealFloat a => Scene a -> Int -> Ray a -> RTColor a
 traceRay _ d _ | d < 1 = black
 traceRay scene d ray@(orig, dir) =
  if null hits then black
  else if isLight matter then white
  else foldl (%+) black [
    max 0 (diffusion matter) * max 0 (n <.> m) *% color matter %% c
     %+ max 0 (specularity matter) * (max 0 q)^20 *% c
    | (c, l) <- lights scene,
      let m = normalize (l <-> p),
      let q = dir' <.> (m <-> scale (2 * m <.> n) n),
      all (\(x, _, _, t) -> isLight t || x >= magnitude (l <-> p))
	  (intersections scene (p <+> scale epsilon m, m))
   ] %+ max 0 (reflection matter)
	*% traceRay scene (d-1) (p <+> scale epsilon r, r)
	%% color matter
   %+ (if index > 0 && sin2θ <= 1
       then traceRay scene (d-1) (p <+> scale epsilon refr, refr)
       else black)
  where hits = intersections scene ray
	(ð, n, s, matter) = minimum hits
	p = orig <+> scale ð dir
	dir' = normalize dir
	r = dir' <-> scale (2 * cosθ) n
	index = (if s then recip else id) (refracRatio matter)
      --index = if s then 1 else refracRatio matter -- for Bikker compatibility
	cosθ = dir' <.> n
	sin2θ = index^2 * (1 - cosθ^2)
	refr = scale index $ dir' <-> scale (cosθ + sqrt (1 - sin2θ)) n

-- Intersectables: ------------------------------------------------------------

 sphere :: RealFloat a => Vec3 a -> a -> Intersectable a
 sphere center r (orig, dir) = do
  sol <- minST2 (>= 0) =<< quadratic' (magn2 dir)
				      (2 * dir <.> (orig <-> center))
				      (magn2 (orig <-> center) - r*r)
  wrapUp sol (orig<+>scale sol dir<->center) (magnitude (orig<->center) < r)

 cylinderShell :: RealFloat a => Ray a -> a -> Intersectable a
 -- hollow walls of a cylinder (sans bases); arguments: axis and radius
 cylinderShell (axOrig, axDir) r (orig, dir) = case (a, b, c) of
  (0, 0, 0) | alongAxis 0 -> finalize 0
  (0, 0, 0) -> finalize =<< minST (>= 0) [((axOrig <-> orig) <.> axDir + i)
					  / dir <.> axDir | i <- [0,1]]
  (0, 0, _) -> Nothing
  (0, _, _) -> let ð = -c/b in if alongAxis ð then finalize ð else Nothing
  (_, _, _) -> quadratic' a b c >>= minST2 alongAxis >>= finalize
  where alef = component dir axDir
	bet  = component (orig <-> axOrig) axDir
	a = alef <.> alef
	b = 2 * alef <.> bet
	c = bet <.> bet - r*r
	alongAxis ð = ð >= 0 && 0 <= l && l <= 1
	 where l = projFactor (orig <+> scale ð dir <-> axOrig) axDir
	finalize ð = wrapUp ð (component sol axDir) (magnitude bet < r)
	 where sol = orig <+> scale ð dir <-> axOrig

 plane :: RealFloat a => Vec3 a -> a -> Intersectable a
 -- arguments: normal vector, distance from origin along normal vector
 plane n d (orig, dir) = if denom == 0  -- ray and plane are parallel
			 then if d' == d
			      then Just (0, normalize n, False)
			      else Nothing
			 else if ð >= 0 then wrapUp ð n revsign
			 else Nothing
  where denom = dir <.> n
	d' = projFactor orig n
	ð = (scale d n <-> orig) <.> n / denom
	revsign = if d == 0 then (d' < 0 || d' == 0 && denom > 0) else d' < d

 -- |@polygon n d verts@ creates an 'Intersectable' for a (convex?) polygon
 -- with vertices @verts@ lying in @plane n d@.  It is assumed that the given
 -- vertices actually lie in this plane and that each vertex is adjacent to its
 -- neighbors in the list, cycling around at the ends.
 polygon :: RealFloat a => Vec3 a -> a -> [Vec3 a] -> Intersectable a
 polygon n d verts (orig, dir) = do
  guard $ n /= Vec3 (0, 0, 0)
  (ð, n', s) <- plane n d (orig, dir)
  guard $ ingon n verts $ orig <+> scale ð dir
  return (ð, n', s)

 triangle :: RealFloat a => Vec3 a -> Vec3 a -> Vec3 a -> Intersectable a
 triangle a b c = polygon n (projFactor b n) [a, b, c]
  where n = cross (b <-> a) (b <-> c)

 parallelogram :: RealFloat a => Vec3 a -> Vec3 a -> Vec3 a -> Intersectable a
 -- arguments: a corner, vectors to vertices adjacent to that corner
 parallelogram v e1 e2 = polygon n (projFactor v n)
  [v, v <+> e1, v <+> e1 <+> e2, v <+> e2]
  where n = cross e1 e2

 regularPoly :: RealFloat a => Vec3 a -> Vec3 a -> Vec3 a -> Int
  -> Intersectable a
 regularPoly norm center rad n (orig, dir) = do
  guard $ n >= 3
  guard $ norm /= Vec3 (0, 0, 0)
  guard $ norm <.> rad == 0
  let apothem = magnitude rad * cos (pi / fromIntegral n)  -- length of apothem
      θ = 2 * pi / fromIntegral n
  (ð, n', s) <- plane norm (projFactor center norm) (orig, dir)
  let p = orig <+> scale ð dir <-> center
      pr = magnitude p
      pφ = abs $ angleTwixt p rad
      pθ = until (< θ) (subtract θ) pφ
  guard $ pr <= apothem
   || not (isNaN pφ) && not (isInfinite pφ) && pr * cos (pθ - θ/2) <= apothem
   -- Due to floating-point roundoff, angleTwixt can sometimes end up trying to
   -- take the arccosine of a number greater than 1, resulting in a NaN.  The
   -- isInfinite check is just for completeness.  Try to figure out a better
   -- way to deal with this problem.
  return (ð, n', s)

-- Lights: --------------------------------------------------------------------

 lights :: Scene a -> [(RTColor a, Vec3 a)]
 lights sc = [(color m, c) | (_, m) <- sc, Just c <- [lightCenter m]]

 isLight :: Material a -> Bool
 isLight (Material {lightCenter = Just _}) = True
 isLight _ = False

 mkLight :: RealFloat a => Vec3 a -> a -> RTColor a -> Object a
 mkLight loc rad colour = (sphere loc rad, Material colour 0 0 0 0 (Just loc))

-- RTColor: -------------------------------------------------------------------

 newtype RTColor a = RTColor (a, a, a) deriving (Eq, Ord, Read, Show)
  -- constructor not for export

 instance (RealFrac a) => Color (RTColor a) where
  black = RTColor (0, 0, 0)
  white = RTColor (1, 1, 1)
  getRGBA (RTColor (r, g, b)) = (unrt r, unrt g, unrt b, 255)
   where unrt = round . (* 255) . max 0 . min 1

 instance (RealFrac a) => GreyColor (RTColor a) where
  fromGrey x = RTColor (tort x, tort x, tort x)
   where tort = (/ 255) . fromIntegral

 instance (RealFrac a) => RealColor (RTColor a) where
  fromRGB r g b = RTColor (tort r, tort g, tort b)
   where tort = (/ 255) . fromIntegral

 rtcolor :: RealFrac a => a -> a -> a -> RTColor a
 rtcolor r g b = RTColor (f r, f g, f b) where f = max 0 . min 1

 -- Should these three be exported?

 infixl 7 %%, *%
 infixl 6 %+

 (%+) :: Num a => RTColor a -> RTColor a -> RTColor a  -- add RTColors
 RTColor (r, g, b) %+ RTColor (x, y, z) = RTColor (r+x, g+y, b+z)

 (%%) :: Num a => RTColor a -> RTColor a -> RTColor a  -- multiply RTColors
 RTColor (r, g, b) %% RTColor (x, y, z) = RTColor (r*x, g*y, b*z)

 (*%) :: Num a => a -> RTColor a -> RTColor a  -- scale an RTColor
 c *% RTColor (r, g, b) = RTColor (c*r, c*g, c*b)

-- Unexported functions: ------------------------------------------------------

 epsilon :: Fractional a => a
 epsilon = 0.0000001

 minST :: Ord a => (a -> Bool) -> [a] -> Maybe a  -- "minimum such that"
 minST p xs = case filter p xs of [] -> Nothing; ys -> Just (minimum ys)

 minST2 :: Ord a => (a -> Bool) -> (a, a) -> Maybe a
 minST2 p (a, b) = minST p [a, b]

 wrapUp :: Floating f => f -> Vec3 f -> Bool -> Maybe (f, Vec3 f, Bool)
 --wrapUp ð n ins = Just (ð, scale ((-1) ^ fromEnum ins) (normalize n), ins)
 wrapUp ð n ins = Just (ð, (if ins then scale (-1) else id) (normalize n), ins)

 -- |Tests whether a given point is inside or on an edge of a polygon, assuming
 -- that the given vertices all lie in the plane normal to the first vector and
 -- that each vertex is adjacent to its neighbors in the list, cycling around
 -- at the ends
 ingon :: RealFloat a => Vec3 a -> [Vec3 a] -> Vec3 a -> Bool
 -- Should this be merged into 'polygon'?
 ingon _ [] _ = False
 ingon n vs u = case head prods of (0, d) -> 0 <= d && d <= 1
				   (s, _) -> ingon' s (tail prods)
  where prods = [(signum $ cross a b <.> n, projFactor b a)
		 | (x,y) <- pairNext' vs, let a = x <-> y, let b = u <-> y]
	ingon' s ((0,d):_)  = 0 <= d && d <= 1
	ingon' s ((t,_):xs) = s == t && ingon' s xs
	ingon' _ [] = True
