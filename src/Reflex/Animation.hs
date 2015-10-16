{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Animation
  ( Animation (..)
    
  , stretched
  , delayed

  , Clip (..)
  , sampleClip
  , toMaybe
  , stretchTo

  , section
  , clamped

  , repeat
  , replicate

  , cropEnd
  , cropStart
  , crop

  , linear
  , linearIn
  , linearOut
  , piecewise

  , keyframes
  
  )
  
  where

import Control.Applicative

import Data.Bifunctor
import Data.Profunctor
import Data.Semigroup

import Data.VectorSpace

import Data.List.NonEmpty (NonEmpty(..))

import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Prelude hiding (repeat, replicate)


-- | Infinite animations  'time -> a'
newtype Animation time a = Animation { sampleAt :: time -> a } 
          deriving (Functor, Applicative, Monad, Profunctor) 
                    
          

stretched :: (Num time) => time -> Animation time a -> Animation time a
stretched factor = lmap (* factor)
  
delayed :: (Num time) => time -> Animation time a -> Animation time a
delayed t = lmap (subtract t)


-- | Infinite animations, Animation with a period  
data Clip time a = Clip { clipAnim :: Animation time a, period :: time }

instance Functor (Clip time) where
  fmap f (Clip anim p) = Clip (f <$> anim) p
  
  
  
instance (Num time, Ord time) => Semigroup (Clip time a) where
  clip <> clip'           = piecewise [clip, clip']
  sconcat (clip :| clips) = piecewise (clip : clips)

-- | Constructor for clips to simplify creation
clip :: (time -> a) -> time -> Clip time a
clip anim = Clip (Animation anim) 

apply :: Clip time (a -> b) -> Animation time a -> Clip time b
apply (Clip anim p) a = Clip (anim <*> a) p




-- | Take a section of an infinite animation as a Clip
section :: (Ord time, Num time) => (time, time) -> Animation time a -> Clip time a
section (s, e) a = Clip (lmap (+s) a) (s - e)


-- | Sample from a clip, returning Nothing outside the domain
sampleClip :: (Ord time, Num time) => Clip time a -> time -> Maybe a
sampleClip clip t | t >= 0 && t <= period clip = Just $ sampleAt (clipAnim clip) t 
                | otherwise    = Nothing
         
-- | Turn a clip into an infinite Animation by using Maybe
toMaybe :: (Ord time, Num time) => Clip time a -> Animation time (Maybe a)
toMaybe clip = Animation (sampleClip clip)

  
-- | Make an infinite animation by clamping time to lie within the period
clamped ::  (Ord time, Num time) => Clip time a -> Animation time a
clamped (Clip anim p) = lmap (clamp (0, p)) anim


-- | Make an infinite animation by repeating the clip 
repeat :: (RealFrac time) => Clip time a -> Animation time a
repeat (Clip anim p) = lmap (`fmod` p) anim
  
  
-- | Repeat a clip a fixed number of times to make a new one  
replicate :: (RealFrac time) => Int -> Clip time a -> Clip time a 
replicate n (Clip anim p) = Clip (lmap time anim) (fromIntegral n * p) where
  time t | t < 0                      = 0.0
         | t >= fromIntegral n * p    = p
         | otherwise     = t `fmod` p


-- | Stretch a clip to a specific size by scaling time
stretchTo :: (RealFrac time)  => time -> Clip time a -> Clip time a
stretchTo p clip = Clip (lmap (* factor) (clipAnim clip)) p
  where factor = period clip / p 



-- | Shorten a clip to a certain period by cropping the end
cropEnd :: (Ord time, Num time) => time -> Clip time a -> Clip time a
cropEnd p' (Clip anim p) = Clip anim (clamp (0, p) p')


-- | Shorten a clip by cropping the start
cropStart :: (Ord time, Num time) => time -> Clip time a -> Clip time a
cropStart s (Clip anim p) = Clip (lmap (+ s') anim) (p - s')
  where s' = clamp (0, p) s

-- | Crop the clip to a range
crop ::  (Ord time, Num time) => (time, time) -> Clip time a -> Clip time a
crop (s, e) = cropStart s . cropEnd e 

-- | Crop the clip to half the period
half :: (RealFrac time) => Clip time a -> Clip time a
half clip = cropStart (0.5 * period clip) clip



type Interpolater time a = time -> (a, a) ->  Clip time a

linear :: (VectorSpace v, RealFrac (Scalar v)) => Interpolater (Scalar v) v
linear p (s, e) = clip (\t -> lerp s e (t / p)) p
 



intervalsWith ::  (RealFrac time) => Interpolater time a -> a -> [(time, a)] -> [Clip time a]
intervalsWith interp start []     = [clip (const start) 0]
intervalsWith interp start frames = zipWith toInterval ((0, start) : frames) frames
  where toInterval (_, k) (p, k') = interp p (k, k') 
  
keyframesWith ::  (RealFrac time) => Interpolater time a -> a -> [(time, a)] -> Clip time a
keyframesWith interp start frames  =  piecewise $ intervalsWith interp start frames


keyframes :: (VectorSpace v, RealFrac (Scalar v)) =>  v -> [(Scalar v, v)] -> Clip (Scalar v) v
keyframes = keyframesWith linear
 
sampleInterval ::  (Ord time, Num time) => Animation time a -> Map time (Animation time a) -> time -> a
sampleInterval start m t = sampleAt anim0 (t - t0) where
  (t0, anim0) = fromMaybe (0, start) (Map.lookupLT t m)
  
  
piecewise :: (Ord time, Num time) => [Clip time a] -> Clip time a
piecewise []    = error "piecewise: empty list"
piecewise [a]   = a
piecewise clips = clip (sampleInterval start m) (last times) where
  m = Map.fromList (zip times (clipAnim <$> clips))
  times = scanl (+) 0 (period <$> clips)
  start = clipAnim $ head clips
 
 
-- | Predefined clips based on special functions for building up animations
linearIn ::  (RealFrac time) => time -> Clip time time
linearIn p | p <= 0.0  = error "linearIn: time must be >= 0"
           | otherwise = clip (/ p) p


linearOut ::  (RealFrac time) => time -> Clip time time
linearOut p | p <= 0    = error "linearOut: time must be >= 0"
            | otherwise = clip (\t -> 1.0 - t / p) p 

sine :: (RealFrac time, Floating time) => time -> Clip time time
sine p = stretchTo p (clip sin pi)

cosine :: (RealFrac time, Floating time) => time -> Clip time time
cosine p = stretchTo p (clip cos pi)


-- | Utility functions 
fmod :: RealFrac a => a -> a -> a
fmod x d | x > 0 || frac == 0 =  frac * d
         | otherwise          = (frac + 1) * d
  where frac = snd $ properFraction (x / d)
        
        
fDivMod :: RealFrac a => a -> a -> (a, a)
fDivMod x d | x > 0 || frac == 0 =  (fromIntegral n, frac * d)
            | otherwise          =  (fromIntegral n, (frac + 1) * d)
  where (n, frac) = properFraction (x / d)
        
  
clamp :: Ord a => (a, a) -> a -> a
clamp (lower, upper) a = max lower (min upper a)

  