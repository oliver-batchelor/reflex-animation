module Reflex.Monad.Time 
  ( MonadTime (..)

  , delay_
   
  , animate
  , animateClip
  , animateOn
  
  
  , play
  , playClip
  , playClamp
  , playOn
    
  , match
  , matchBy
 
  , pushFor
  
  ) where

  
import Reflex.Animation

import Reflex.Monad
import Reflex

import Data.VectorSpace
import Data.Functor
import Control.Applicative

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)


import Control.Monad

 
  
class (MonadReflex t m, RealFrac time) => MonadTime t time m | m -> t time where
  
  -- | A behavior for time, must be up to date 
  -- (i.e. represents current time not previous time)
  getTime :: m (Behavior t time)
  
  -- | Fire an event at or just after a period of time
  after :: time -> m (Event t ())
  
  -- | Delay an event by a period of time, returns a list in case
  -- two delayed events occur within the sampling rate of the framework
  delay :: Event t (a, time) ->  m (Event t (NonEmpty a))
  



-- | Delay a void event stream
delay_ :: (MonadTime t time m) => Event t time ->  m (Event t ())
delay_ e = void <$> delay (((), ) <$>  e)
    

-- | Sample a Clip during it's period, outside it's period return Nothing
animateClip :: (Reflex t, RealFrac time) => Clip time a -> Behavior t time -> Behavior t (Maybe a)
animateClip clip = animate $ toMaybe clip

-- | Animate an infinite animation using framework time
animate :: (Reflex t, RealFrac time) => Animation time a -> Behavior t time -> Behavior t a
animate anim time = sampleAt anim <$> time 

   
-- | Helper for animateOn using the underlying representation (time -> a)
sampleOn :: (Reflex t, RealFrac time) => Event t (time -> a) -> Behavior t time -> Event t (Behavior t a)
sampleOn e t = attachWith startAt t e where
  startAt start f = f . subtract start <$> t
  


  
-- | Create a Behavior from an infinite animation on the occurance of the event   
animateOn :: (Reflex t, RealFrac time) => Event t (Animation time a) -> Behavior t time -> Event t (Behavior t a)
animateOn e = sampleOn (sampleAt <$> e)


-- | Record time offset from the current time
fromNow ::  MonadTime t time m => m (Behavior t time)
fromNow = do
  time  <- getTime
  start <- sample time
  return (subtract start <$> time)


-- | Play an animation clip, giving a Behavior of it's value and an Event firing as it finishes
playClip :: MonadTime t time m =>  Clip time a ->  m (Behavior t (Maybe a), Event t ())
playClip clip = do
  (b, done) <- playClamp clip
  b' <- switcher (Just <$> b) (constant Nothing <$ done)
  return (b', done)


-- | Play an animation clip, except clamp the ends so the Behavior is no longer 'Maybe a'
playClamp :: MonadTime t time m =>  Clip time a ->  m (Behavior t a, Event t ())
playClamp clip = do
  b <- play (clamped clip)
  done <- after (period clip)
  return (b, done)  
  
  
-- | Play an infinite animation starting now
play :: MonadTime t time m =>  Animation time a ->  m (Behavior t a)
play anim = do
  time <- fromNow
  return (sampleAt anim <$> time)  
  


-- | Play an animation clip starting on the occurance of an Event
-- if another play event occurs before the last one has finished, switch to that one instead.
playOn :: MonadTime t time m => Event t (Clip time a) ->  m (Behavior t (Maybe a), Event t ())
playOn e = do
  time <- getTime
  done <- delay_ (period <$> e)
  
  b <- hold (constant Nothing) $ 
      leftmost [constant Nothing <$ done, fmap Just <$> animateOn (clamped <$> e) time]
  return (join b, done)
  



-- | Helper functions using filter with Eq
match :: (Reflex t, Eq a) => a -> Event t a -> Event t ()
match a = matchBy (== a)

matchBy :: (Reflex t) => (a -> Bool) -> Event t a -> Event t ()
matchBy f = void . ffilter f  

-- | Helper for pushAlways
pushFor ::  Reflex t => Event t a -> (a -> PushM t b) -> Event t b
pushFor = flip pushAlways

  
  