module Reflex.Monad.Time 
  ( MonadTime (..)

  , observeChanges
  , delay_
  
  , pushFor
  
  , animate
  , animateClip
  , animateOn

  
  , play
  , playClip
  , playClamp
  , playOn
    
  , match
  , matchBy
 
  
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
  
  integrate :: (VectorSpace v, Scalar v ~ time) => v -> Behavior t v -> m (Behavior t v)
  observe :: Behavior t a -> m (Event t a)
  
  getTime :: m (Behavior t time)
  
  after :: time -> m (Event t ())
  delay :: Event t (a, time) ->  m (Event t (NonEmpty a))
  

pushFor ::  Reflex t => Event t a -> (a -> PushM t b) -> Event t b
pushFor = flip pushAlways



delay_ :: (MonadTime t time m) => Event t time ->  m (Event t ())
delay_ e = void <$> delay (((), ) <$>  e)
    

-- | Sample a Clip during it's period, outside it's period return Nothing
animateClip :: (Reflex t, RealFrac time) => Clip time a -> Behavior t time -> Behavior t (Maybe a)
animateClip clip = animate $ toMaybe clip

animate :: (Reflex t, RealFrac time) => Animation time a -> Behavior t time -> Behavior t a
animate anim time = sampleAt anim <$> time 

   
   
sampleOn :: (Reflex t, RealFrac time) => Event t (time -> a) -> Behavior t time -> Event t (Behavior t a)
sampleOn e t = attachWith startAt t e where
  startAt start f = f . subtract start <$> t
  


   
animateOn :: (Reflex t, RealFrac time) => Event t (Animation time a) -> Behavior t time -> Event t (Behavior t a)
animateOn e = sampleOn (sampleAt <$> e)


fromNow ::  MonadTime t time m => m (Behavior t time)
fromNow = do
  time  <- getTime
  start <- sample time
  return (subtract start <$> time)


playClip :: MonadTime t time m =>  Clip time a ->  m (Behavior t (Maybe a), Event t ())
playClip clip = do
  (b, done) <- playClamp clip
  b' <- switcher (Just <$> b) (constant Nothing <$ done)
  return (b', done)

  
playClamp :: MonadTime t time m =>  Clip time a ->  m (Behavior t a, Event t ())
playClamp clip = do
  b <- play (clamped clip)
  done <- after (period clip)
  return (b, done)  
  
  
  
play :: MonadTime t time m =>  Animation time a ->  m (Behavior t a)
play anim = do
  time <- fromNow
  return (sampleAt anim <$> time)  
  

  

playOn :: MonadTime t time m => Event t (Clip time a) ->  m (Behavior t (Maybe a), Event t ())
playOn e = do
  time <- getTime
  done <- delay_ (period <$> e)
  
  b <- hold (constant Nothing) $ 
      leftmost [constant Nothing <$ done, fmap Just <$> animateOn (clamped <$> e) time]
  return (join b, done)
  
  
observeChanges :: (Eq a, MonadTime t time m) => Behavior t a -> m (Event t a)
observeChanges b = do
  initial <- sample b
  d <- holdDyn initial =<< observe b
  return (updated $ nubDyn d)  



match :: (Reflex t, Eq a) => a -> Event t a -> Event t ()
match a = matchBy (== a)

matchBy :: (Reflex t) => (a -> Bool) -> Event t a -> Event t ()
matchBy f = void . ffilter f  



  
  