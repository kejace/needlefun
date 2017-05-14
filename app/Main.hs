{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -W #-}

module Main where

import Prelude hiding ((.), id)
import Control.Category
import Data.Tuple
import Data.Profunctor
import Lib
import Control.Arrow
import Control.Arrow.Needle
--import Text.Uniqhash hiding (main)
--import Data.Machine.MealyM hiding (upgrade)
--import Text.UniqhashMachines hiding (main)
import GHC.Tuple
import System.IO
import Data.Machine
import System.IO.Machine
import Control.Varying.Core hiding (accumulate)
import Control.Auto

newtype Circuit s t a b = C { runC :: (a, s) -> (b, t) }

instance Category (Circuit s s) where
    id = C id
    C f . C g = C (f . g)

instance Arrow (Circuit s s) where
    arr f = C $ \(a, s) -> (f a, s)
    first (C g) = C $ \((a, x), s) -> let (b, t) = g (a, s)
                                      in  ((b, x), t)

mul :: Circuit s s (Int, Int) Int
mul = C $ \((x, y), s) -> (x*y, s)

neg :: Circuit s s Int Int
neg = C $ \(x, s) -> (-x, s)

store :: Circuit Int Int Int ()
store = C $ \(x, _) -> ((), x)

load :: Circuit Int Int () Int
load = C $ \((), s) -> (s, s)

accumulate :: Circuit Int Int Int Int
accumulate = C $ \(a, s) -> (a, s+a)

type Lens s t a b = forall p. Strong p => p a b -> p s t

_1 :: Lens (a, x) (b, x) a b
_1 = first'

_2 :: Lens (x, a) (x, b) a b
_2 = dimap swap swap . first'

newtype Flipped p s t a b = F { unF :: p a b s t }

instance Profunctor (Flipped Circuit a b) where
    lmap f (F (C g)) = F $ C $ \(a, s) -> g (a, f s)
    rmap f (F (C g)) = F $ C $ \(a, s) -> let (b, t) = g (a, s)
                                          in  (b, f t)

instance Strong (Flipped Circuit a b) where
    first' (F (C g)) = F $ C $ \(a, (s, x)) -> let (b, t) = g (a, s)
                                               in  (b, (t, x))

data CPU = CPU { _x :: Int, _y :: Int, _z :: Int, _t :: Int } deriving Show

type ExplodedCPU = (Int, (Int, (Int, Int)))

explode :: CPU -> ExplodedCPU
explode (CPU u v w t) = (u, (v, (w, t)))

implode :: ExplodedCPU -> CPU
implode (u, (v, (w, t))) = CPU u v w t

upgrade :: Profunctor p =>
           (p a a -> p ExplodedCPU ExplodedCPU) ->
           (p a a -> p CPU CPU)
upgrade f = dimap explode implode . f

x, y, z, t :: Flipped Circuit a b Int Int -> Flipped Circuit a b CPU CPU
x = upgrade _1
y = upgrade $ _2 . _1
z = upgrade $ _2 . _2 . _1
t = upgrade $ _2 . _2 . _2

(!) :: p s t a b -> (Flipped p a b s t -> Flipped p a b s' t') ->
       p s' t' a b
x ! f = dimap F unF f x

test :: Circuit CPU CPU () ()
test = proc () -> do
    a  <- load ! x       -< ()
    b  <- load ! y       -< ()
    c  <- mul            -< (a, b)
    d  <- neg            -< c
    e  <- accumulate ! t -< d
    () <- store ! z      -< e
    returnA              -< ()


main :: IO ()
main = print $ runC test ((), CPU 2 30 400 5000)

f :: (Int, Int, Int) -> (Int, Int, Int, Int)
f = proc (a,b,c) -> do
    d <- (+1) -< a
    e <- uncurry div -< (d,c)
    f <- negate -< e
    g <- (*2) -< b
    returnA -< (d,e,f,g)

fNeedle :: (Int, Int, Int) -> (Int, Int, Int, Int)
fNeedle = [nd|
    }=={(+1)}=\==========================>
              \
    }===\     \             /============>
        \     \             /
    }=) \ (==={uncurry div}=/={negate}===>
        \
        \=={(*2)}========================>
|]

--main :: IO ()
--main = putStrLn $ show $ fNeedle (1, 2, 3)

repeatNeedle :: IO ()
repeatNeedle = runKleisli [nd|
    {Kleisli $ const getLine}=={arr ("You said: "++)}=={Kleisli putStrLn}=>
|] ()

--main :: IO ()
--main = repeatNeedle

-- n :: MealyM IO FilePath (Maybe FilePath)
-- n = [nd|
-- }===\================\
--     \                { uncurry (,) }==\=============\
--     \=={ hashPipe }==/                \             { uncurry retrieve }==>
--                                       \=={ cache }==/
-- |]

-- main :: IO ()
-- main = runT_ $ autoMealyM n

v :: (Int, Int) -> Int
v = proc (a,b) -> do
      ez <- (uncurry (+)) -< (a,b)
      returnA -< ez

vNeedle :: (Int, Int) -> Int
vNeedle = [nd|
  }==\
     \
  }=={uncurry (+)}===>
|]

-- main :: IO ()
-- main = putStrLn $ show $ streamAuto' (arr vNeedle) [(1,1),(2,2)]


