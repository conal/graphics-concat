-- stack build :run
--
-- stack build && stack build :run >& ~/Haskell/graphics-concat/out/o1
--
-- stack build :run --flag concat-run:trace >& ~/Haskell/graphics-concat/out/o1
--
-- stack build && stack build :run-trace >& ~/Haskell/graphics-concat/out/o1
-- 
-- You might also want to use stack's --file-watch flag for automatic recompilation.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}

-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

-- {-# OPTIONS_GHC -fsimpl-tick-factor=250 #-}  -- default 100

module Main where

import Control.Applicative (liftA2)
import GHC.Float (int2Double)   -- TEMP

import ConCat.Misc ((:*),R,sqr,magSqr,Unop,Binop,inNew,inNew2)
import ConCat.Circuit (GenBuses,(:>))
import ConCat.Graphics.GLSL (genHtml,CAnim)
import ConCat.Graphics.Image
import qualified ConCat.RunCircuit as RC
import ConCat.Syntactic (Syn,render)
import ConCat.AltCat (Ok2,ccc,(:**:)(..))

import ConCat.Rebox () -- necessary for reboxing rules to fire

main :: IO ()
main = sequence_
  [ putChar '\n' -- return ()

  -- , genHtml "wobbly-disk" $ ccc $
  --     \ t -> disk' (0.75 + 0.25 * cos t)
  -- , genHtml "diag-plus-im"  $ ccc $ \ t ((x,y) :: R2) -> x + sin t > y
  -- , genHtml "disk-sizing"   $ ccc $ disk . cos
  -- , genHtml "diag-disk-turning" $ ccc $
  --     \ t -> udisk `intersectR` rotate t xPos
  -- , genHtml "checker-rotate" $ ccc $ \ t -> rotate t checker

  -- , genHtml "sqr-sqr-anim" $ ccc $
  --     \ t ((x,y) :: R2) -> sqr (sqr x) > y + sin t
  -- , genHtml "diag-disk-turning-sizing" $ ccc $
  --     \ t -> disk' (cos t) `xorR` rotate t xyPos

  -- , genHtml "foo" $ ccc $ const (disk 1)

  -- , genHtml "slide0" $ ccc $ const slide0

  -- , genHtml "slide1" $ ccc $ slide1

  -- , genHtml "orbits1" $ ccc $ orbits1

  -- , genHtml "foo" $ ccc $
  --     \ t -> uscale ((sin t + 2) / 10) udisk

  -- , genHtml "checker-orbits1" $ ccc $ liftA2 xorR (const (uscale (1/3) checker)) orbits1

  , genHtml "checker-orbits2" $ ccc $
      \ t -> uscale (sin t + 1.05) checker `xorR` orbits1 t

  -- , runCirc "foo" $ ccc $ (uniform scale 2 :: Filter Bool)

  -- , runCirc "bar" $ ccc $ \ () -> 1/2 :: R

  -- , runCirc "bar" $ ccc $ uscale 2 xPos

  -- , runCirc "bar" $ ccc $ scale @Bool

  ]

{--------------------------------------------------------------------
    Testing utilities
--------------------------------------------------------------------}

type GO a b = (GenBuses a, Ok2 (:>) a b)

type EC = Syn :**: (:>)

runSyn :: Syn a b -> IO ()
runSyn syn = putStrLn ('\n' : render syn)

runCirc :: GO a b => String -> (a :> b) -> IO ()
runCirc nm circ = RC.run nm [] circ

runSynCirc :: GO a b => String -> EC a b -> IO ()
runSynCirc nm (syn :**: circ) = runSyn syn >> runCirc nm circ

runCircHtml :: String -> CAnim -> IO ()
runCircHtml nm circ = runCirc nm circ >> genHtml nm circ

-- TODO: Fix runCircHtml to construct the graph once instead of twice.

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

later :: Num t => t -> Unop (t -> a)
later dt f = f . subtract dt

orbits1 :: R -> Region
orbits1 z = translate (cos theta,0) d `xorR` translate (0,sin theta) d
 where
   d = disk (1/2)
   theta = z * 5

-- orbits = liftA2 xorR slide1 (later (pi/2) slide1)

slide0 :: Region
-- slide0 = udisk
-- slide0 = disk' 1
slide0 = uniform scale 1 udisk

slide1 :: R -> Region
slide1 theta = translate (cos theta, 0) (disk (1/2))
