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

{-# OPTIONS_GHC -ddump-rule-rewrites #-}

module Main where

import GHC.Float (int2Double)   -- TEMP

import ConCat.Misc ((:*),R,sqr,magSqr,Binop,inNew,inNew2)
import ConCat.Circuit (GenBuses,(:>))
import ConCat.Graphics.Image
import qualified ConCat.RunCircuit as RC
import ConCat.GLSL (genHtml,CAnim)
import ConCat.Syntactic (Syn,render)
import ConCat.AltCat (Ok2,ccc,(:**:)(..))

import ConCat.Rebox () -- necessary for reboxing rules to fire

main :: IO ()
main = sequence_
  [ putChar '\n' -- return ()

  -- , runCircGlsl "wobbly-disk" $ ccc $
  --     \ t -> disk' (0.75 + 0.25 * cos t)
  -- , runCircGlsl "diag-plus-im"  $ ccc $ \ t ((x,y) :: R2) -> x + sin t > y
  , runCircGlsl "disk-sizing"   $ ccc $ disk . cos
  -- , runCircGlsl "disk-sizing-p" $ ccc $ disk' . cos
  -- , runCircGlsl "diag-disk-turning" $ ccc $
  --     \ t -> udisk `intersectR` rotate t xPos
  -- , runCircGlsl "sqr-sqr-anim" $ ccc $
  --     \ t ((x,y) :: R2) -> sqr (sqr x) > y + sin t
  -- , runCircGlsl "diag-disk-turning-sizing" $ ccc $
  --     \ t -> disk' (cos t) `xorR` rotate t xyPos
  -- , runCircGlsl "checker-rotate" $ ccc $ \ t -> rotate t checker

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

runCircGlsl :: String -> CAnim -> IO ()
runCircGlsl nm circ = runCirc nm circ >> genHtml nm circ

-- TODO: Fix runCircGlsl to construct the graph once instead of twice.

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}
