{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Online

import Chart hiding (Vector)
import Tower.Prelude
import qualified Control.Foldl as L

main :: IO ()
main = do
    let fake = [0..100] <> replicate 101 100 :: [Double]
    fileSvg "other/av.svg" (300,300) $
        withChart def
        (lines
         [ LineConfig 0.005 (Color 0.88 0.33 0.12 1)
         , LineConfig 0.005 (Color 0.12 0.33 0.83 1)
         , LineConfig 0.002 (Color 0.33 0.33 0.33 1)
         ])
        [ zipWith V2 [0..] (drop 1 $ L.scan (online identity (* 0.9)) fake)
        , zipWith V2 [0..] (drop 1 $ L.scan (online identity (* 0.99)) fake)
        , zipWith V2 [0..] fake
        ]
    fileSvg "other/std.svg" (300,300) $
        withChart def
        (lines
         [ LineConfig 0.005 (Color 0.88 0.33 0.12 1)
         , LineConfig 0.005 (Color 0.12 0.33 0.83 1)
         , LineConfig 0.002 (Color 0.33 0.33 0.33 1)
         ])
        [ zipWith V2 [0..] (drop 1 $ L.scan (std 0.9) fake)
        , zipWith V2 [0..] (drop 1 $ L.scan (std 0.99) fake)
        , zipWith V2 [0..] fake
        ]
