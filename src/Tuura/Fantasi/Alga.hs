module Tuura.Fantasi.Alga where

import Algebra.Graph
import Algebra.Graph.ToGraph

import Data.Set(toList)

-- Given a hub vertex, and four new vertices, the function replaces the hub with the vertices 
-- reducing the peak connectivity but preserving all distances.
-- Note that our assumption is that in the original graph we have edges going in both direction 
-- between every pair of connected vertices. In the result, however, this is no longer true.
splitHub :: Ord a => a -> (a, a, a, a) -> Graph a -> Graph a
splitHub x (aa, ab, ba, bb) g = overlays
    [ removeVertex x g 
    , biclique a [aa, ab]
    , biclique b [ba, bb]
    , biclique [aa, ba] a
    , biclique [ab, bb] b ]
  where
    neighbours = toList $ preSet x g
    n = length neighbours
    half = n `div` 2
    (a, b) = (take half neighbours, drop half neighbours)