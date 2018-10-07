{-# LANGUAGE OverloadedStrings #-}

module Tuura.Fantasi.HubRewrite where

import Pangraph

import Data.ByteString.Char8 (pack)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS

import Data.List(sortBy)

import Algebra.Graph 
import Algebra.Graph.ToGraph hiding(vertexList, edgeList)
import qualified Algebra.Graph.ToGraph as Alga

import qualified Data.Set as Set

import Data.Set(toList)

-- Given a hub vertex, and four new vertices, the function replaces the hub with the vertices.
splitHub :: Ord a => a -> (a, a, a, a) -> Graph a -> Graph a
splitHub x (x00, x01, x10, x11) g = overlays
    [ removeVertex x g 
    , biclique ai [x00, x01]
    , biclique bi [x10, x11]
    , biclique [x00, x10] ao
    , biclique [x01, x11] bo ]
  where
    breakInHalf as =    let i = length as `div` 2
                        in (take i as, drop i as) 
    (ai, bi) = breakInHalf . toList $ preSet x g
    (ao, bo) = breakInHalf . toList $ postSet x g

-- Useful alias.
type Endpoints = (VertexID, VertexID)

-- Returns the largest degree node from a Pangraph.
-- Uses FGL patrica tree graph.
largestDegreeNodes :: Graph VertexID -> [VertexID]
largestDegreeNodes g = let
    -- Get incoming Vertices of a Vertex.
    sizePreSet = Set.size . \v -> preSet v g
    -- Compare using Int instance.
    ordVerticesBy :: VertexID -> VertexID -> Ordering
    ordVerticesBy a b = sizePreSet a `compare` sizePreSet b
    -- Sort the list largest to smallest using Flip
    in sortBy (flip ordVerticesBy) . Alga.vertexList $ g

-- Takes the a VertexID and generates its aliases based on a NxN matrix.
generateAliasVertexIDs :: Int -> VertexID -> [VertexID]
generateAliasVertexIDs n bs = map (\i -> bs `BS.append` ps i) (generateMatrixCords n)
    where
        ps :: Show a => a -> ByteString
        ps = pack . show
        -- Generate the possible ij of an NxN matrix. Uses Matrix indexing!
        generateMatrixCords :: Int -> [(Int, Int)]
        generateMatrixCords 0 = []
        generateMatrixCords n' = concatMap (\l -> zip (repeat l) [1..n']) [1..n']

aliasHub :: Pangraph -> Maybe Pangraph
aliasHub p = let
    gOriginal = Alga.toGraph p
    vID:_ = largestDegreeNodes gOriginal
    aliasIDs = generateAliasVertexIDs 2 vID
    -- Is there a better way?
    aliasesAsTuples = (aliasIDs !! 0, aliasIDs !! 1, aliasIDs !! 2, aliasIDs !! 3)
    gAliased = splitHub vID aliasesAsTuples gOriginal
    in makePangraph (map makeVertex' . Alga.vertexList $ gAliased) (map makeEdge' . Alga.edgeList $ gAliased)

makeVertex' :: VertexID -> Vertex
makeVertex' vID = makeVertex vID [("id", vID)]

makeEdge' :: Endpoints -> Edge
makeEdge' e = makeEdge e [("source", fst e), ("target", snd e)]