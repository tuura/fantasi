{-# LANGUAGE OverloadedStrings #-}

module Tuura.Fantasi.HubRewrite where

import Pangraph
import qualified Pangraph.FGL as FGL
-- import qualified Pangraph.GraphML.Parser as GraphML

import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGLTree

import Data.ByteString.Char8 (pack)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS

import Data.List(maximumBy)

-- import Data.Maybe(fromJust)

-- import Data.Set(Set)
-- import qualified Data.Set as Set

-- import Control.Applicative((<*>))

import Tuura.Fantasi.Alga

import qualified Algebra.Graph.ToGraph as Alga

-- | Used for a 'Set' implementation for 'Eq' and 'Ord' instances on endpoints only.
newtype OrderEdgeByEnds = OrderEdgeByEnds Edge deriving (Show)

-- Eq on endpoints.
instance Eq OrderEdgeByEnds where
    (==) (OrderEdgeByEnds a) (OrderEdgeByEnds b) = edgeEndpoints a == edgeEndpoints b 

-- Ord on endpoints.
instance Ord OrderEdgeByEnds where
    compare (OrderEdgeByEnds a) (OrderEdgeByEnds b) = compare (edgeEndpoints a) (edgeEndpoints b)

-- driveN1 :: IO ()
-- driveN1 = do
--     p <- fromJust . GraphML.parse <$> BS.readFile "./n1.graphml"
--     let largestDeg = largestDegreeNode p
--     let (setA, setB) = identifyEdgesAndSplit p largestDeg
--     let vs = replaceVertex p largestDeg
--     -- endsSet :: (Set Endpoints , Set Endpoints)
--     let endsSet = (Set.fromList setA, Set.fromList setB)
--     let es = replaceEdges p largestDeg endsSet

--     putStrLn "SetA"
--     mapM_ print setA
--     putStrLn "------------"
--     putStrLn "SetB"
--     mapM_ print setB
--     putStrLn "------------"
--     putStrLn "New Vertex Ids"
--     mapM_ print $ generateAliasVertexIDs  2 largestDeg
--     putStrLn "------------"
--     putStrLn "New Vertices"
--     mapM_ print vs
--     putStrLn "------------"
--     putStrLn "New Edge List"
--     mapM_ print es

-- Useful alias.
type Endpoints = (VertexID, VertexID)

-- Returns the largest degree node from a Pangraph.
-- Uses FGL patrica tree graph.
largestDegreeNode :: Pangraph -> VertexID
largestDegreeNode p = let
    -- Build a patricia tree backed FGL Graph instance.
    fglGraph :: Pangraph -> FGLTree.Gr VertexID Int
    fglGraph = uncurry FGL.mkGraph . FGL.convert
    -- Calculate the order of all Vertices.
    -- Return the VertexID of the maximum.
    maxVertexOrder ::  FGLTree.Gr VertexID Int -> (VertexID, Int)
    maxVertexOrder = maximumBy compareBySnd 
        . map (\(a,b) -> (b, FGL.deg (fglGraph p) a)) . FGL.labNodes
        where
            compareBySnd :: (ByteString, Int) -> (ByteString, Int) -> Ordering
            compareBySnd a b = compare (snd a) (snd b)
    -- Combine together returning the VertexID which came from Pangraph
    in (fst . maxVertexOrder . fglGraph) p

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
    hubID = largestDegreeNode p
    aliasIDs = generateAliasVertexIDs 2 hubID
    -- Is there a better way?
    aliasesAsTuples = (aliasIDs !! 0, aliasIDs !! 1, aliasIDs !! 2, aliasIDs !! 3)
    g = splitHub hubID aliasesAsTuples (Alga.toGraph p)

    in makePangraph (map makeVertex' . Alga.vertexList $ g) (map makeEdge' . Alga.edgeList $ g)  

makeVertex' :: VertexID -> Vertex
makeVertex' vID = makeVertex vID [("id", vID)]

makeEdge' :: Endpoints -> Edge
makeEdge' e = makeEdge e [("source", fst e), ("target", snd e)]