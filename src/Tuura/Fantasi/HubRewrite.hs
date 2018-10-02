{-# LANGUAGE OverloadedStrings #-}

module Tuura.Fantasi.HubRewrite
(aliasHub) where

import Pangraph

import qualified Pangraph.FGL as FGL

import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGLTree

import Data.ByteString.Char8 (pack)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS

import Data.List(maximumBy)
import Data.List.Split(chunksOf)

import Data.Maybe(fromJust)

import Data.Set(Set)
import qualified Data.Set as Set

import Control.Applicative((<*>))

-- | Used for a 'Set' implementation for 'Eq' and 'Ord' instances on endpoints only.
newtype OrderEdgeByEnds = OrderEdgeByEnds Edge deriving (Show)

-- Eq on endpoints.
instance Eq OrderEdgeByEnds where
    (==) (OrderEdgeByEnds a) (OrderEdgeByEnds b) = edgeEndpoints a == edgeEndpoints b 

-- Ord on endpoints.
instance Ord OrderEdgeByEnds where
    compare (OrderEdgeByEnds a) (OrderEdgeByEnds b) = compare (edgeEndpoints a) (edgeEndpoints b)

-- Useful alias.
type Endpoints = (VertexID, VertexID)

-- Used to pattern match on.
data NeighbourCombinations 
    = AA (VertexID, VertexID)
    | AB (VertexID, VertexID)
    | BB (VertexID, VertexID)
    | BA (VertexID, VertexID)

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


-- Returns the tuple pair of neighbours and also balanced between in/out going. 
identifyEdgesAndSplit :: Pangraph -> VertexID 
    ->  ([Endpoints], [Endpoints])
identifyEdgesAndSplit p bs = let
    -- Filter endpoints which do not have this VertexID and are not self loops.
    ends :: (Endpoints -> Bool) -> [Endpoints]
    ends f = filter (\e -> f e && uncurry (/=) e) . map edgeEndpoints . edgeList $ p 
    inComing :: Endpoints -> Bool
    inComing e = (bs == snd e)
    outGoing :: Endpoints -> Bool
    outGoing e = (bs == fst e)
    -- Split the list incoming or outoging by two, leaving the first larger then the last if list is uneven.
    splitListInTo2 :: [Endpoints] -> [[Endpoints]]
    splitListInTo2 ls = chunksOf (length ls `div` 2) ls
    -- Pattern match to tuples.
    [a1,b1] = splitListInTo2 $ ends inComing
    [a2,b2] = splitListInTo2 $ ends outGoing
    -- Merge the two sets so they have almost* equal incoming and outgoing neighbours.
    -- *1 ChunksOf may return unbalanced lists if the length is not a multiple of chunk size.
    in (a1 ++ a2, b1 ++ b2)

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

-- Build an attribute as it would appear from GraphML for a Vertex.
idToAttribute :: VertexID -> [Attribute]
idToAttribute vID = [("id", vID)]

-- Remove the a Vertex and return remaining vetices + new  aliased vertices
replaceVertex :: Pangraph -> VertexID -> [Vertex]
replaceVertex p vID = let
    -- Keep all but the given VertexID
    remainingVertices :: Pangraph -> [Vertex]
    remainingVertices = filter (\v -> vID /= vertexID v) . vertexList
    -- Generate the aliases.
    newVertices :: VertexID -> [Vertex]
    newVertices =  map (f . idToAttribute) . generateAliasVertexIDs 2
        where
            f :: [Attribute] -> Vertex
            f as = makeVertex (fromJust $ lookup "id" as) as
    -- Concatenate and return
    in (newVertices vID ++ remainingVertices p)

-- Take the pangraph remove old edges and add the new ones for the aliases.
replaceEdges :: Pangraph -> VertexID -> (Set Endpoints, Set Endpoints) -> [Edge]
replaceEdges p vID setTuple@(setA, setB) = let
    -- Build the set from the Pangraph
    currentSet :: Set OrderEdgeByEnds
    currentSet = Set.fromList . map OrderEdgeByEnds . edgeList $ p
    -- Build the set from neighbours to be removed.
    combinedSet :: Set OrderEdgeByEnds
    combinedSet = Set.map (OrderEdgeByEnds . (`makeEdge` [])) (setA `Set.union` setB)

    -- Take the difference of the sets to remove old edges.
    remainingEdges :: [Edge]
    remainingEdges = map (\(OrderEdgeByEnds a) -> a) . Set.toList $ Set.difference currentSet combinedSet

    -- From the removed edges and create all the aliased edges.
    newEdges :: [Edge]
    newEdges =  map (`makeEdge` []) $ concatMap (Set.toList . (`updateSet` setTuple)) $ sumList <*> zip (repeat vID) (generateAliasVertexIDs 2 vID)

    -- The list of combinations.
    sumList :: [(VertexID, VertexID) -> NeighbourCombinations]
    sumList = [AA, AB, BA, BB]
    -- Concatenate and return.
    in remainingEdges ++ newEdges

-- Match on the first arguement and connect the set(s) and rename the ids when they appear.
updateSet :: NeighbourCombinations -> (Set Endpoints, Set Endpoints) -> Set Endpoints
updateSet (AA ids) (setA, _   ) = Set.map (`renameEdgeInTuple` ids) setA
updateSet (AB ids) (setA, setB) = Set.map (`renameEdgeInTuple` ids) (setA `Set.union` setB)
updateSet (BA ids) (setA, setB) = Set.map (`renameEdgeInTuple` ids) (setA `Set.union` setB)
updateSet (BB ids) (_   , setB) = Set.map (`renameEdgeInTuple` ids) setB

-- Rename the exactly one endpoint and return. 
renameEdgeInTuple :: Endpoints -> (VertexID, VertexID) -> Endpoints
renameEdgeInTuple e (old, new) = 
    case (fst e == old, snd e == old) of
            (True, True)    -> error $ "Self loop: " ++ show e
            (True, _)       -> (new, snd e)
            (_, True)       -> (fst e, new)
            (False, False)  -> error $ "Not a neighbour: " ++ show e
            
-- | Aliases the largest hub in the Pangraph into two equivalently connected Vertices.
aliasHub :: Pangraph -> Maybe Pangraph
aliasHub p = let
    -- Find the largest degree VertexID
    largestDeg :: VertexID
    largestDeg = largestDegreeNode p
    -- Find all edges connecting this Vertex.
    (setA, setB) = identifyEdgesAndSplit p largestDeg
    -- Transform into actual sets.
    endsSet :: (Set Endpoints , Set Endpoints)
    endsSet = (Set.fromList setA, Set.fromList setB)
    -- Generate the Vertice's aliases.
    vs :: [Vertex]
    vs = replaceVertex p largestDeg
    es :: [Edge]
    es = replaceEdges p largestDeg endsSet
    in makePangraph vs es
