{-# LANGUAGE OverloadedStrings #-}

module Tuura.Fantasi.HubRewrite where

import Pangraph
import qualified Pangraph.FGL as FGL
import qualified Pangraph.GraphML.Parser as GraphML

import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGLTree

import Data.ByteString.Char8 (pack)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS

import Data.List(maximumBy)

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

driveN1 :: IO ()
driveN1 = do
    p <- fromJust . GraphML.parse <$> BS.readFile "./n1.graphml"
    let largestDeg = largestDegreeNode p
    let (setA, setB) = identifyEdgesAndSplit p largestDeg
    let vs = replaceVertex p largestDeg
    -- endsSet :: (Set Endpoints , Set Endpoints)
    let endsSet = (Set.fromList setA, Set.fromList setB)
    let es = replaceEdges p largestDeg endsSet

    putStrLn "SetA"
    mapM_ print setA
    putStrLn "------------"
    putStrLn "SetB"
    mapM_ print setB
    putStrLn "------------"
    putStrLn "New Vertex Ids"
    mapM_ print $ generateAliasVertexIDs  2 largestDeg
    putStrLn "------------"
    putStrLn "New Vertices"
    mapM_ print vs
    putStrLn "------------"
    putStrLn "New Edge List"
    mapM_ print $ es

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

-- Returns the tuple pair of neighbour sets and also balanced between in/out going. 
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
    splitListInTwo :: [Endpoints] -> ([Endpoints], [Endpoints])
    splitListInTwo myList = splitAt ((length myList + 1) `div` 2) myList
    
    -- Pattern match to tuples.
    (a1,b1) = splitListInTwo $ ends inComing
    (a2,b2) = splitListInTwo $ ends outGoing
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

        
        -- Remove the a Vertex and return remaining vetices + new  aliased vertices
replaceVertex :: Pangraph -> VertexID -> [Vertex]
replaceVertex p vID = let
    -- Keep all but the given VertexID
    remainingVertices :: Pangraph -> [Vertex]
    remainingVertices = filter (\v -> vID /= vertexID v) . vertexList
    -- Generate the aliases.
    newVertices :: VertexID -> [Vertex]
    newVertices =  map makeVertex' . generateAliasVertexIDs 2
        where
            makeVertex' :: VertexID -> Vertex
            makeVertex' vID' = makeVertex vID' [("id", vID')]
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
    combinedSet = Set.map (OrderEdgeByEnds . makeEdge') (setA `Set.union` setB)
    
    -- Make edge in GraphML Style
    makeEdge' :: Endpoints -> Edge
    makeEdge' (a, b) = makeEdge (a,b) [("source", a), ("target", b)]

    -- Take the difference of the sets to remove old edges.
    remainingEdges :: [Edge]
    remainingEdges = map (\(OrderEdgeByEnds a) -> a) . Set.toList $ Set.difference currentSet combinedSet

    -- From the removed edges and create all the aliased edges.
    newEdges :: [Edge]
    newEdges =  map makeEdge' $ concatMap (Set.toList . (`updateSet` setTuple)) $ sumList <*> zip (repeat vID) (generateAliasVertexIDs 2 vID)

    -- The list of combinations.
    sumList :: [(VertexID, VertexID) -> NeighbourCombinations]
    sumList = [AA, AB, BA, BB]

    -- Concatenate and return.
    in remainingEdges ++ newEdges

-- Match on the first arguement and connect the set(s) and rename the ids when they appear.
-- TODO: Find a more beautiful method.
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
    -- Find all edges connecting this Vertex,
    -- split them into input and output balanced groups.
    (vsA, vsB) = identifyEdgesAndSplit p largestDeg
    -- Transform into actual sets.
    endsSet :: (Set Endpoints , Set Endpoints)
    endsSet = (Set.fromList vsA, Set.fromList vsB)
    -- Replace old with new for edges and the Vertex.
    vs :: [Vertex]
    vs = replaceVertex p largestDeg
    es :: [Edge]
    es = replaceEdges p largestDeg endsSet
    -- Build and verify the new Graph!
    -- Possibly check connectivity with a Graph Library?
    in makePangraph vs es
