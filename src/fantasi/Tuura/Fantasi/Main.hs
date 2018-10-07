module Tuura.Fantasi.Main (main) where

import Pangraph
import Tuura.Fantasi.Options
import Tuura.Fantasi.HubRewrite     (aliasHub)
import qualified Pangraph.GraphML.Parser  as P
import qualified Tuura.Fantasi.VHDL.Writer     as VHDL
import Data.ByteString  (readFile, writeFile, ByteString)
import Prelude hiding   (readFile, writeFile)
import Data.Maybe       (fromMaybe)
import Control.Monad(when)

main :: IO ()
main = do
    -- get arguments
    options <- getOptions
    let graphMLPath           = optGraphML options
        graphVHDLPath         = optGraphName options
        simulationEnvVhdlPath = optSimName options
        runAlias              = optAliasHub options

    when runAlias (print "Running Alias")

    -- read and parse graph, handling error with a message.
    pangraph <- (`getPangraph` options) <$> readFile graphMLPath
    
    let graphVHDL   = VHDL.writeGraph pangraph
    let simEnvVHDL  = VHDL.writeEnvironment pangraph

    -- output vhdl graph
    writeFile graphVHDLPath graphVHDL
    -- output vhdl simulation environment
    writeFile simulationEnvVhdlPath simEnvVHDL

getPangraph :: ByteString -> Options -> Pangraph
getPangraph bs options = let
    applyAlias = if optAliasHub options
                then aliasHub
                else Just 
    maybePangraph = P.parse bs >>= applyAlias
    in fromMaybe (error errMsg) maybePangraph 
    where errMsg = "Pangraph is nothing! Does the graph construct? Further if \
        \ -a is enabled, does the graph have vertices and edges?"
        