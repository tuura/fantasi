module Tuura.Fantasi.VHDL.Writer (
Tuura.Fantasi.VHDL.Writer.writeGraph,
Tuura.Fantasi.VHDL.Writer.writeEnvironment
) where

import Tuura.Fantasi.VHDL.Internal.EnvironmentWriter as VHDL
import Tuura.Fantasi.VHDL.Internal.GraphWriter       as VHDL

import Data.ByteString.Char8(pack)
import Data.ByteString(ByteString)
import Pangraph

-- | A serialiser which will write a graph into a VHDL connections. See more in the Fantasi section.
writeGraph :: Pangraph -> ByteString
writeGraph = pack . VHDL.writeGraph

-- | A serialiser which will write an enviroment VHDL file. See more in the Fantasi section.
writeEnvironment :: Pangraph -> ByteString
writeEnvironment = pack . VHDL.writeEnvironment
