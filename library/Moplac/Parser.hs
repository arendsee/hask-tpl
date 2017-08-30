module Parser
(
  L2_parse
) where

L1_parse :: String -> L1
L1_parse = erase . L2_parse

L2_parse :: String -> L2
L2_parse = undefined
