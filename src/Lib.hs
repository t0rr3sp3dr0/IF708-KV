module Lib
    (   get,
        add,
        ANode (..),
    ) where

import Types

import Data.Char
data ANode = NilNode | Node Value [ANode] deriving Show

emptyAdjacencyList :: [ANode]
emptyAdjacencyList = (take 256 (repeat NilNode))

addToK :: Int -> [Char] -> Value -> [ANode] -> [ANode]
addToK position key value list = (take (position) list) ++ [add key value (head (drop (position) list))] ++ (drop (position + 1) list)

add :: [Char] -> Value -> ANode -> ANode
add "" value NilNode = Node value emptyAdjacencyList
add "" value (Node _ nodeList) = Node value nodeList
add (k:key) value (Node nodeValue nodeList) = Node nodeValue (addToK (ord k) key value nodeList)
add (k:key) value NilNode = Node zero (addToK (ord k) key value emptyAdjacencyList)

get :: [Char] -> ANode -> Value
get _ NilNode = zero
get "" (Node value _) = value
get (hd:tl) (Node _ list) = get tl (head (drop (ord hd) list))
