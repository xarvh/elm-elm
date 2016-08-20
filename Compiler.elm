module Compiler exposing (..)


import String



-- Abstract syntax tree
type Node
    = Node String (List Node)




compile : String -> (Maybe Node, List String)
compile input =
    (Just <| parse input, [])

--     case Parser.parse Nu.digit input of
--         Ok result -> (Just (Value <| toString input), [])
--         Err message -> (Nothing, [message])




allOps = [ "-", "+" ]





parse fragment =
    parseOps allOps fragment


parseOps ops fragment =
    case ops of
        [] ->
            Node fragment []

        op :: xops ->
            case String.split op fragment of
                [single] -> parseOps xops fragment
                operands -> Node op <| List.map parse operands
