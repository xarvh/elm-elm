module Compiler exposing (..)



import Combine
import Combine.Num


-- Abstract syntax tree
type Node
    = Node String (List Node)





constant : Combine.Parser Node
constant =
    Combine.Num.float
    |> Combine.map (\f -> Node (toString f) [])



operator : Combine.Parser String
operator =
    Combine.regex "[~!@#$%^&*-+/?<>|=]+"





opSequence : Combine.Parser (Node -> Node -> Node)
opSequence =
    let
        opMap op left right =
            Node op [left, right]
    in
        Combine.map opMap operator




expression : Combine.Parser Node
expression =
    Combine.choice
        [ Combine.parens expression
        , Combine.chainl expression opSequence
        , constant
        ]






compile : String -> Result (List String) Node
compile input =
    fst <| Combine.parse expression input


