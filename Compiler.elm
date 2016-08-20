module Compiler exposing (..)



import Combine
import Combine.Num


-- Abstract syntax tree
type Node
    = Node String (List Node)





constant : Combine.Parser Node
constant =
    Combine.Num.int
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
    -- Need to defer instantiation: https://github.com/Bogdanp/elm-combine/issues/7#issuecomment-177468446
    Combine.rec <| \() ->
        Combine.chainl (Combine.choice [constant, Combine.parens expression]) opSequence







compile : String -> Result (List String) Node
compile input =
    fst <| Combine.parse expression input


