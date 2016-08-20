module Compiler exposing (..)



import Combine
import Combine.Num


-- Abstract syntax tree
type Node
    = Expression (List Node)
    | Attribute String Node
    | Fragment String String



symbolRegex =
    "[a-zA-z][a-zA-Z0-9]*"

symbol : Combine.Parser Node
symbol =
    Combine.regex symbolRegex
    |> Combine.map (Fragment "symbol")


withAttribute p =
    let
        parseAttribute node =
            Combine.regex ("[.]" ++ symbolRegex)
            |> Combine.map (\attribute -> Attribute attribute node)
    in
        Combine.or (p `Combine.andThen` parseAttribute) p



constant : Combine.Parser Node
constant =
    Combine.Num.int
    |> Combine.map (Fragment "number" << toString)


operator : Combine.Parser Node
operator =
    Combine.regex "[~!@#$%^&*-+/?<>|=]+"
    |> Combine.map (Fragment "op")


fragment =
    Combine.choice
        [ Combine.parens expression |> withAttribute
        , symbol |> withAttribute
        , operator
        , constant
        ]


whitespace =
    Combine.regex "[ \t]*"


expression : Combine.Parser Node
expression =
    -- Need to defer instantiation: https://github.com/Bogdanp/elm-combine/issues/7#issuecomment-177468446
    Combine.rec <| \() ->
        Combine.sepBy1 whitespace fragment
        |> Combine.map Expression








compile : String -> Result (List String) Node
compile input =
    let
        parseEnd = 0


    in
        fst <| Combine.parse expression input --`Combine.andThen` (\


