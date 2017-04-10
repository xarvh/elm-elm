module Parser exposing (..)

import Combine exposing (Parser, ParseLocation, string)
import Combine.Num


type
    NodeValue
    -- "abc", 1, -3, 4.5, ...
    = Literal String
      -- +, -, ==>, ...
    | Operator String
      -- a, model, update, beginnerProgram, ...
    | Symbol String
      -- nodes
    | FunctionCall Node (List Node)


type Node
    = Node ParseLocation NodeValue



-- Helpers


mustEnd : Parser s x -> Parser s x
mustEnd =
    Combine.map always >> Combine.andMap Combine.end


toNodeParser : (ParseLocation -> a -> Node) -> Parser s a -> Parser s Node
toNodeParser valueToNode parser =
    Combine.withLocation <| \location -> Combine.map (valueToNode location) parser



-- Elements Parsers


integer : Parser s String
integer =
    Combine.Num.int |> Combine.map toString


operator : Parser s String
operator =
    Combine.regex "[~!=@#$%^&*-+|<>]+"


symbol : Parser s String
symbol =
    Combine.regex "[a-z][a-zA-Z0-9]*"


element : Parser s Node
element =
    Combine.choice
        [ integer |> Combine.map Literal
        , Combine.parens operator |> Combine.map Symbol
        , symbol |> Combine.map Symbol
        ]
        |> toNodeParser (\location nodeValue -> Node location nodeValue)



-- Higher order constructs


list : Parser s Node
list =
    Combine.lazy <|
        \() ->
            expression
                |> Combine.sepBy (Combine.string ",")
                |> Combine.between (Combine.string "[") (Combine.string "]")
                |> toNodeParser (\location array -> Node location <| FunctionCall (Node location <| Symbol "[]") array)


tuple : Parser s Node
tuple =
    Combine.lazy <|
        \() ->
            expression
                |> Combine.sepBy (Combine.string ",")
                |> Combine.between (Combine.string "(") (Combine.string ")")
                |> toNodeParser (\location array -> Node location <| FunctionCall (Node location <| Symbol "()") array)


atom : Parser s Node
atom =
    Combine.lazy <|
        \() ->
            [ element
            , list
            , tuple
            , Combine.parens expression
            ]
                |> Combine.choice
                |> Combine.between Combine.whitespace Combine.whitespace


functionCall : Parser s Node
functionCall =
    let
        arrayToCall location array =
            case array of
                [] ->
                    Node location <| Literal "This is not going to happen"

                function :: arguments ->
                    Node location <| FunctionCall function arguments
    in
        Combine.lazy <|
            \() ->
                Combine.sepBy1 Combine.whitespace atom
                    |> toNodeParser arrayToCall



{-
   op0 : P s -> P s
   op0 previous =
       let
           operator string result =
               string
                   |> Combine.string
                   |> Combine.map (always result)

           opNode leftChild rightChild =
               FunctionCall (Element "*") [ leftChild, rightChild ]

           parseOp =
               operator "*" opNode
       in
           Combine.lazy <|
               \() -> Combine.chainl parseOp previous
-}


expression : Parser s Node
expression =
    Combine.lazy <|
        \() ->
            let
                prev =
                    Combine.choice
                        [ mustEnd atom
                        , functionCall
                        ]
            in
                Combine.choice
                    [ mustEnd prev

                    --, op0 prev
                    ]


parse code =
    Combine.parse (mustEnd expression) code
