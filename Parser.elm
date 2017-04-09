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


type Node
    = Node ParseLocation NodeValue (List NodeValue)



-- Helpers


mustEnd : Parser s x -> Parser s x
mustEnd =
    Combine.map always >> Combine.andMap Combine.end


toNodeParser : (ParseLocation -> a -> Node) -> Parser s a -> Parser s Node
toNodeParser valueToNode =
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
        [ integer
        , Combine.parens operator
        , symbol
        ]
        |> Combine.map Literal
        |> toNodeParser (\location element -> Node location element [])



-- Higher order constructs


list : Parser s Node
list =
    expression
        |> Combine.sepBy (Combine.string ",")
        |> Combine.between (Combine.string "[") (Combine.string "]")
        |> toNodeParser (\location array -> Node location (Operator "[]") array)


tuple : Parser s Node
tuple =
    expression
        |> Combine.sepBy (Combine.string ",")
        |> Combine.between (Combine.string "(") (Combine.string ")")
        |> toNodeParser (\location array -> Node location (Operator "()") array)


atom : P s
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


functionCall : P s
functionCall =
    let
        arrayToCall location array =
          case array of
            [] -> Node location (Literal "This is not going to happen") []
            function :: arguments -> Node location function arguments
    in
        Combine.sepBy1 Combine.whitespace atom
            |> toNodeParser arrayToCall


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


expression : P s
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
                    , op0 prev
                    ]


parse code =
    Combine.parse (mustEnd expression) code
