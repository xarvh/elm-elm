module Parser exposing (..)

import Combine
import Combine.Num


type Node
    = Element String
    | FunctionCall Node (List Node) -- function, arguments


type alias P s =
    Combine.Parser s Node


test =
    FunctionCall
        (Element "+")
        [ Element "2"
        , FunctionCall
            (Element "*")
            [ Element "4"
            , Element "2"
            ]
        ]



-- Helpers


mustEnd : P s -> P s
mustEnd p =
    p
        |> Combine.map always
        |> Combine.andMap Combine.end



-- Elements Parsers


integer =
    Combine.Num.int
        |> Combine.map (toString >> Element)


operator =
    Combine.regex "[~!=@#$%^&*-+|<>]+"
        |> Combine.map Element


symbol =
    Combine.regex "[a-z][a-zA-Z0-9]*"
        |> Combine.map Element


element : P s
element =
    Combine.choice
        [ integer
        , Combine.parens operator
        , symbol
        ]



-- Higher order constructs


list : P s
list =
    expression
        |> Combine.sepBy (Combine.string ",")
        |> Combine.between (Combine.string "[") (Combine.string "]")
        |> Combine.map (\l -> FunctionCall (Element "[]") l)


tuple : P s
tuple =
    expression
        |> Combine.sepBy (Combine.string ",")
        |> Combine.between (Combine.string "(") (Combine.string ")")
        |> Combine.map (\l -> FunctionCall (Element "()") l)


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
        listToParser list =
            case list of
                [] ->
                    Combine.fail "nope"

                element :: [] ->
                    Combine.succeed element

                function :: arguments ->
                    Combine.succeed <| FunctionCall function arguments
    in
        Combine.sepBy1 Combine.whitespace atom
            |> Combine.andThen listToParser


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
