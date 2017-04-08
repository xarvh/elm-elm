module Parser exposing (..)

import Combine exposing (..)
import Combine.Num


type Node
    = Element String
    | FunctionCall Node (List Node) -- function, arguments


type alias P s = Combine.Parser s Node




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


integer =
    Combine.Num.int
        |> Combine.map (toString >> Element)


operator =
    Combine.regex "[~!=@#$%^&*-+|<>]+"
        |> Combine.map Element


symbol =
    Combine.regex "[a-z][a-zA-Z0-9]*"
        |> Combine.map Element


element : Parser s Node
element =
    Combine.choice
        [ integer
        , Combine.parens operator
        , symbol
        ]


atom : P s
atom =
    [ element
    , Combine.parens <| Combine.lazy <| \() -> expression
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
        op n1 n2 =
            FunctionCall (Element "*") [ n1, n2 ]

        parseOp =
            Combine.string "*" $> op
    in
        Combine.lazy <|
            \() -> Combine.chainr parseOp previous


expression : P s
expression =
    Combine.lazy <|
        \() ->
          let
              prev =
                Combine.choice
                    [ atom <* Combine.end
                    , functionCall
                    ]
          in
              Combine.choice
                [ prev <* Combine.end
                , op0 prev
                ]


parse code =
    Combine.parse (expression <* Combine.end) code
