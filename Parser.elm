module Parser exposing (..)

import Combine exposing (..)
import Combine.Num


type Node
    = Element String
    | FunctionCall Node (List Node) -- function, arguments


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


atom : Parser s Node
atom =
    Combine.choice
        [ element
        , Combine.parens <| Combine.lazy <| \() -> expression
        ]


functionCall : Parser s Node
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


expression : Parser s Node
expression =
    Combine.lazy <|
        \() ->
            Combine.choice
                [ atom <* Combine.end
                , functionCall
                ]



{-
   factor : Parser s Int
   factor =
       whitespace *> (Combine.parens expression <|> Combine.Num.int) <* whitespace


   expression : Parser s Int
   expression =
       Combine.lazy <| \() -> Combine.chainl addop term
-}


parse code =
    Combine.parse (expression <* Combine.end) code
