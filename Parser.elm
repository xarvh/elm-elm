module Parser exposing (..)

import Combine exposing (..)
import Combine.Num exposing (int)


type Node
    = FunctionCall { name : String, arity : Int, arguments : List Node }
    | Symbol { name : String }
    | Literal String


test =
    FunctionCall
        { name = "(+)"
        , arity = 2
        , arguments =
            [ Literal "2"
            , FunctionCall
                { name = "(*)"
                , arity = 2
                , arguments =
                    [ Literal "4"
                    , Literal "2"
                    ]
                }
            ]
        }




addop : Parser s (Int -> Int -> Int)
addop =
    Combine.choice
        [ (+) <$ Combine.string "+"
        , (-) <$ Combine.string "-"
        ]


mulop : Parser s (Int -> Int -> Int)
mulop =
    Combine.choice
        [ (*) <$ Combine.string "*"
        ]


term : Parser s Int
term =
    let
        go () =
            chainl mulop factor
    in
        lazy go


factor : Parser s Int
factor =
    whitespace *> (parens expression <|> int) <* whitespace



expression : Parser s Int
expression =
    let
        go () =
            Combine.chainl addop term
    in
        lazy go


parse : String -> Result String Int
parse code =
    case Combine.parse (expression <* Combine.end) code of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, stream, ms ) ->
            Err ("parse error: " ++ toString ms ++ ", " ++ toString stream)
