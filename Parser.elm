module Parser exposing (..)

import Combine exposing (..)
import Combine.Num


type Node
    = Element String
    | FunctionCall Node (List Node)



-- function, arguments


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
    Combine.regex "[~!@#$%^&*-+|<>]+"
      |> Combine.map Element


symbol =
    Combine.regex "[a-z][a-zA-Z0-9]*"
      |> Combine.map Element


element : Parser s Node
element =
    Combine.choice
        [ integer
        , operator
        , symbol
        ]




{-
factor : Parser s Int
factor =
    whitespace *> (Combine.parens expression <|> Combine.Num.int) <* whitespace


expression : Parser s Int
expression =
    Combine.lazy <| \() -> Combine.chainl addop term
-}


parse : String -> Result String Node
parse code =
    case Combine.parse (element <* Combine.end) code of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, stream, ms ) ->
            Err ("parse error: " ++ toString ms ++ ", " ++ toString stream)
