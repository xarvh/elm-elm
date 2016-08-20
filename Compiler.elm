module Compiler exposing (..)


import String

import Combine
import Combine.Infix exposing ((<*))
import Combine.Num



type FragmentType
    = Expression
    | Attribute
    | Symbol
    | Constant
    | Operator



type Node =
    Node
        { consumedText : String
        , value : String
        , fragmentType : FragmentType
        , position : Int
        , children : List Node
        }



extractContext =
    Combine.primitive (\context -> (Ok context, context))


parseNode fragmentType fragmentParser =
    extractContext `Combine.andThen` \oldContext ->
    fragmentParser `Combine.andThen` \(value, children) ->
    extractContext `Combine.andThen` \(newContext) ->
    Combine.succeed <|
        Node
            { consumedText = String.slice oldContext.position newContext.position oldContext.input
            , value = value
            , fragmentType = fragmentType
            , position = oldContext.position
            , children = children
            }






symbolRegex =
    "[a-zA-z][a-zA-Z0-9]*"


symbol : Combine.Parser Node
symbol =
    Combine.regex symbolRegex
    |> Combine.map (\v -> (v, []))
    |> parseNode Symbol


attribute =
    Combine.regex ("[.]" ++ symbolRegex)
    |> Combine.map (\v -> (v, []))
    |> parseNode Attribute


canHaveTrailingAttributes parser =
    let
        makeA node attributes =
            case attributes of
                Node at :: ats -> makeA (Node { at | children = [node] }) ats
                [] -> node

    in
        parser `Combine.andThen` \node ->
        Combine.many attribute `Combine.andThen` \parsedAttributes ->
        Combine.succeed <|
            makeA node parsedAttributes



constant : Combine.Parser Node
constant =
    Combine.Num.int
    |> Combine.map (\v -> (toString v, [])) --Fragment "number" << toString)
    |> parseNode Constant


operator : Combine.Parser Node
operator =
    Combine.regex "[~!@#$%^&*-+/?<>|=]+"
    |> Combine.map (\v -> (v, [])) --Fragment "op")
    |> parseNode Operator


fragment =
    Combine.rec <| \() ->
    Combine.choice
        [ Combine.parens expression |> canHaveTrailingAttributes -- TODO or tuple!!
        , symbol |> canHaveTrailingAttributes
        , attribute
--         , bracketedStuff
--         , bracedStuff |> canHaveTrailingAttributes
        , operator
        , constant
        ]


whitespace =
    Combine.regex "[ \t\n\r]*"


expression : Combine.Parser Node
expression =
    -- Need to defer instantiation: https://github.com/Bogdanp/elm-combine/issues/7#issuecomment-177468446
    Combine.rec <| \() ->
        Combine.sepBy1 whitespace fragment
        |> Combine.map (\n -> ("()", n))
        |> parseNode Expression








compile : String -> Result (List String) Node
compile input =
    let
        (result, context) =
            Combine.parse (expression <* whitespace <* Combine.end) input
    in
        result

