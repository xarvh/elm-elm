module Parser exposing (..)

import Combine exposing (Parser, ParseLocation, (*>))
import Combine.Num


-- Structure nmore or less copied from avh4's fantastic elm-format
-- https://github.com/avh4/elm-format/blob/master/parser/src/AST/Expression.hs


type UnaryOperator
    = Negative


type alias LiteralValue =
    String


type alias VariableReference =
    String


type LocatedExpression
    = LocatedExpression ParseLocation Expression


type Expression
    = BinaryExpression LocatedExpression VariableReference LocatedExpression
    | FunctionCall LocatedExpression (List LocatedExpression)
    | ListExpression (List LocatedExpression)
    | LiteralExpression LiteralValue
    | RecordAccessorFunction String
    | RecordAccess LocatedExpression (List LocatedExpression)
    | TupleExpression (List LocatedExpression)
    | UnaryExpression UnaryOperator LocatedExpression
    | Unit
    | VariableExpression VariableReference



-- type LetDeclaration
--   = LetDefinition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments Expr
--   | LetAnnotation (Var.Ref, Comments) (Comments, Type)
--   | LetComment Comment
--| Record
--{ base :: Maybe LowercaseIdentifier
--, fields :: List (LowercaseIdentifier, Expr)
--}



--| Lambda (List Pattern) Expr
--| If LocatedExpression LocatedExpression LocatedExpression
--| Let (List Declaration) LocatedExpression
--| Case LocatedExpression (List (Pattern, Expression))
-- Helpers


mustEnd : Parser s x -> Parser s x
mustEnd =
    Combine.map always >> Combine.andMap Combine.end


withLocation : (a -> Expression) -> Parser s a -> Parser s LocatedExpression
withLocation valueToExpression parser =
    Combine.lazy <|
        \() ->
            Combine.withLocation <|
                \location ->
                    parser
                        |> Combine.map (LocatedExpression location << valueToExpression)


withWhitespace : Parser s a -> Parser s a
withWhitespace =
    Combine.between Combine.whitespace Combine.whitespace


{-| TODO isn't there a nicer way to implement map2?
-}
map2 : (a -> b -> c) -> Parser s a -> Parser s b -> Parser s c
map2 f parserA parserB =
    parserA
        |> Combine.andThen (\a -> Combine.map (f a) parserB)



-- String parsers


operator : Parser s String
operator =
    Combine.regex "[~!=@#$%^&*-+|<>]+"


{-| This covers variable, attribute, module and sub-module names
-}
symbol : Parser s String
symbol =
    Combine.regex "[a-zA-Z][a-zA-Z0-9_]*"


dotAccessorString : Parser s String
dotAccessorString =
    Combine.string "." *> symbol



-- Element expressions


integerLiteral : Parser s Expression
integerLiteral =
    Combine.Num.int
        |> Combine.map toString
        |> Combine.map LiteralExpression


prefixOperator : Parser s Expression
prefixOperator =
    operator
        |> Combine.parens
        |> Combine.map VariableExpression


variable : Parser s Expression
variable =
    symbol
        |> Combine.map VariableExpression


recordAccessorFunction : Parser s Expression
recordAccessorFunction =
    dotAccessorString
        |> Combine.map RecordAccessorFunction


elementExpression : Parser s LocatedExpression
elementExpression =
    Combine.choice
        [ integerLiteral
        , prefixOperator
        , recordAccessorFunction
        , variable
        ]
        |> withLocation identity



-- Higher order constructs


sequence : String -> String -> (List LocatedExpression -> Expression) -> Parser s LocatedExpression
sequence leftDelimiter rightDelimiter listToExpression =
    Combine.lazy <|
        \() ->
            expression
                |> withWhitespace
                |> Combine.sepBy (Combine.string ",")
                |> Combine.between (Combine.string leftDelimiter) (Combine.string rightDelimiter)
                |> withLocation listToExpression


listExpression =
    Combine.lazy <|
        \() ->
            sequence "[" "]" ListExpression


tupleExpression =
    Combine.lazy <|
        \() ->
            sequence "(" ")" TupleExpression


{-| An atom is something that doesn't need precedence rules
-}
atom : Parser s LocatedExpression
atom =
    Combine.lazy <|
        \() ->
            [ elementExpression
            , listExpression
            , tupleExpression
            , Combine.parens expression
            ]
                |> Combine.choice
                |> withRecordAccess


withRecordAccess : Parser s LocatedExpression -> Parser s LocatedExpression
withRecordAccess parser =
    let
        stuffToLocatedExpression exp accessors =
            case accessors of
                [] ->
                    exp

                (LocatedExpression location expression) :: xs ->
                    LocatedExpression location (RecordAccess exp accessors)
    in
        map2 stuffToLocatedExpression parser (Combine.many (recordAccessorFunction |> withLocation identity))


functionCall : Parser s LocatedExpression
functionCall =
    let
        listToFunctionCallParser list =
            case list of
                function :: secondArgument :: otherArguments ->
                    Combine.succeed <| FunctionCall function (secondArgument :: otherArguments)

                _ ->
                    Combine.fail "not a function call"
    in
        Combine.lazy <|
            \() ->
                Combine.sepBy Combine.whitespace1 atom
                    |> Combine.andThen listToFunctionCallParser
                    |> withLocation identity


op0 : Parser s String -> Parser s LocatedExpression -> Parser s LocatedExpression
op0 opParser previous =
    let
        makeOpExpression : ParseLocation -> String -> (LocatedExpression -> LocatedExpression -> LocatedExpression)
        makeOpExpression location opString leftExpression rightExpression =
            LocatedExpression location <| BinaryExpression leftExpression opString rightExpression

        operatorParser : Parser s (LocatedExpression -> LocatedExpression -> LocatedExpression)
        operatorParser =
            Combine.withLocation <|
                \location ->
                    opParser
                        |> withWhitespace
                        |> Combine.map (makeOpExpression location)
    in
        Combine.chainl operatorParser previous


expression : Parser s LocatedExpression
expression =
    Combine.lazy <|
        \() ->
            Combine.choice
                [ functionCall
                , atom
                ]
                |> op0 (Combine.or (Combine.string "*") (Combine.string "/"))
                |> op0 (Combine.or (Combine.string "+") (Combine.string "-"))


mainParser : Parser s LocatedExpression
mainParser =
    expression
        |> withWhitespace
        |> mustEnd


parse code =
    Combine.parse mainParser code
