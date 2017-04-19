module Parser exposing (..)

import Combine exposing (Parser, ParseLocation, (*>))
import Combine.Num


-- Structure more or less copied from avh4's fantastic elm-format
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



-- Operators


type Associativity
    = LeftAssociative
    | RightAssociative


type alias OpGroup s =
    { parser : Parser s String
    , precedence : Int
    , associativity : Associativity
    }


type alias Ops s =
    List (OpGroup s)



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


operatorString : Parser s String
operatorString =
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
    operatorString
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


sequence : Ops s -> String -> String -> (List LocatedExpression -> Expression) -> Parser s LocatedExpression
sequence ops leftDelimiter rightDelimiter listToExpression =
    Combine.lazy <|
        \() ->
            expression ops
                |> withWhitespace
                |> Combine.sepBy (Combine.string ",")
                |> Combine.between (Combine.string leftDelimiter) (Combine.string rightDelimiter)
                |> withLocation listToExpression


listExpression ops =
    Combine.lazy <|
        \() ->
            sequence ops "[" "]" ListExpression


tupleExpression ops =
    Combine.lazy <|
        \() ->
            sequence ops "(" ")" TupleExpression


{-| An atom is something that doesn't need precedence rules
-}
atom : Ops s -> Parser s LocatedExpression
atom ops =
    Combine.lazy <|
        \() ->
            [ elementExpression
            , listExpression ops
            , tupleExpression ops
            , Combine.parens (expression ops)
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


functionCall : Ops s -> Parser s LocatedExpression
functionCall ops =
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
                Combine.sepBy Combine.whitespace1 (atom ops)
                    |> Combine.andThen listToFunctionCallParser
                    |> withLocation identity


operatorParser : OpGroup s -> Parser s LocatedExpression -> Parser s LocatedExpression
operatorParser opGroup higherPrecedenceParser =
    let
        makeOpExpression : ParseLocation -> String -> (LocatedExpression -> LocatedExpression -> LocatedExpression)
        makeOpExpression location opString leftExpression rightExpression =
            LocatedExpression location <| BinaryExpression leftExpression opString rightExpression

        operatorParser : Parser s (LocatedExpression -> LocatedExpression -> LocatedExpression)
        operatorParser =
            Combine.withLocation <|
                \location ->
                    opGroup.parser
                        |> withWhitespace
                        |> Combine.map (makeOpExpression location)

        chain =
            case opGroup.associativity of
                LeftAssociative ->
                    Combine.chainl

                RightAssociative ->
                    Combine.chainr
    in
        chain operatorParser higherPrecedenceParser


expression : Ops s -> Parser s LocatedExpression
expression ops =
    Combine.lazy <|
        \() ->
            let
                parserWithHigherPrecedenceThanAnyOperator =
                    Combine.choice
                        [ functionCall ops
                        , atom ops
                        ]
            in
                List.foldl operatorParser parserWithHigherPrecedenceThanAnyOperator ops


op0 : Ops s
op0 =
    [ { parser = [ "+", "-" ] |> List.map Combine.string |> Combine.choice
      , precedence = 6
      , associativity = LeftAssociative
      }
    , { parser = [ "*", "/" ] |> List.map Combine.string |> Combine.choice
      , precedence = 7
      , associativity = LeftAssociative
      }
    , { parser = Combine.string "^"
      , precedence = 8
      , associativity = RightAssociative
      }
    , { parser = [ "//", "%" ] |> List.map Combine.string |> Combine.choice
      , precedence = 7
      , associativity = LeftAssociative
      }

    -- catch ops that don't have a precedence/associativity declaration
    , { parser = operatorString
      , precedence = 0
      , associativity = LeftAssociative
      }
    ]
        |> List.sortBy .precedence
        |> List.reverse


mainParser : Parser s LocatedExpression
mainParser =
    expression op0
        |> withWhitespace
        |> mustEnd


parse code =
    Combine.parse mainParser code
