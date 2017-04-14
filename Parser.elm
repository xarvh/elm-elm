module Parser exposing (..)

import Combine exposing (Parser, ParseLocation, string)
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



--| RecordAttribute Expr LowercaseIdentifier
--| RecordAttributeFunction LowercaseIdentifier
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



-- Elements


operator : Parser s String
operator =
    Combine.regex "[~!=@#$%^&*-+|<>]+"


lowercaseIdentifier : Parser s String
lowercaseIdentifier =
    Combine.regex "[a-z][a-zA-Z0-9]*"



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
    lowercaseIdentifier
        |> Combine.map VariableExpression


elementExpression : Parser s LocatedExpression
elementExpression =
    Combine.choice
        [ integerLiteral
        , prefixOperator
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


atupleExpression =
--     Combine.lazy <|
--         \() ->
            sequence "<" ">" TupleExpression

listExpression =
    Combine.lazy <|
        \() ->
            sequence "[" "]" ListExpression





-- atom : Parser s LocatedExpression
-- atom =
--     Combine.lazy <|
--         \() ->
--             [ elementExpression
--             , list
--             , tuple
--             , Combine.parens expression
--             ]
--                 |> Combine.choice
--                 |> Combine.between Combine.whitespace Combine.whitespace
{-
   functionCall : Parser s Node
   functionCall =
       let
           arrayToCall location array =
               case array of
                   [] ->
                       Element location "This is not going to happen"

                   function :: arguments ->
                       FunctionCall location function arguments
       in
           Combine.lazy <|
               \() ->
                   Combine.sepBy1 Combine.whitespace atom
                       |> toNodeParser arrayToCall

-}
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


expression : Parser s LocatedExpression
expression =
    Combine.lazy <|
        \() ->
            Combine.choice
                [ listExpression
                , atupleExpression
                , elementExpression
                ]
                  |> withWhitespace



--            let
--                prev =
--                    Combine.choice
--                        [ mustEnd atom
--                        , functionCall
--                        ]
--            in
--             Combine.choice
--                 [ elementExpression
--                 , list
--                 , tuple
--
--, op0 prev
--                 ]


parse code =
    Combine.parse (mustEnd expression) code
