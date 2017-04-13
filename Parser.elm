module Parser exposing (..)

import Combine exposing (Parser, ParseLocation, string)
import Combine.Num


-- Structure nmore or less copied from avh4's fantastic elm-format
-- https://github.com/avh4/elm-format/blob/master/parser/src/AST/Expression.hs


type UnaryOperator
    = Negative



-- type LetDeclaration
--   = LetDefinition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments Expr
--   | LetAnnotation (Var.Ref, Comments) (Comments, Type)
--   | LetComment Comment


type alias LiteralValue =
    String


type alias VariableReference =
    String


type LocatedExpression
    = LocatedExpression ParseLocation Expression


type Expression
    = Unit
    | Literal LiteralValue
    | Variable VariableReference
    | FunctionCall LocatedExpression (List LocatedExpression)
      --| Unary UnaryOperator LocatedExpression
    | Binary LocatedExpression VariableReference LocatedExpression
    | Parens LocatedExpression
    | Array (List LocatedExpression)
    | Tuple (List LocatedExpression)



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


withLocation : (ParseLocation -> a -> LocatedExpression) -> Parser s a -> Parser s LocatedExpression
withLocation valueToNode parser =
    Combine.lazy <|
        \() ->
            Combine.withLocation <|
                \location ->
                    Combine.map (valueToNode location) parser



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
        |> Combine.map Literal


prefixOperator : Parser s Expression
prefixOperator =
    operator
        |> Combine.parens
        |> Combine.map Variable


variable : Parser s Expression
variable =
    lowercaseIdentifier
        |> Combine.map Variable


elementExpression : Parser s LocatedExpression
elementExpression =
    Combine.choice
        [ integerLiteral
        , prefixOperator
        , variable
        ]
        |> withLocation LocatedExpression



-- Higher order constructs


{-
list : Parser s Expression
list =
            expression
                |> Combine.sepBy (Combine.string ",")
                |> Combine.between (Combine.string "[") (Combine.string "]")
                |> toNodeParser (\location array -> FunctionCall location (Element location "[]") array)


tuple : Parser s Node
tuple =
    Combine.lazy <|
        \() ->
            expression
                |> Combine.sepBy (Combine.string ",")
                |> Combine.between (Combine.string "(") (Combine.string ")")
                |> toNodeParser (\location array -> FunctionCall location (Element location "()") array)


atom : Parser s Node
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
    elementExpression
{-
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

                    --, op0 prev
                    ]
-}


parse code =
    Combine.parse (mustEnd expression) code
