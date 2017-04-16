module Main exposing (..)

import Combine
import Html exposing (..)
import Html.Attributes
import Html.Events
import Parser
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import TreeDiagram
import TreeDiagram.Svg


type alias Model =
    { code : String
    , location : String
    }


init =
    { code = """
[ 1
, (==)
, a
, [1, (==), a, sin 1, [x, y], (2, 3), 8 * 9]
, (1, (==), a, sin 1, [x, y], (2, 3), 8 * 9)
, (a)
, sin a
, 1 * Module.SubModule.record.function 5
, .standaloneAccessor
]
"""
    , location = "hover on a node to see its text location"
    }



--


type Msg
    = OnCodeInput String
    | OnHover Combine.ParseLocation


update msg model =
    case msg of
        OnCodeInput code ->
            { model | code = code }

        OnHover location ->
            { model | location = toString location }



-- VIEW


drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    Svg.line
        [ Svg.Attributes.x1 <| toString 0
        , Svg.Attributes.y1 <| toString 0
        , Svg.Attributes.x2 <| toString targetX
        , Svg.Attributes.y2 <| toString targetY
        , Svg.Attributes.stroke "black"
        ]
        []


drawNode : ( Combine.ParseLocation, String ) -> Svg Msg
drawNode ( location, label ) =
    Svg.g
        [ Svg.Events.onMouseOver (OnHover location) ]
        [ Svg.circle
            [ Svg.Attributes.r "16"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.fill "white"
            , Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Html.Attributes.alt (toString location)
            ]
            []
        , Svg.text_
            [ Svg.Attributes.transform "translate(-8 5)" ]
            [ Svg.text label ]
        ]


{-| locatedExpressionToTree
-}
le2t : Parser.LocatedExpression -> TreeDiagram.Tree ( Combine.ParseLocation, String )
le2t (Parser.LocatedExpression location expression) =
    let
        n =
            TreeDiagram.node
    in
        case expression of
            Parser.BinaryExpression left reference right ->
                n ( location, reference ) [ le2t left, le2t right ]

            Parser.FunctionCall function arguments ->
                n ( location, "call" ) (List.map le2t (function :: arguments))

            Parser.ListExpression elements ->
                n ( location, "[]" ) (List.map le2t elements)

            Parser.LiteralExpression value ->
                n ( location, value ) []

            Parser.RecordAccessorFunction name ->
                n ( location, "." ++ name ) []

            Parser.RecordAccess exp accessors ->
                n ( location, "acc" ) (List.map le2t (exp :: accessors))

            Parser.TupleExpression entries ->
                n ( location, "()" ) (List.map le2t entries)

            Parser.UnaryExpression operator value ->
                n ( location, toString operator ) [ le2t value ]

            Parser.Unit ->
                n ( location, "unit" ) []

            Parser.VariableExpression variableReference ->
                n ( location, variableReference ) []


view model =
    let
        result =
            Parser.parse model.code
    in
        div
            [ Html.Attributes.style
                [ ( "width", "100%" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                , ( "margin-top", "1rem" )
                ]
            ]
            [ Html.textarea
                [ Html.Events.onInput OnCodeInput
                , Html.Attributes.style
                    [ ( "width", "700px" )
                    , ( "height", "300px" )
                    ]
                ]
                [ text model.code ]
            , div
                [ Html.Attributes.style
                    [ ( "margin-top", "2rem" )
                    ]
                ]
                [ text model.location]

            , div
                [ Html.Attributes.style
                    [ ( "margin-top", "2rem" )
                    ]
                ]
                [ case result of
                    Err ( state, inputStream, errors ) ->
                        div
                            []
                            [ ul
                                []
                                (List.map (\e -> li [] [ text e ]) errors)
                            , div
                                []
                                [ text <| toString state ]
                            , div
                                []
                                [ text <| toString inputStream ]
                            ]

                    Ok ( _, _, tree ) ->
                        tree
                            |> le2t
                            |> TreeDiagram.Svg.draw TreeDiagram.defaultTreeLayout drawNode drawLine
                ]
            ]


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
