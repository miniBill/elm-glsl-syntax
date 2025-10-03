module ExpressionRoundtripTests exposing (examples, fuzzer, roundtrip, variableNameFuzzer)

import ErrorUtils
import Expect
import Fuzz exposing (Fuzzer)
import Glsl exposing (BinaryOperation(..), Expression(..), RelationOperation(..), UnaryOperation(..))
import Glsl.Node as Node exposing (Node)
import Glsl.Parser
import Glsl.PrettyPrinter
import IsAlmostEquals
import Parser
import Parser.Advanced exposing ((|.))
import Set exposing (Set)
import Test exposing (Test, describe, test)


examples : Test
examples =
    describe "Expression examples"
        [ example "-1" (Node.combine UnaryOperation (Node.empty Negate) (Node.empty (Int 1)))
        , example "1" (Node.empty (Int 1))
        , example "-1++" (Node.combine UnaryOperation (Node.empty PostfixIncrement) (Node.empty (Int -1)))
        , example "(false ? false : false)++"
            (Node.combine UnaryOperation
                (Node.empty PostfixIncrement)
                (Node.combine3 Ternary (Node.empty (Bool False)) (Node.empty (Bool False)) (Node.empty (Bool False)))
            )
        ]


example : String -> Node Expression -> Test
example label expr =
    test label <| \_ ->
    expr
        |> Glsl.PrettyPrinter.expr
        |> Expect.equal label


roundtrip : Test
roundtrip =
    Test.fuzz (fuzzer 3) "Expression roundtrips" <| \expr ->
    let
        str : String
        str =
            Glsl.PrettyPrinter.expr expr
    in
    case Parser.Advanced.run (Glsl.Parser.expression |. Parser.Advanced.end Parser.ExpectingEnd) str of
        Err errs ->
            errs
                |> ErrorUtils.errorsToString str
                |> Expect.fail

        Ok actual ->
            actual
                |> IsAlmostEquals.expr expr
                |> IsAlmostEquals.toExpectation


fuzzer : Int -> Fuzzer (Node Expression)
fuzzer depth =
    let
        base : Fuzzer (Node Expression)
        base =
            Fuzz.oneOf
                [ Fuzz.map Bool Fuzz.bool
                , Fuzz.map Int Fuzz.int
                , Fuzz.map Float Fuzz.niceFloat
                ]
                |> Fuzz.map Node.empty

        inner : Fuzzer (Node Expression) -> Fuzzer (Node Expression)
        inner child =
            Fuzz.oneOf
                [ Fuzz.map (\v -> v |> Bool |> Node.empty) Fuzz.bool
                , Fuzz.map (\v -> v |> Int |> Node.empty) Fuzz.int
                , Fuzz.map (\v -> v |> Float |> Node.empty) Fuzz.niceFloat
                , Fuzz.map (\v -> v |> Variable |> Node.empty) variableNameFuzzer
                , Fuzz.map3 (Node.combine3 Ternary) child child child
                , Fuzz.map2 excludeNonsensicalUnary unaryOperationFuzzer child
                , Fuzz.map3 excludeNonsensicalBinary child binaryOperationFuzzer child
                , Fuzz.map2 (Node.combine Call)
                    (Fuzz.map (\v -> v |> Variable |> Node.empty) variableNameFuzzer)
                    (Fuzz.map Node.empty (Fuzz.listOfLengthBetween 0 3 child))
                , Fuzz.map2 excludeNonsensicalDot child (Fuzz.map Node.empty variableNameFuzzer)
                ]
    in
    List.foldl (\_ -> inner) base (List.range 1 depth)


excludeNonsensicalDot : Node Expression -> Node String -> Node Expression
excludeNonsensicalDot child var =
    case Node.value child of
        Int _ ->
            child

        Float _ ->
            child

        _ ->
            Node.combine Dot child var


excludeNonsensicalBinary : Node Expression -> Node BinaryOperation -> Node Expression -> Node Expression
excludeNonsensicalBinary l op r =
    case ( Node.value l, Node.value op, Node.value r ) of
        ( Float _, ArraySubscript, _ ) ->
            r

        ( Int _, ArraySubscript, _ ) ->
            r

        _ ->
            Node.combine3 BinaryOperation l op r


excludeNonsensicalUnary : Node UnaryOperation -> Node Expression -> Node Expression
excludeNonsensicalUnary op c =
    case ( Node.value op, Node.value c ) of
        ( PostfixIncrement, Int _ ) ->
            -- this wouldn't make sense
            c

        ( PostfixDecrement, Int _ ) ->
            -- this wouldn't make sense
            c

        ( PostfixIncrement, Float _ ) ->
            -- this wouldn't make sense
            c

        ( PostfixDecrement, Float _ ) ->
            -- this wouldn't make sense
            c

        ( PrefixIncrement, Int _ ) ->
            -- this wouldn't make sense
            c

        ( PrefixDecrement, Int _ ) ->
            -- this wouldn't make sense
            c

        ( PrefixIncrement, Float _ ) ->
            -- this wouldn't make sense
            c

        ( PrefixDecrement, Float _ ) ->
            -- this wouldn't make sense
            c

        ( Negate, Float f ) ->
            Node.empty (Float -f)

        ( Negate, Int i ) ->
            Node.empty (Int -i)

        _ ->
            Node.combine UnaryOperation op c


variableNameFuzzer : Fuzzer String
variableNameFuzzer =
    Fuzz.oneOf
        [ List.range (Char.toCode 'a') (Char.toCode 'z')
            |> List.map Char.fromCode
            |> Fuzz.oneOfValues
            |> Fuzz.listOfLengthBetween 1 10
            |> Fuzz.map String.fromList
        , Fuzz.oneOfValues (Set.toList reserved)
        ]
        |> Fuzz.map
            (\str ->
                if Set.member str reserved then
                    str ++ "_"

                else
                    str
            )


reserved : Set String
reserved =
    [ "break"
    , "return"
    , "continue"
    , "if"
    , "for"
    ]
        |> Set.fromList


binaryOperationFuzzer : Fuzzer (Node BinaryOperation)
binaryOperationFuzzer =
    Fuzz.oneOfValues
        [ ArraySubscript
        , By
        , Div
        , Mod
        , Add
        , Subtract
        , ShiftLeft
        , ShiftRight
        , RelationOperation LessThan
        , RelationOperation GreaterThan
        , RelationOperation LessThanOrEquals
        , RelationOperation GreaterThanOrEquals
        , RelationOperation Equals
        , RelationOperation NotEquals
        , BitwiseAnd
        , BitwiseOr
        , BitwiseXor
        , And
        , Xor
        , Or
        , Assign
        , ComboAdd
        , ComboSubtract
        , ComboBy
        , ComboDiv
        , ComboMod
        , ComboLeftShift
        , ComboRightShift
        , ComboBitwiseAnd
        , ComboBitwiseXor
        , ComboBitwiseOr
        , Comma
        ]
        |> Fuzz.map Node.empty


unaryOperationFuzzer : Fuzzer (Node UnaryOperation)
unaryOperationFuzzer =
    Fuzz.oneOfValues
        [ PostfixIncrement
        , PostfixDecrement
        , PrefixIncrement
        , PrefixDecrement
        , Plus
        , Negate
        , Invert
        , Not
        ]
        |> Fuzz.map Node.empty
