module StatementRoundtripTests exposing (examples, roundtrip)

import ErrorUtils
import Expect
import ExpressionRoundtripTests
import Fuzz exposing (Fuzzer)
import Glsl exposing (Expression, Statement(..), Type(..))
import Glsl.Node as Node exposing (Node(..))
import Glsl.Parser
import Glsl.PrettyPrinter
import IsAlmostEquals
import Parser
import Parser.Advanced exposing ((|.))
import Test exposing (Test, describe, test)
import Utils exposing (i, var)


examples : Test
examples =
    describe "Statement examples"
        [ example "1;" (Node.empty (ExpressionStatement (i 1)))
        , example """if (a) {
    1;
} else {
    0;
}"""
            (Node.combine3
                IfElse
                (var "a")
                (Node.empty (ExpressionStatement (i 1)))
                (Node.empty (ExpressionStatement (i 0)))
            )
        ]


example : String -> Node Statement -> Test
example label stat =
    test label <| \_ ->
    stat
        |> Glsl.PrettyPrinter.stat 0
        |> Expect.equal label


roundtrip : Test
roundtrip =
    Test.fuzz (statFuzzer 3) "Statement roundtrips" <| \expected ->
    let
        str : String
        str =
            Glsl.PrettyPrinter.stat 0 expected
    in
    case Parser.Advanced.run (Glsl.Parser.statement |. Parser.Advanced.end Parser.ExpectingEnd) str of
        Err errs ->
            errs
                |> ErrorUtils.errorsToString str
                |> Expect.fail

        Ok actual ->
            actual
                |> IsAlmostEquals.statement expected
                |> IsAlmostEquals.toExpectation


statFuzzer : Int -> Fuzzer (Node Statement)
statFuzzer depth =
    let
        base : Fuzzer (Node Statement)
        base =
            Fuzz.oneOf
                [ Fuzz.constant Break
                , Fuzz.constant Continue
                , Fuzz.map Return (ExpressionRoundtripTests.fuzzer 0)
                ]
                |> Fuzz.map Node.empty

        inner : Fuzzer (Node Expression) -> Fuzzer (Node Statement) -> Fuzzer (Node Statement)
        inner expr child =
            Fuzz.oneOf
                [ Fuzz.constant (Node.empty Break)
                , Fuzz.constant (Node.empty Continue)
                , Fuzz.map (lift Return) expr
                , Fuzz.map2 (Node.combine If) expr child
                , Fuzz.map3 (Node.combine3 IfElse) expr child child
                , Fuzz.map4 (\m -> Node.combine3 (For m)) (Fuzz.maybe child) expr expr child
                , Fuzz.map (lift ExpressionStatement) expr
                , Fuzz.map3 (\t n e -> Node.combine (\tv nv -> Decl tv nv e) t n)
                    typeFuzzer
                    (ExpressionRoundtripTests.variableNameFuzzer |> Fuzz.map Node.empty)
                    (Fuzz.maybe expr)
                , Fuzz.map (\v -> v |> Block |> Node.empty) (Fuzz.listOfLengthBetween 0 3 child)
                ]
    in
    List.foldl (\i -> inner (ExpressionRoundtripTests.fuzzer i)) base (List.range 1 depth)


lift : (Node a -> b) -> Node a -> Node b
lift f ((Node r _) as v) =
    Node r (f v)


typeFuzzer : Fuzzer (Node Type)
typeFuzzer =
    let
        base : Fuzzer (Node Type)
        base =
            Fuzz.oneOfValues
                [ Tvoid
                , Tfloat
                , Tint
                , Tbool
                , Tvec2
                , Tvec3
                , Tvec4
                , Tivec2
                , Tivec3
                , Tivec4
                , Tmat3
                ]
                |> Fuzz.map Node.empty
    in
    Fuzz.oneOf
        [ base

        -- , Fuzz.map Tin base
        -- , Fuzz.map Tout base
        ]
