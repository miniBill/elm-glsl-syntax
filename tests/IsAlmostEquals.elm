module IsAlmostEquals exposing (Check, Path, declaration, expr, list, maybe, node, statement, string, toExpectation)

import Expect exposing (Expectation)
import Glsl exposing (Declaration(..), Expression(..), Statement(..), Type)
import Glsl.Node as Node exposing (Node(..))
import Glsl.PrettyPrinter
import Glsl.Simplify


type alias Path =
    List String


type alias Check =
    Result
        { path : Path
        , expected : String
        , expectedDebug : String
        , actual : String
        , actualDebug : String
        }
        ()


node : (a -> b -> c) -> Node a -> Node b -> c
node inner (Node _ l) (Node _ r) =
    inner l r


expr : Node Expression -> Node Expression -> Check
expr expected actual =
    innerExpr (Glsl.Simplify.expression expected) (Glsl.Simplify.expression actual)


innerExpr : Node Expression -> Node Expression -> Check
innerExpr expected actual =
    case ( Node.value expected, Node.value actual ) of
        ( Call el (Node _ er), Call al (Node _ ar) ) ->
            map2
                "called expression"
                (innerExpr el al)
                "arguments"
                (list innerExpr er ar)

        ( Ternary ec et ef, Ternary ac at af ) ->
            map3
                "condition"
                (innerExpr ec ac)
                "true branch"
                (innerExpr et at)
                "false branch"
                (innerExpr ef af)

        ( BinaryOperation el eop er, BinaryOperation al aop ar ) ->
            map3
                "operation"
                (equalsNode (\(Node _ op) -> Glsl.PrettyPrinter.binaryOperation op) eop aop)
                "left"
                (innerExpr el al)
                "right"
                (innerExpr er ar)

        ( UnaryOperation eop er, UnaryOperation aop ar ) ->
            map2
                "operation"
                (equalsNode (\(Node _ op) -> Glsl.PrettyPrinter.unaryOperation op) eop aop)
                "child"
                (innerExpr er ar)

        ( Dot el er, Dot al ar ) ->
            map2
                "child"
                (innerExpr el al)
                "fields"
                (string er ar)

        ( Parens l, _ ) ->
            innerExpr l actual

        ( _, Parens r ) ->
            innerExpr expected r

        ( Float l, Float r ) ->
            if isInfinite l && isInfinite r then
                equals String.fromFloat l r

            else if isNaN l && isNaN r then
                Ok ()

            else
                let
                    check : Bool
                    check =
                        abs (l - r) <= 0.000001 * max (abs l) (abs r)
                in
                if check then
                    Ok ()

                else
                    equals String.fromFloat l r

        _ ->
            equalsNode Glsl.PrettyPrinter.expr expected actual


equalsNode : (Node a -> String) -> Node a -> Node a -> Check
equalsNode ts e a =
    if Node.value e == Node.value a then
        Ok ()

    else
        Err
            { path = []
            , expected = ts e
            , expectedDebug = Debug.toString e
            , actual = ts a
            , actualDebug = Debug.toString a
            }


equals : (a -> String) -> a -> a -> Check
equals ts e a =
    if e == a then
        Ok ()

    else
        Err
            { path = []
            , expected = ts e
            , expectedDebug = Debug.toString e
            , actual = ts a
            , actualDebug = Debug.toString a
            }


map2 : String -> Check -> String -> Check -> Check
map2 atl l atr r =
    Result.map2 (\_ _ -> ()) (withPath atl l) (withPath atr r)


map3 : String -> Check -> String -> Check -> String -> Check -> Check
map3 atl l atm m atr r =
    Result.map3 (\_ _ _ -> ()) (withPath atl l) (withPath atm m) (withPath atr r)


map4 : String -> Check -> String -> Check -> String -> Check -> String -> Check -> Check
map4 atl l atm m atr r atq q =
    Result.map4 (\_ _ _ _ -> ()) (withPath atl l) (withPath atm m) (withPath atr r) (withPath atq q)


withPath : String -> Check -> Check
withPath piece result =
    case result of
        Ok () ->
            Ok ()

        Err err ->
            Err { err | path = piece :: err.path }


statement : Node Statement -> Node Statement -> Check
statement expected actual =
    innerStatement
        (Glsl.Simplify.statement expected)
        (Glsl.Simplify.statement actual)


innerStatement : Node Statement -> Node Statement -> Check
innerStatement expected actual =
    case ( Node.value expected, Node.value actual ) of
        ( Decl etype ename einit, Decl atype aname ainit ) ->
            map3
                "type"
                (type_ etype atype)
                "name"
                (string ename aname)
                "value"
                (maybe expr einit ainit)

        ( Return el, Return al ) ->
            expr el al

        ( If el em, If al am ) ->
            map2
                "condition"
                (expr el al)
                "statement"
                (innerStatement em am)

        ( IfElse el em er, IfElse al am ar ) ->
            map3
                "condition"
                (expr el al)
                "true branch"
                (innerStatement em am)
                "false branch"
                (innerStatement er ar)

        ( For el em er ep, For al am ar ap ) ->
            map4
                "init"
                (maybe innerStatement el al)
                "check"
                (expr em am)
                "step"
                (expr er ar)
                "child"
                (innerStatement ep ap)

        ( ExpressionStatement el, ExpressionStatement al ) ->
            expr el al

        ( Block ec, Block ac ) ->
            list innerStatement ec ac

        _ ->
            equalsNode (Glsl.PrettyPrinter.stat 0) expected actual


declaration : Node Declaration -> Node Declaration -> Check
declaration expected actual =
    let
        names : Node String -> Node String -> String
        names (Node _ ex) (Node _ ac) =
            if ex == ac then
                ex

            else
                ex ++ "/" ++ ac
    in
    case ( Node.value expected, Node.value actual ) of
        ( ConstDeclaration et en ev, ConstDeclaration at an av ) ->
            withPath ("const " ++ names en an) <|
                map3
                    "type"
                    (type_ et at)
                    "name"
                    (string en an)
                    "value"
                    (innerExpr ev av)

        ( FunctionDeclaration et en (Node _ ea) es, FunctionDeclaration at an (Node _ aa) as_ ) ->
            withPath ("function " ++ names en an) <|
                map4
                    "type"
                    (type_ et at)
                    "name"
                    (string en an)
                    "args"
                    (list (tuple type_ string) ea aa)
                    "stat"
                    (list innerStatement es as_)

        ( UniformDeclaration et en, UniformDeclaration at an ) ->
            withPath ("uniform " ++ names en an) <|
                map2
                    "type"
                    (type_ et at)
                    "name"
                    (string en an)

        _ ->
            equalsNode Glsl.PrettyPrinter.declaration expected actual


tuple :
    (a -> a -> Check)
    -> (b -> b -> Check)
    -> ( a, b )
    -> ( a, b )
    -> Check
tuple f s ( ef, es ) ( af, as_ ) =
    map2
        "first"
        (f ef af)
        "second"
        (s es as_)


type_ : Node Type -> Node Type -> Check
type_ expected actual =
    equalsNode Glsl.PrettyPrinter.type_ expected actual


string : Node String -> Node String -> Check
string expected actual =
    equalsNode Node.value expected actual


list : (a -> a -> Check) -> List a -> List a -> Check
list f exps acts =
    let
        go : Int -> List a -> List a -> Check
        go i e a =
            case ( e, a ) of
                ( eh :: et, ah :: at ) ->
                    withPath (String.fromInt i) (f eh ah)
                        |> Result.andThen (\_ -> go (i + 1) et at)

                ( [], [] ) ->
                    Ok ()

                ( _ :: _, [] ) ->
                    equals Debug.toString e a

                ( [], _ :: _ ) ->
                    equals Debug.toString e a
    in
    go 0 exps acts


maybe : (a -> a -> Check) -> Maybe a -> Maybe a -> Check
maybe check expected actual =
    case ( expected, actual ) of
        ( Nothing, Nothing ) ->
            Ok ()

        ( Just e, Just a ) ->
            check e a

        ( Just _, Nothing ) ->
            equals Debug.toString expected actual

        ( Nothing, Just _ ) ->
            equals Debug.toString expected actual


toExpectation : Check -> Expectation
toExpectation check =
    case check of
        Ok () ->
            Expect.pass

        Err err ->
            let
                ( expectedString, actualString ) =
                    if err.expectedDebug == err.actualDebug then
                        ( err.expected, err.actual )

                    else
                        ( err.expected ++ " (" ++ err.expectedDebug ++ ")"
                        , err.actual ++ " (" ++ err.actualDebug ++ ")"
                        )
            in
            Expect.fail ("At " ++ pathToString err.path ++ "\n\nExpected: " ++ expectedString ++ "\n\nActual:   " ++ actualString)


pathToString : Path -> String
pathToString path =
    path
        |> String.join " -> "
