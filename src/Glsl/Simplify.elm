module Glsl.Simplify exposing (expr, stat)

import Glsl exposing (Expression(..), Statement(..), UnaryOperation(..))


stat : Statement -> Statement
stat root =
    case root of
        Return e ->
            Return (expr e)

        ExpressionStatement e ->
            ExpressionStatement (expr e)

        If e s1 ->
            If (expr e) (stat s1)

        IfElse e s1 s2 ->
            IfElse (expr e) (stat s1) (stat s2)

        Decl tipe name val ->
            Decl tipe name (Maybe.map expr val)

        For init check step loop ->
            For (Maybe.map stat init) (expr check) (expr step) (stat loop)

        Block [] ->
            Nop

        Block [ s ] ->
            stat s

        Block children ->
            Block (List.map stat children)

        Nop ->
            Nop

        Break ->
            Break

        Continue ->
            Continue


expr : Expression -> Expression
expr root =
    case root of
        Ternary c t f ->
            Ternary (expr c) (expr t) (expr f)

        Dot l r ->
            Dot (expr l) r

        BinaryOperation l op r ->
            BinaryOperation (expr l) op (expr r)

        UnaryOperation Negate (Int i) ->
            Int -i

        UnaryOperation Negate (Float f) ->
            Float -f

        UnaryOperation op l ->
            UnaryOperation op (expr l)

        Call l r ->
            Call (expr l) (List.map expr r)

        Bool _ ->
            root

        Int _ ->
            root

        Float _ ->
            root

        Uint _ ->
            root

        Double _ ->
            root

        Variable _ ->
            root
