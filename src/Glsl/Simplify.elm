module Glsl.Simplify exposing (expression, statement)

import Glsl exposing (Expression(..), Statement(..), UnaryOperation(..))
import Glsl.Node as Node exposing (Node(..))


node : (a -> b) -> Node a -> Node b
node f n =
    Node.map f n


statement : Node Statement -> Node Statement
statement =
    Node.map
        (\root ->
            case root of
                Return e ->
                    Return (expression e)

                ExpressionStatement e ->
                    ExpressionStatement (expression e)

                If e s1 ->
                    If (expression e) (statement s1)

                IfElse e s1 s2 ->
                    IfElse (expression e) (statement s1) (statement s2)

                Decl c tipe name val ->
                    Decl c tipe name (Maybe.map expression val)

                For init check step loop ->
                    For
                        (Maybe.map statement init)
                        (expression check)
                        (expression step)
                        (statement loop)

                Block children ->
                    Block (List.map statement children)

                Break ->
                    Break

                Continue ->
                    Continue
        )


expression : Node Expression -> Node Expression
expression =
    Node.map
        (\root ->
            case root of
                Ternary c t f ->
                    Ternary (expression c) (expression t) (expression f)

                Dot l r ->
                    Dot (expression l) r

                BinaryOperation l op r ->
                    BinaryOperation (expression l) op (expression r)

                UnaryOperation (Node _ Plus) (Node _ (Int i)) ->
                    Int i

                UnaryOperation (Node _ Plus) (Node _ (Float f)) ->
                    Float f

                UnaryOperation (Node _ Negate) (Node _ (Int i)) ->
                    Int -i

                UnaryOperation (Node _ Negate) (Node _ (Float f)) ->
                    Float -f

                UnaryOperation op l ->
                    UnaryOperation op (expression l)

                Call l r ->
                    Call (expression l) (node (List.map expression) r)

                Bool _ ->
                    root

                Int _ ->
                    root

                Float _ ->
                    root

                Variable _ ->
                    root

                Parens c ->
                    Parens (expression c)
        )
