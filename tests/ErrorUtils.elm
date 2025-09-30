module ErrorUtils exposing (errorsToString, expectEqualMultiline)

import Ansi.Color
import Diff
import Diff.ToString
import Expect
import Glsl.Parser
import Parser exposing (Problem)
import Parser.Advanced exposing (DeadEnd)
import Parser.Error


errorsToString : String -> List (DeadEnd Glsl.Parser.Context Problem) -> String
errorsToString input e =
    Parser.Error.renderError
        { text = identity
        , formatCaret = Ansi.Color.fontColor Ansi.Color.cyan
        , newline = "\n"
        , formatContext = Ansi.Color.fontColor Ansi.Color.red
        , linesOfExtraContext = 3
        }
        { contextStack =
            \{ contextStack } ->
                List.map
                    (\{ row, col, context } ->
                        { row = row
                        , col = col
                        , context = contextToString context
                        }
                    )
                    contextStack
        , problemToString = Parser.Error.problemToExpected
        }
        input
        e
        |> String.join "\n"


contextToString : Glsl.Parser.Context -> String
contextToString context =
    case context of
        Glsl.Parser.ParsingFile ->
            "File"

        Glsl.Parser.ParsingFunction ->
            "Function"

        Glsl.Parser.ParsingStatement ->
            "Statement"

        Glsl.Parser.ParsingExpression ->
            "Expression"

        Glsl.Parser.ParsingForInitialization ->
            "For Initialization"

        Glsl.Parser.ParsingForCondition ->
            "For Condition"

        Glsl.Parser.ParsingForStep ->
            "For Step"

        Glsl.Parser.ParsingForBody ->
            "For Body"


expectEqualMultiline : String -> String -> Expect.Expectation
expectEqualMultiline exp actual =
    if exp == actual then
        Expect.pass

    else
        let
            header : String
            header =
                Ansi.Color.fontColor Ansi.Color.blue "Diff from expected to actual:"
        in
        Expect.fail
            (header
                ++ "\n"
                ++ (Diff.diffLinesWith
                        (Diff.defaultOptions
                            |> Diff.ignoreLeadingWhitespace
                        )
                        exp
                        actual
                        |> Diff.ToString.diffToString { context = 4, color = True }
                   )
            )
