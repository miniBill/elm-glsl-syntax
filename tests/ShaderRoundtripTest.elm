module ShaderRoundtripTest exposing (simple)

import ErrorUtils
import Expect
import Glsl exposing (BinaryOperation(..), Declaration(..), Expression(..), Statement(..), Type(..))
import Glsl.Parser
import Parser
import Parser.Advanced exposing ((|.))
import Test exposing (Test, test)


simple : Test
simple =
    checkParses "Simple shader" simpleSrc


checkParses : String -> String -> Test
checkParses label source =
    test label <| \_ ->
    case
        Parser.Advanced.run
            (Glsl.Parser.file
                |. Parser.Advanced.end Parser.ExpectingEnd
            )
            source
    of
        Err errs ->
            errs
                |> ErrorUtils.errorsToString source
                |> Expect.fail

        Ok o ->
            o
                |> Expect.equal
                    ( Just { version = 300 }
                    , [ FunctionDeclaration Tvoid "main" [] <|
                            [ ExpressionStatement
                                (BinaryOperation
                                    (Variable "pos3")
                                    Assign
                                    (Call
                                        (Variable "vec3")
                                        [ Variable "pos"
                                        , Int 1
                                        ]
                                    )
                                )
                            ]
                      ]
                    )


simpleSrc : String
simpleSrc =
    """#version 300
  
void main()
{
  pos3 = vec3(pos, 1);
}
"""
