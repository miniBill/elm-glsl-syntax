module Glsl.Parser exposing (Context(..), DeadEnd, Parser, expression, file, function, statement)

import Glsl exposing (BinaryOperation(..), Declaration(..), Expression(..), RelationOperation(..), Statement(..), Type(..), UnaryOperation(..))
import Glsl.Node as Node exposing (Node(..))
import Parser exposing (Problem(..))
import Parser.Advanced exposing ((|.), (|=), Step(..), Trailing(..))
import ParserWithContext exposing (chompIf, chompWhile, end, float, getChompedString, inContext, int, keyword, location, loop, many, node, oneOf, sequence, spaces, succeed, symbol)


type Context
    = ParsingFile
    | ParsingFunction
    | ParsingStatement
    | ParsingExpression
    | ParsingForInitialization
    | ParsingForCondition
    | ParsingForStep
    | ParsingForBody


type alias Parser a =
    ParserWithContext.Parser Context a


type alias DeadEnd =
    { row : Int
    , col : Int
    , problem : Problem
    , contextStack : List { row : Int, col : Int, context : Context }
    }


file : Parser ( Maybe { version : Int }, List (Node Declaration) )
file =
    succeed Tuple.pair
        |. spaces
        |= oneOf
            [ succeed (\version -> Just { version = version })
                |. symbol "#version"
                |= int
            , succeed Nothing
            ]
        |= many
            (oneOf
                [ const
                , uniform
                , function
                ]
                |> node
            )
        |. end
        |> inContext ParsingFile


function : Parser Declaration
function =
    succeed FunctionDeclaration
        |= typeParser
        |= node identifierParser
        |= node
            (sequence
                { start = "("
                , end = ")"
                , separator = ","
                , item = argParser
                , trailing = Forbidden
                }
            )
        |= sequence
            { start = "{"
            , item = statement
            , separator = ""
            , end = "}"
            , trailing = Optional
            }
        |> inContext ParsingFunction


const : Parser Declaration
const =
    succeed ConstDeclaration
        |. keyword "const"
        |= typeParser
        |= node identifierParser
        |. symbol "="
        |= expression
        |. symbol ";"


uniform : Parser Declaration
uniform =
    succeed UniformDeclaration
        |. keyword "uniform"
        |= typeParser
        |= node identifierParser
        |. symbol ";"


argParser : Parser ( Node Type, Node String )
argParser =
    succeed Tuple.pair
        |= typeParser
        |= node identifierParser


identifierParser : Parser String
identifierParser =
    getChompedString
        (succeed ()
            |. chompIf (\c -> Char.isAlpha c || c == '_') (Expecting "Letter or underscore")
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')
        )
        |. spaces


typeParser : Parser (Node Type)
typeParser =
    let
        baseParser : Parser (Node Type)
        baseParser =
            types
                |> List.map (\( s, t ) -> succeed t |. keyword s)
                |> oneOf
                |> node
    in
    succeed identity
        |. oneOf
            [ keyword "const"
            , succeed ()
            ]
        |= oneOf
            [ succeed Tout
                |. keyword "out"
                |= baseParser
                |> node
            , succeed Tin
                |. keyword "in"
                |= baseParser
                |> node
            , baseParser
            ]


types : List ( String, Type )
types =
    [ ( "void", Tvoid )
    , ( "bool", Tbool )
    , ( "bvec2", Tbvec2 )
    , ( "bvec3", Tbvec3 )
    , ( "bvec4", Tbvec4 )
    , ( "int", Tint )
    , ( "ivec2", Tivec2 )
    , ( "ivec3", Tivec3 )
    , ( "ivec4", Tivec4 )
    , ( "uint", Tuint )
    , ( "uvec2", Tuvec2 )
    , ( "uvec3", Tuvec3 )
    , ( "uvec4", Tuvec4 )
    , ( "float", Tfloat )
    , ( "vec2", Tvec2 )
    , ( "vec3", Tvec3 )
    , ( "vec4", Tvec4 )
    , ( "double", Tdouble )
    , ( "dvec2", Tdvec2 )
    , ( "dvec3", Tdvec3 )
    , ( "dvec4", Tdvec4 )
    , ( "mat2", Tmat2 )
    , ( "mat2x2", Tmat2 )
    , ( "mat3", Tmat3 )
    , ( "mat3x3", Tmat3 )
    , ( "mat4", Tmat4 )
    , ( "mat4x4", Tmat4 )
    , ( "mat2x3", Tmat2x3 )
    , ( "mat2x4", Tmat2x4 )
    , ( "mat3x2", Tmat3x2 )
    , ( "mat3x4", Tmat3x4 )
    , ( "mat4x2", Tmat4x2 )
    , ( "mat4x3", Tmat4x3 )
    , ( "dmat2", Tdmat2 )
    , ( "dmat2x2", Tdmat2 )
    , ( "dmat3", Tdmat3 )
    , ( "dmat3x3", Tdmat3 )
    , ( "dmat4", Tdmat4 )
    , ( "dmat4x4", Tdmat4 )
    , ( "dmat2x3", Tdmat2x3 )
    , ( "dmat2x4", Tdmat2x4 )
    , ( "dmat3x2", Tdmat3x2 )
    , ( "dmat3x4", Tdmat3x4 )
    , ( "dmat4x2", Tdmat4x2 )
    , ( "dmat4x3", Tdmat4x3 )
    , ( "sampler1d", Tsampler1D )
    , ( "image1d", Timage1D )
    , ( "sampler2d", Tsampler2D )
    , ( "image2d", Timage2D )
    , ( "sampler3d", Tsampler3D )
    , ( "image3d", Timage3D )
    , ( "samplercube", TsamplerCube )
    , ( "imagecube", TimageCube )
    , ( "sampler2drect", Tsampler2DRect )
    , ( "image2drect", Timage2DRect )
    , ( "sampler1darray", Tsampler1DArray )
    , ( "image1darray", Timage1DArray )
    , ( "sampler2darray", Tsampler2DArray )
    , ( "image2darray", Timage2DArray )
    , ( "samplerbuffer", TsamplerBuffer )
    , ( "imagebuffer", TimageBuffer )
    , ( "sampler2dms", Tsampler2DMS )
    , ( "image2dms", Timage2DMS )
    , ( "sampler2dmsarray", Tsampler2DMSArray )
    , ( "image2dmsarray", Timage2DMSArray )
    , ( "samplercubearray", TsamplerCubeArray )
    , ( "imagecubearray", TimageCubeArray )
    , ( "sampler1dshadow", Tsampler1DShadow )
    , ( "sampler2dshadow", Tsampler2DShadow )
    , ( "sampler2drectshadow", Tsampler2DRectShadow )
    , ( "sampler1darrayshadow", Tsampler1DArrayShadow )
    , ( "sampler2darrayshadow", Tsampler2DArrayShadow )
    , ( "samplercubeshadow", TsamplerCubeShadow )
    , ( "samplercubearrayshadow", TsamplerCubeArrayShadow )
    ]


statement : Parser (Node Statement)
statement =
    (ParserWithContext.lazy <| \_ ->
    oneOf
        [ blockParser
        , returnParser
        , breakContinueParser
        , ifParser
        , forParser
        , defParser
        , expressionStatementParser
        ]
    )
        |> node
        |> inContext ParsingStatement


breakContinueParser : Parser Statement
breakContinueParser =
    oneOf
        [ succeed Break
            |. keyword "break"
        , succeed Continue
            |. keyword "continue"
        ]
        |. symbol ";"


expressionStatementParser : Parser Statement
expressionStatementParser =
    succeed ExpressionStatement
        |= expression
        |. symbol ";"


ifParser : Parser Statement
ifParser =
    succeed (\e s k -> k e s)
        |. keyword "if"
        |. symbol "("
        |= expression
        |. symbol ")"
        |= statement
        |= oneOf
            [ succeed (\b e s -> IfElse e s b)
                |. keyword "else"
                |= statement
            , succeed If
            ]


forParser : Parser Statement
forParser =
    succeed For
        |. keyword "for"
        |. symbol "("
        |= (oneOf
                [ succeed Just |= statement
                , succeed Nothing
                    |. symbol ";"
                ]
                |> inContext ParsingForInitialization
           )
        |= (expression |> inContext ParsingForCondition)
        |. symbol ";"
        |= (expression |> inContext ParsingForStep)
        |. symbol ")"
        |= (statement |> inContext ParsingForBody)


returnParser : Parser Statement
returnParser =
    succeed Return
        |. keyword "return"
        |= expression
        |. symbol ";"


blockParser : Parser Statement
blockParser =
    sequence
        { start = "{"
        , item = statement
        , separator = ""
        , end = "}"
        , trailing = Optional
        }
        |> ParserWithContext.map Block


defParser : Parser Statement
defParser =
    succeed
        (\type_ var val ->
            Decl type_ var val
        )
        |= typeParser
        |= node identifierParser
        |= oneOf
            [ succeed Just
                |. symbol "="
                |= expression
            , succeed Nothing
            ]
        |. symbol ";"


expression : Parser (Node Expression)
expression =
    prec17Parser
        |> inContext ParsingExpression


prec17Parser : Parser (Node Expression)
prec17Parser =
    multiSequenceAssocLeft
        { separators = [ ( Comma, symbol "," ) ]
        , item = prec16Parser
        }


prec16Parser : Parser (Node Expression)
prec16Parser =
    prec15Parser
        |> ParserWithContext.andThen
            (\l ->
                oneOf
                    [ succeed (BinaryOperation l)
                        |= node
                            (oneOf
                                [ succeed Assign |. singleSymbol "="
                                , succeed ComboAdd |. symbol "+="
                                , succeed ComboSubtract |. symbol "-="
                                , succeed ComboBy |. symbol "*="
                                , succeed ComboDiv |. symbol "/="
                                , succeed ComboMod |. symbol "%="
                                , succeed ComboLeftShift |. symbol "<<="
                                , succeed ComboRightShift |. symbol ">>="
                                , succeed ComboBitwiseAnd |. symbol "&="
                                , succeed ComboBitwiseOr |. symbol "|="
                                , succeed ComboBitwiseXor |. symbol "^="
                                ]
                            )
                        |= ParserWithContext.lazy (\_ -> prec16Parser)
                        |> node
                    , succeed l
                    ]
            )


prec15Parser : Parser (Node Expression)
prec15Parser =
    prec14Parser
        |> ParserWithContext.andThen
            (\c ->
                oneOf
                    [ succeed (Ternary c)
                        |. symbol "?"
                        |= prec14Parser
                        |. symbol ":"
                        |= ParserWithContext.lazy (\_ -> prec15Parser)
                        |> node
                    , succeed c
                    ]
            )


prec14Parser : Parser (Node Expression)
prec14Parser =
    multiSequenceAssocLeft
        { separators = [ ( Or, symbol "||" ) ]
        , item = prec13Parser
        }


prec13Parser : Parser (Node Expression)
prec13Parser =
    multiSequenceAssocLeft
        { separators = [ ( Xor, symbol "^^" ) ]
        , item = prec12Parser
        }


prec12Parser : Parser (Node Expression)
prec12Parser =
    multiSequenceAssocLeft
        { separators = [ ( And, symbol "&&" ) ]
        , item = prec11Parser
        }


prec11Parser : Parser (Node Expression)
prec11Parser =
    multiSequenceAssocLeft
        { separators = [ ( BitwiseOr, singleSymbol "|" ) ]
        , item = prec10Parser
        }


prec10Parser : Parser (Node Expression)
prec10Parser =
    multiSequenceAssocLeft
        { separators = [ ( BitwiseXor, singleSymbol "^" ) ]
        , item = prec9Parser
        }


prec9Parser : Parser (Node Expression)
prec9Parser =
    multiSequenceAssocLeft
        { separators = [ ( BitwiseAnd, singleSymbol "&" ) ]
        , item = prec8Parser
        }


singleSymbol : String -> Parser ()
singleSymbol s =
    symbolNotFollowedBy s [ s ]


symbolNotFollowedBy : String -> List String -> Parser ()
symbolNotFollowedBy s nots =
    succeed ()
        |. ParserWithContext.backtrackable (symbol s)
        |. oneOf
            [ succeed ()
                |. oneOf (List.map symbol nots)
                |. ParserWithContext.problem ("Expecting " ++ s ++ " not follwed by any of " ++ String.join ", " nots)
                |> ParserWithContext.backtrackable
            , succeed ()
            ]


prec8Parser : Parser (Node Expression)
prec8Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( RelationOperation Equals, symbol "==" )
            , ( RelationOperation NotEquals, symbol "!=" )
            ]
        , item = prec7Parser
        }


prec7Parser : Parser (Node Expression)
prec7Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( RelationOperation LessThanOrEquals, symbol "<=" )
            , ( RelationOperation GreaterThanOrEquals, symbol ">=" )
            , ( RelationOperation LessThan, symbolNotFollowedBy "<" [ "<=" ] )
            , ( RelationOperation GreaterThan, symbolNotFollowedBy ">" [ ">=" ] )
            ]
        , item = prec6Parser
        }


prec6Parser : Parser (Node Expression)
prec6Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( ShiftLeft, symbolNotFollowedBy "<<" [ "=" ] )
            , ( ShiftRight, symbolNotFollowedBy ">>" [ "=" ] )
            ]
        , item = prec5Parser
        }


prec5Parser : Parser (Node Expression)
prec5Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( Add, symbolNotFollowedBy "+" [ "=" ] )
            , ( Subtract, symbolNotFollowedBy "-" [ "=" ] )
            ]
        , item = prec4Parser
        }


prec4Parser : Parser (Node Expression)
prec4Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( By, symbolNotFollowedBy "*" [ "=" ] )
            , ( Div, symbolNotFollowedBy "/" [ "=" ] )
            , ( Mod, symbolNotFollowedBy "%" [ "=" ] )
            ]
        , item = prec3Parser
        }


prec3Parser : Parser (Node Expression)
prec3Parser =
    oneOf
        [ succeed UnaryOperation
            |= node (succeed PrefixIncrement |. symbol "++")
            |= ParserWithContext.lazy (\_ -> prec3Parser)
            |> node
        , succeed UnaryOperation
            |= node (succeed Plus |. singleSymbol "+")
            |= ParserWithContext.lazy (\_ -> prec3Parser)
            |> node
        , succeed UnaryOperation
            |= node (succeed PrefixDecrement |. symbol "--")
            |= ParserWithContext.lazy (\_ -> prec3Parser)
            |> node
        , succeed
            (\op c ->
                case Node.value c of
                    Float f ->
                        Float -f

                    Int i ->
                        Int -i

                    _ ->
                        UnaryOperation op c
            )
            |= node (succeed Negate |. singleSymbol "-")
            |= ParserWithContext.lazy (\_ -> prec3Parser)
            |> node
        , succeed UnaryOperation
            |= node (succeed Invert |. symbol "~")
            |= ParserWithContext.lazy (\_ -> prec3Parser)
            |> node
        , succeed UnaryOperation
            |= node (succeed Not |. symbol "!")
            |= ParserWithContext.lazy (\_ -> prec3Parser)
            |> node
        , prec2Parser
        ]


prec2Parser : Parser (Node Expression)
prec2Parser =
    prec1Parser
        |> ParserWithContext.andThen
            (\a ->
                succeed (\f -> f a)
                    |= ParserWithContext.lazy (\_ -> prec2Suffixes)
            )


prec2Suffixes : Parser (Node Expression -> Node Expression)
prec2Suffixes =
    oneOf
        [ succeed (\args k v -> k (Node.combine Call v args))
            |= (sequence
                    { start = "("
                    , separator = ","
                    , item = ParserWithContext.lazy <| \_ -> prec16Parser
                    , end = ")"
                    , trailing = Forbidden
                    }
                    |> node
               )
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes
                , succeed identity
                ]
        , succeed
            (\start arg end k v ->
                k
                    (Node.combine3
                        BinaryOperation
                        v
                        (Node
                            { start = start
                            , end = end
                            }
                            ArraySubscript
                        )
                        arg
                    )
            )
            |= location
            |. symbol "["
            |= ParserWithContext.lazy (\_ -> prec16Parser)
            |. symbol "]"
            |= location
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes
                , succeed identity
                ]
        , succeed (\p k v -> k (Node.combine Dot v p))
            |. symbol "."
            |= node identifierParser
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes
                , succeed identity
                ]
        , succeed (\op k v -> k (Node.combine UnaryOperation op v))
            |= node (succeed PostfixIncrement |. symbol "++")
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes
                , succeed identity
                ]
        , succeed (\op k v -> k (Node.combine UnaryOperation op v))
            |= node (succeed PostfixDecrement |. symbol "--")
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes
                , succeed identity
                ]
        , succeed identity
        ]


prec1Parser : Parser (Node Expression)
prec1Parser =
    oneOf
        [ succeed Parens
            |. symbol "("
            |= ParserWithContext.lazy (\_ -> expression)
            |. symbol ")"
        , succeed (Bool True)
            |. keyword "true"
        , succeed (Bool False)
            |. keyword "false"
        , succeed Variable
            |= identifierParser
        , succeed Float
            |= float
        , succeed Int
            |= int
        ]
        |> node


type alias SequenceData =
    { separators : List ( BinaryOperation, Parser () )
    , item : Parser (Node Expression)
    }


multiSequenceAssocLeft : SequenceData -> Parser (Node Expression)
multiSequenceAssocLeft data =
    succeed identity
        |= data.item
        |> ParserWithContext.andThen
            (\first ->
                loop first (\expr -> multiSequenceHelpLeft data expr)
            )


multiSequenceHelpLeft :
    SequenceData
    -> Node Expression
    -> Parser (Step (Node Expression) (Node Expression))
multiSequenceHelpLeft { separators, item } acc =
    let
        separated : Parser (Step (Node Expression) a)
        separated =
            separators
                |> List.map
                    (\( f, parser ) ->
                        succeed (\op e -> BinaryOperation acc op e)
                            |= node (succeed f |. parser)
                            |= item
                            |> node
                            |> ParserWithContext.map Loop
                    )
                |> oneOf
    in
    oneOf
        [ separated
        , succeed (Done acc)
        ]
