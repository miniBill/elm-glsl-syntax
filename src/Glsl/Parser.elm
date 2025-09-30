module Glsl.Parser exposing (Context(..), DeadEnd, Parser, expression, file, function, statement)

import Glsl exposing (BinaryOperation(..), Declaration(..), Expression(..), RelationOperation(..), Statement(..), Type(..), UnaryOperation(..))
import Parser exposing (Problem(..))
import Parser.Advanced exposing ((|.), (|=), Step(..), Trailing(..))
import ParserWithContext exposing (chompIf, chompWhile, end, float, getChompedString, inContext, int, keyword, loop, many, oneOf, sequence, spaces, succeed, symbol)


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


file : Parser ( Maybe { version : Int }, List Declaration )
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
            )
        |. end
        |> inContext ParsingFile


function : Parser Declaration
function =
    succeed FunctionDeclaration
        |= typeParser
        |= identifierParser
        |= sequence
            { start = "("
            , end = ")"
            , separator = ","
            , item = argParser
            , trailing = Forbidden
            }
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
        |= identifierParser
        |. symbol "="
        |= expression
        |. symbol ";"


uniform : Parser Declaration
uniform =
    succeed UniformDeclaration
        |. keyword "uniform"
        |= typeParser
        |= identifierParser
        |. symbol ";"


argParser : Parser ( Type, String )
argParser =
    succeed Tuple.pair
        |= typeParser
        |= identifierParser


identifierParser : Parser String
identifierParser =
    getChompedString
        (succeed ()
            |. chompIf (\c -> Char.isAlpha c || c == '_') (Expecting "Letter or underscore")
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')
        )
        |. spaces


typeParser : Parser Type
typeParser =
    let
        baseParser : Parser Type
        baseParser =
            types
                |> List.map (\( s, t ) -> succeed t |. keyword s)
                |> oneOf
    in
    succeed identity
        |. oneOf
            [ keyword "const"
            , succeed ()
            ]
        |= oneOf
            [ succeed TOut
                |. keyword "out"
                |= baseParser
            , succeed TIn
                |. keyword "in"
                |= baseParser
            , baseParser
            ]


types : List ( String, Type )
types =
    [ ( "void", TVoid )
    , ( "bool", TBool )
    , ( "bvec2", TBVec2 )
    , ( "bvec3", TBVec3 )
    , ( "bvec4", TBVec4 )
    , ( "int", TInt )
    , ( "ivec2", TIVec2 )
    , ( "ivec3", TIVec3 )
    , ( "ivec4", TIVec4 )
    , ( "uint", TUint )
    , ( "uvec2", TUVec2 )
    , ( "uvec3", TUVec3 )
    , ( "uvec4", TUVec4 )
    , ( "float", TFloat )
    , ( "vec2", TVec2 )
    , ( "vec3", TVec3 )
    , ( "vec4", TVec4 )
    , ( "double", TDouble )
    , ( "dvec2", TDVec2 )
    , ( "dvec3", TDVec3 )
    , ( "dvec4", TDVec4 )
    , ( "mat2", TMat2 )
    , ( "mat2x2", TMat2 )
    , ( "mat3", TMat3 )
    , ( "mat3x3", TMat3 )
    , ( "mat4", TMat4 )
    , ( "mat4x4", TMat4 )
    , ( "mat2x3", TMat23 )
    , ( "mat2x4", TMat24 )
    , ( "mat3x2", TMat32 )
    , ( "mat3x4", TMat34 )
    , ( "mat4x2", TMat42 )
    , ( "mat4x3", TMat43 )
    , ( "dmat2", TDMat2 )
    , ( "dmat2x2", TDMat2 )
    , ( "dmat3", TDMat3 )
    , ( "dmat3x3", TDMat3 )
    , ( "dmat4", TDMat4 )
    , ( "dmat4x4", TDMat4 )
    , ( "dmat2x3", TDMat23 )
    , ( "dmat2x4", TDMat24 )
    , ( "dmat3x2", TDMat32 )
    , ( "dmat3x4", TDMat34 )
    , ( "dmat4x2", TDMat42 )
    , ( "dmat4x3", TDMat43 )
    , ( "sampler1d", TSampler1D )
    , ( "image1d", TImage1D )
    , ( "sampler2d", TSampler2D )
    , ( "image2d", TImage2D )
    , ( "sampler3d", TSampler3D )
    , ( "image3d", TImage3D )
    , ( "samplercube", TSamplerCube )
    , ( "imagecube", TImageCube )
    , ( "sampler2drect", TSampler2DRect )
    , ( "image2drect", TImage2DRect )
    , ( "sampler1darray", TSampler1DArray )
    , ( "image1darray", TImage1DArray )
    , ( "sampler2darray", TSampler2DArray )
    , ( "image2darray", TImage2DArray )
    , ( "samplerbuffer", TSamplerBuffer )
    , ( "imagebuffer", TImageBuffer )
    , ( "sampler2dms", TSampler2DMS )
    , ( "image2dms", TImage2DMS )
    , ( "sampler2dmsarray", TSampler2DMSArray )
    , ( "image2dmsarray", TImage2DMSArray )
    , ( "samplercubearray", TSamplerCubeArray )
    , ( "imagecubearray", TImageCubeArray )
    , ( "sampler1dshadow", TSampler1DShadow )
    , ( "sampler2dshadow", TSampler2DShadow )
    , ( "sampler2drectshadow", TSampler2DRectShadow )
    , ( "sampler1darrayshadow", TSampler1DArrayShadow )
    , ( "sampler2darrayshadow", TSampler2DArrayShadow )
    , ( "samplercubeshadow", TSamplerCubeShadow )
    , ( "samplercubearrayshadow", TSamplerCubeArrayShadow )
    ]


statement : Parser Statement
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
        |= identifierParser
        |= oneOf
            [ succeed Just
                |. symbol "="
                |= expression
            , succeed Nothing
            ]
        |. symbol ";"


expression : Parser Expression
expression =
    prec17Parser
        |> inContext ParsingExpression


prec17Parser : Parser Expression
prec17Parser =
    multiSequenceAssocLeft
        { separators = [ ( \l r -> BinaryOperation l Comma r, symbol "," ) ]
        , item = prec16Parser
        }


prec16Parser : Parser Expression
prec16Parser =
    succeed (\a f -> f a)
        |= prec15Parser
        |= oneOf
            [ succeed (\r l -> BinaryOperation l Assign r)
                |. singleSymbol "="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboAdd r)
                |. symbol "+="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboSubtract r)
                |. symbol "-="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBy r)
                |. symbol "*="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboDiv r)
                |. symbol "/="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboMod r)
                |. symbol "%="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboLeftShift r)
                |. symbol "<<="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboRightShift r)
                |. symbol ">>="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseAnd r)
                |. symbol "&="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseOr r)
                |. symbol "|="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed (\r l -> BinaryOperation l ComboBitwiseXor r)
                |. symbol "^="
                |= ParserWithContext.lazy (\_ -> prec16Parser)
            , succeed identity
            ]


prec15Parser : Parser Expression
prec15Parser =
    succeed (\k f -> f k)
        |= prec14Parser
        |= oneOf
            [ succeed (\t f c -> Ternary c t f)
                -- c is passed in last in the lambda because it's passed
                -- from above
                |. symbol "?"
                |= prec14Parser
                |. symbol ":"
                |= ParserWithContext.lazy (\_ -> prec15Parser)
            , succeed identity
            ]


prec14Parser : Parser Expression
prec14Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l Or r, symbol "||" )
            ]
        , item = prec13Parser
        }


prec13Parser : Parser Expression
prec13Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l Xor r, symbol "^^" )
            ]
        , item = prec12Parser
        }


prec12Parser : Parser Expression
prec12Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l And r, symbol "&&" )
            ]
        , item = prec11Parser
        }


prec11Parser : Parser Expression
prec11Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l BitwiseOr r, singleSymbol "|" )
            ]
        , item = prec10Parser
        }


prec10Parser : Parser Expression
prec10Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l BitwiseXor r, singleSymbol "^" )
            ]
        , item = prec9Parser
        }


prec9Parser : Parser Expression
prec9Parser =
    multiSequenceAssocLeft
        { separators = [ ( \l r -> BinaryOperation l BitwiseAnd r, singleSymbol "&" ) ]
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


prec8Parser : Parser Expression
prec8Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l (RelationOperation Equals) r, symbol "==" )
            , ( \l r -> BinaryOperation l (RelationOperation NotEquals) r, symbol "!=" )
            ]
        , item = prec7Parser
        }


prec7Parser : Parser Expression
prec7Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l (RelationOperation LessThanOrEquals) r, symbol "<=" )
            , ( \l r -> BinaryOperation l (RelationOperation GreaterThanOrEquals) r, symbol ">=" )
            , ( \l r -> BinaryOperation l (RelationOperation LessThan) r, symbolNotFollowedBy "<" [ "<=" ] )
            , ( \l r -> BinaryOperation l (RelationOperation GreaterThan) r, symbolNotFollowedBy ">" [ ">=" ] )
            ]
        , item = prec6Parser
        }


prec6Parser : Parser Expression
prec6Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l ShiftLeft r, symbolNotFollowedBy "<<" [ "=" ] )
            , ( \l r -> BinaryOperation l ShiftRight r, symbolNotFollowedBy ">>" [ "=" ] )
            ]
        , item = prec5Parser
        }


prec5Parser : Parser Expression
prec5Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l Add r, symbolNotFollowedBy "+" [ "=" ] )
            , ( \l r -> BinaryOperation l Subtract r, symbolNotFollowedBy "-" [ "=" ] )
            ]
        , item = prec4Parser
        }


prec4Parser : Parser Expression
prec4Parser =
    multiSequenceAssocLeft
        { separators =
            [ ( \l r -> BinaryOperation l By r, symbolNotFollowedBy "*" [ "=" ] )
            , ( \l r -> BinaryOperation l Div r, symbolNotFollowedBy "/" [ "=" ] )
            , ( \l r -> BinaryOperation l Mod r, symbolNotFollowedBy "%" [ "=" ] )
            ]
        , item = prec3Parser
        }


prec3Parser : Parser Expression
prec3Parser =
    oneOf
        [ succeed (UnaryOperation PrefixIncrement)
            |. symbol "++"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Plus)
            |. singleSymbol "+"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation PrefixDecrement)
            |. symbol "--"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed
            (\c ->
                case c of
                    Float f ->
                        Float -f

                    Int i ->
                        Int -i

                    _ ->
                        UnaryOperation Negate c
            )
            |. singleSymbol "-"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Invert)
            |. symbol "~"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , succeed (UnaryOperation Not)
            |. symbol "!"
            |= ParserWithContext.lazy (\_ -> prec3Parser)
        , prec2Parser
        ]


prec2Parser : Parser Expression
prec2Parser =
    succeed (\a f -> f a)
        |= prec1Parser
        |= ParserWithContext.lazy prec2Suffixes


prec2Suffixes : () -> Parser (Expression -> Expression)
prec2Suffixes () =
    oneOf
        [ succeed (\args k v -> k (Call v args))
            |= sequence
                { start = "("
                , separator = ","
                , item = ParserWithContext.lazy <| \_ -> prec16Parser
                , end = ")"
                , trailing = Forbidden
                }
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\arg k v -> k (BinaryOperation v ArraySubscript arg))
            |. symbol "["
            |= ParserWithContext.lazy (\_ -> prec16Parser)
            |. symbol "]"
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\p k v -> k (Dot v p))
            |. symbol "."
            |= identifierParser
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\k v -> k (UnaryOperation PostfixIncrement v))
            |. symbol "++"
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed (\k v -> k (UnaryOperation PostfixDecrement v))
            |. symbol "--"
            |= oneOf
                [ ParserWithContext.lazy <| \_ -> prec2Suffixes ()
                , succeed identity
                ]
        , succeed identity
        ]


prec1Parser : Parser Expression
prec1Parser =
    oneOf
        [ succeed identity
            |. symbol "("
            |= ParserWithContext.lazy (\_ -> expression)
            |. symbol ")"
        , succeed (Bool True)
            |. keyword "true"
        , succeed (Bool False)
            |. keyword "false"
        , succeed Variable
            |= identifierParser
        , succeed Float |= float
        , succeed Int |= int
        ]


type alias SequenceData =
    { separators : List ( Expression -> Expression -> Expression, Parser () )
    , item : Parser Expression
    }


multiSequenceAssocLeft : SequenceData -> Parser Expression
multiSequenceAssocLeft data =
    succeed identity
        |= data.item
        |> ParserWithContext.andThen
            (\first ->
                loop first (\expr -> multiSequenceHelpLeft data expr)
            )


multiSequenceHelpLeft :
    SequenceData
    -> Expression
    -> Parser (Step Expression Expression)
multiSequenceHelpLeft { separators, item } acc =
    let
        separated : Parser (Step Expression a)
        separated =
            separators
                |> List.map
                    (\( f, parser ) ->
                        succeed (\e -> Loop <| f acc e)
                            |. parser
                            |= item
                    )
                |> oneOf
    in
    oneOf
        [ separated
        , succeed (Done acc)
        ]
