module Glsl.PrettyPrinter exposing (binaryOperation, declaration, expr, float, stat, type_, unaryOperation)

import Glsl exposing (BinaryOperation(..), Declaration(..), Expression(..), RelationOperation(..), Statement(..), Type(..), UnaryOperation(..))
import Glsl.Node as Node exposing (Node(..))


stat : Int -> Node Statement -> String
stat i (Node _ c) =
    case c of
        Block [] ->
            "{}"

        Block children ->
            (indent i "{"
                :: List.map (stat (i + 1)) children
                ++ [ indent i "}" ]
            )
                |> String.join "\n"

        If cond t ->
            indent i ("if (" ++ expr cond ++ ") ") ++ String.trimLeft (stat (i + 1) t)

        IfElse cond t ((Node _ (If _ _)) as f) ->
            [ indent i ("if (" ++ expr cond ++ ") {")
            , stat (i + 1) t
            , indent i <| "} else " ++ String.trimLeft (stat i f)
            ]
                |> String.join "\n"

        IfElse cond t ((Node _ (IfElse _ _ _)) as f) ->
            [ indent i ("if (" ++ expr cond ++ ") {")
            , stat (i + 1) t
            , indent i "} else {"
            , stat (i + 1) f
            , indent i "}"
            ]
                |> String.join "\n"

        IfElse cond t f ->
            [ indent i ("if (" ++ expr cond ++ ") {")
            , stat (i + 1) t
            , indent i "} else {"
            , stat (i + 1) f
            , indent i "}"
            ]
                |> String.join "\n"

        For init check step loop ->
            let
                initString : String
                initString =
                    case init of
                        Nothing ->
                            ";"

                        Just s ->
                            stat 0 s
            in
            [ indent i ("for ( " ++ initString ++ " " ++ expr check ++ "; " ++ expr step ++ ") {")
            , stat (i + 1) loop
            , indent i "}"
            ]
                |> String.join "\n"

        Return e ->
            indent i <| "return " ++ expr e ++ ";"

        Break ->
            indent i "break;"

        Continue ->
            indent i "continue;"

        ExpressionStatement e ->
            indent i (expr e ++ ";")

        Decl t (Node _ n) (Just e) ->
            indent i (type_ t ++ " " ++ n ++ " = " ++ expr e ++ ";")

        Decl t (Node _ n) Nothing ->
            indent i (type_ t ++ " " ++ n ++ ";")


indent : Int -> String -> String
indent i line =
    String.repeat (4 * i) " " ++ line


expr : Node Expression -> String
expr root =
    let
        showParen : Bool -> String -> String
        showParen show e =
            if show then
                "(" ++ e ++ ")"

            else
                e

        infixl_ : Int -> Int -> Node Expression -> String -> Node Expression -> String
        infixl_ n p l op r =
            showParen (p > n) (go n l ++ " " ++ op ++ " " ++ go (n + 1) r)

        infixr_ : Int -> Int -> Node Expression -> String -> Node Expression -> String
        infixr_ n p l op r =
            showParen (p > n) (go (n + 1) l ++ " " ++ op ++ " " ++ go n r)

        go : Int -> Node Expression -> String
        go p (Node _ tree) =
            case tree of
                Bool b ->
                    if b then
                        "true"

                    else
                        "false"

                Float f ->
                    float f

                Int i ->
                    String.fromInt i

                Variable v ->
                    v

                BinaryOperation l (Node _ ArraySubscript) r ->
                    showParen (p > 15) (go 15 l ++ "[" ++ go 16 r ++ "]")

                Call l (Node _ r) ->
                    showParen (p > 15) (go 15 l ++ "(" ++ String.join ", " (List.map (go 16) r) ++ ")")

                Dot l (Node _ r) ->
                    showParen (p > 15) (go 15 l ++ "." ++ r)

                UnaryOperation (Node _ PostfixIncrement) r ->
                    showParen (p > 15) (go 16 r ++ "++")

                UnaryOperation (Node _ PostfixDecrement) r ->
                    showParen (p > 15) (go 16 r ++ "--")

                UnaryOperation (Node _ PrefixIncrement) r ->
                    showParen (p > 14) ("++" ++ go 15 r)

                UnaryOperation (Node _ PrefixDecrement) r ->
                    showParen (p > 14) ("--" ++ go 15 r)

                UnaryOperation (Node _ Plus) r ->
                    showParen (p > 14) ("+" ++ go 15 r)

                UnaryOperation (Node _ Negate) r ->
                    showParen (p > 14) ("-" ++ go 15 r)

                UnaryOperation (Node _ Invert) r ->
                    showParen (p > 14) ("~" ++ go 15 r)

                UnaryOperation (Node _ Not) r ->
                    showParen (p > 14) ("!" ++ go 15 r)

                BinaryOperation l (Node _ By) r ->
                    infixl_ 13 p l "*" r

                BinaryOperation l (Node _ Div) r ->
                    infixl_ 13 p l "/" r

                BinaryOperation l (Node _ Mod) r ->
                    infixl_ 13 p l "%" r

                BinaryOperation l (Node _ Add) r ->
                    infixl_ 12 p l "+" r

                BinaryOperation l (Node _ Subtract) r ->
                    infixl_ 12 p l "-" r

                BinaryOperation l (Node _ ShiftLeft) r ->
                    infixl_ 11 p l "<<" r

                BinaryOperation l (Node _ ShiftRight) r ->
                    infixl_ 11 p l ">>" r

                BinaryOperation l (Node _ (RelationOperation LessThan)) r ->
                    infixl_ 10 p l "<" r

                BinaryOperation l (Node _ (RelationOperation LessThanOrEquals)) r ->
                    infixl_ 10 p l "<=" r

                BinaryOperation l (Node _ (RelationOperation GreaterThan)) r ->
                    infixl_ 10 p l ">" r

                BinaryOperation l (Node _ (RelationOperation GreaterThanOrEquals)) r ->
                    infixl_ 10 p l ">=" r

                BinaryOperation l (Node _ (RelationOperation Equals)) r ->
                    infixl_ 9 p l "==" r

                BinaryOperation l (Node _ (RelationOperation NotEquals)) r ->
                    infixl_ 9 p l "!=" r

                BinaryOperation l (Node _ BitwiseAnd) r ->
                    infixl_ 8 p l "&" r

                BinaryOperation l (Node _ BitwiseXor) r ->
                    infixl_ 7 p l "^" r

                BinaryOperation l (Node _ BitwiseOr) r ->
                    infixl_ 6 p l "|" r

                BinaryOperation l (Node _ And) r ->
                    infixl_ 5 p l "&&" r

                BinaryOperation l (Node _ Xor) r ->
                    infixl_ 4 p l "^^" r

                BinaryOperation l (Node _ Or) r ->
                    infixl_ 3 p l "||" r

                Ternary c t f ->
                    showParen (p > 2) (go 3 c ++ " ? " ++ go 3 t ++ " : " ++ go 2 f)

                BinaryOperation l (Node _ Assign) r ->
                    infixr_ 1 p l "=" r

                BinaryOperation l (Node _ ComboAdd) r ->
                    infixr_ 1 p l "+=" r

                BinaryOperation l (Node _ ComboSubtract) r ->
                    infixr_ 1 p l "-=" r

                BinaryOperation l (Node _ ComboBy) r ->
                    infixr_ 1 p l "*=" r

                BinaryOperation l (Node _ ComboDiv) r ->
                    infixr_ 1 p l "/=" r

                BinaryOperation l (Node _ ComboMod) r ->
                    infixr_ 1 p l "%=" r

                BinaryOperation l (Node _ ComboLeftShift) r ->
                    infixr_ 1 p l "<<=" r

                BinaryOperation l (Node _ ComboRightShift) r ->
                    infixr_ 1 p l ">>=" r

                BinaryOperation l (Node _ ComboBitwiseAnd) r ->
                    infixr_ 1 p l "&=" r

                BinaryOperation l (Node _ ComboBitwiseXor) r ->
                    infixr_ 1 p l "^=" r

                BinaryOperation l (Node _ ComboBitwiseOr) r ->
                    infixr_ 1 p l "|=" r

                BinaryOperation l (Node _ Comma) r ->
                    infixl_ 0 p l "," r

                Parens c ->
                    "(" ++ go 0 c ++ ")"
    in
    go 0 root


binaryOperation : BinaryOperation -> String
binaryOperation op =
    case op of
        By ->
            "*"

        Div ->
            "/"

        Mod ->
            "%"

        Add ->
            "+"

        Subtract ->
            "-"

        ShiftLeft ->
            "<<"

        ShiftRight ->
            ">>"

        RelationOperation LessThan ->
            "<"

        RelationOperation LessThanOrEquals ->
            "<="

        RelationOperation GreaterThan ->
            ">"

        RelationOperation GreaterThanOrEquals ->
            ">="

        RelationOperation Equals ->
            "=="

        RelationOperation NotEquals ->
            "!="

        BitwiseAnd ->
            "&"

        BitwiseXor ->
            "^"

        BitwiseOr ->
            "|"

        And ->
            "&&"

        Xor ->
            "^^"

        Or ->
            "||"

        Assign ->
            "="

        ComboAdd ->
            "+="

        ComboSubtract ->
            "-="

        ComboBy ->
            "*="

        ComboDiv ->
            "/="

        ComboMod ->
            "%="

        ComboLeftShift ->
            "<<="

        ComboRightShift ->
            ">>="

        ComboBitwiseAnd ->
            "&="

        ComboBitwiseXor ->
            "^="

        ComboBitwiseOr ->
            "|="

        Comma ->
            ","

        ArraySubscript ->
            "[]"


type_ : Node Type -> String
type_ (Node _ t) =
    case t of
        Tvoid ->
            "void"

        Tin tt ->
            "in " ++ type_ tt

        Tout tt ->
            "out " ++ type_ tt

        Tbool ->
            "bool"

        Tbvec2 ->
            "bvec2"

        Tbvec3 ->
            "bvec3"

        Tbvec4 ->
            "bvec4"

        Tint ->
            "int"

        Tivec2 ->
            "ivec2"

        Tivec3 ->
            "ivec3"

        Tivec4 ->
            "ivec4"

        Tfloat ->
            "float"

        Tvec2 ->
            "vec2"

        Tvec3 ->
            "vec3"

        Tvec4 ->
            "vec4"

        Tmat2 ->
            "mat2"

        Tmat3 ->
            "mat3"

        Tmat4 ->
            "mat4"

        Tsampler1D ->
            "sampler1D"

        Tsampler2D ->
            "sampler2D"

        Tsampler3D ->
            "sampler3D"

        TsamplerCube ->
            "samplerCube"

        Tuint ->
            "uint"

        Tuvec2 ->
            "uvec2"

        Tuvec3 ->
            "uvec3"

        Tuvec4 ->
            "uvec4"

        Tmat2x2 ->
            "mat2x2"

        Tmat2x3 ->
            "mat2x3"

        Tmat2x4 ->
            "mat2x4"

        Tmat3x2 ->
            "mat3x2"

        Tmat3x3 ->
            "mat3x3"

        Tmat3x4 ->
            "mat3x4"

        Tmat4x2 ->
            "mat4x2"

        Tmat4x3 ->
            "mat4x3"

        Tmat4x4 ->
            "mat4x4"

        Tsampler2DRect ->
            "sampler2DRect"

        Tsampler1DShadow ->
            "sampler1DShadow"

        Tsampler2DShadow ->
            "sampler2DShadow"

        Tsampler2DRectShadow ->
            "sampler2DRectShadow"

        Tsampler1DArray ->
            "sampler1DArray"

        Tsampler2DArray ->
            "sampler2DArray"

        Tsampler1DArrayShadow ->
            "sampler1DArrayShadow"

        Tsampler2DArrayShadow ->
            "sampler2DArrayShadow"

        TsamplerBuffer ->
            "samplerBuffer"

        Tsampler2DMS ->
            "sampler2DMS"

        Tsampler2DMSArray ->
            "sampler2DMSArray"

        Tisampler1D ->
            "isampler1D"

        Tisampler2D ->
            "isampler2D"

        Tisampler3D ->
            "isampler3D"

        TisamplerCube ->
            "isamplerCube"

        Tisampler2DRect ->
            "isampler2DRect"

        Tisampler1DArray ->
            "isampler1DArray"

        Tisampler2DArray ->
            "isampler2DArray"

        TisamplerBuffer ->
            "isamplerBuffer"

        Tisampler2DMS ->
            "isampler2DMS"

        Tisampler2DMSArray ->
            "isampler2DMSArray"

        Tusampler1D ->
            "usampler1D"

        Tusampler2D ->
            "usampler2D"

        Tusampler3D ->
            "usampler3D"

        TusamplerCube ->
            "usamplerCube"

        Tusampler2DRect ->
            "usampler2DRect"

        Tusampler1DArray ->
            "usampler1DArray"

        Tusampler2DArray ->
            "usampler2DArray"

        TusamplerBuffer ->
            "usamplerBuffer"

        Tusampler2DMS ->
            "usampler2DMS"

        Tusampler2DMSArray ->
            "usampler2DMSArray"

        Tdouble ->
            "double"

        Tdvec2 ->
            "dvec2"

        Tdvec3 ->
            "dvec3"

        Tdvec4 ->
            "dvec4"

        Tdmat2 ->
            "dmat2"

        Tdmat3 ->
            "dmat3"

        Tdmat4 ->
            "dmat4"

        Tdmat2x2 ->
            "dmat2x2"

        Tdmat2x3 ->
            "dmat2x3"

        Tdmat2x4 ->
            "dmat2x4"

        Tdmat3x2 ->
            "dmat3x2"

        Tdmat3x3 ->
            "dmat3x3"

        Tdmat3x4 ->
            "dmat3x4"

        Tdmat4x2 ->
            "dmat4x2"

        Tdmat4x3 ->
            "dmat4x3"

        Tdmat4x4 ->
            "dmat4x4"

        Ttexture1D ->
            "texture1D"

        Timage1D ->
            "image1D"

        Ttexture1DArray ->
            "texture1DArray"

        Timage1DArray ->
            "image1DArray"

        Ttexture2D ->
            "texture2D"

        Timage2D ->
            "image2D"

        Ttexture2DArray ->
            "texture2DArray"

        Timage2DArray ->
            "image2DArray"

        Ttexture2DMS ->
            "texture2DMS"

        Timage2DMS ->
            "image2DMS"

        Ttexture2DMSArray ->
            "texture2DMSArray"

        Timage2DMSArray ->
            "image2DMSArray"

        Ttexture2DRect ->
            "texture2DRect"

        Timage2DRect ->
            "image2DRect"

        Ttexture3D ->
            "texture3D"

        Timage3D ->
            "image3D"

        TtextureCube ->
            "textureCube"

        TimageCube ->
            "imageCube"

        TsamplerCubeShadow ->
            "samplerCubeShadow"

        TsamplerCubeArray ->
            "samplerCubeArray"

        TtextureCubeArray ->
            "textureCubeArray"

        TimageCubeArray ->
            "imageCubeArray"

        TsamplerCubeArrayShadow ->
            "samplerCubeArrayShadow"

        TtextureBuffer ->
            "textureBuffer"

        TimageBuffer ->
            "imageBuffer"

        TsubpassInput ->
            "subpassInput"

        TsubpassInputMS ->
            "subpassInputMS"

        Titexture1DArray ->
            "itexture1DArray"

        Tiimage1DArray ->
            "iimage1DArray"

        Titexture2D ->
            "itexture2D"

        Tiimage2D ->
            "iimage2D"

        Titexture2DArray ->
            "itexture2DArray"

        Tiimage2DArray ->
            "iimage2DArray"

        Titexture2DMS ->
            "itexture2DMS"

        Tiimage2DMS ->
            "iimage2DMS"

        Titexture2DMSArray ->
            "itexture2DMSArray"

        Tiimage2DMSArray ->
            "iimage2DMSArray"

        Titexture2DRect ->
            "itexture2DRect"

        Tiimage2DRect ->
            "iimage2DRect"

        Titexture3D ->
            "itexture3D"

        Tiimage3D ->
            "iimage3D"

        TitextureCube ->
            "itextureCube"

        TiimageCube ->
            "iimageCube"

        TisamplerCubeArray ->
            "isamplerCubeArray"

        TitextureCubeArray ->
            "itextureCubeArray"

        TiimageCubeArray ->
            "iimageCubeArray"

        TitextureBuffer ->
            "itextureBuffer"

        TiimageBuffer ->
            "iimageBuffer"

        TisubpassInput ->
            "isubpassInput"

        TisubpassInputMS ->
            "isubpassInputMS"

        Tutexture1DArray ->
            "utexture1DArray"

        Tuimage1DArray ->
            "uimage1DArray"

        Tutexture2D ->
            "utexture2D"

        Tuimage2D ->
            "uimage2D"

        Tutexture2DArray ->
            "utexture2DArray"

        Tuimage2DArray ->
            "uimage2DArray"

        Tutexture2DMS ->
            "utexture2DMS"

        Tuimage2DMS ->
            "uimage2DMS"

        Tutexture2DMSArray ->
            "utexture2DMSArray"

        Tuimage2DMSArray ->
            "uimage2DMSArray"

        Tutexture2DRect ->
            "utexture2DRect"

        Tuimage2DRect ->
            "uimage2DRect"

        Tutexture3D ->
            "utexture3D"

        Tuimage3D ->
            "uimage3D"

        TutextureCube ->
            "utextureCube"

        TuimageCube ->
            "uimageCube"

        TusamplerCubeArray ->
            "usamplerCubeArray"

        TutextureCubeArray ->
            "utextureCubeArray"

        TuimageCubeArray ->
            "uimageCubeArray"

        TutextureBuffer ->
            "utextureBuffer"

        TuimageBuffer ->
            "uimageBuffer"

        TusubpassInput ->
            "usubpassInput"

        TusubpassInputMS ->
            "usubpassInputMS"

        Tsampler ->
            "sampler"

        TsamplerShadow ->
            "samplerShadow"


float : Float -> String
float f =
    if isNaN f then
        "(0./0.)"

    else if isInfinite f then
        if f > 0 then
            "(1./0.)"

        else
            "(-1./0.)"

    else
        let
            s : String
            s =
                String.fromFloat f
        in
        if String.contains "." s || String.contains "e" s then
            s

        else
            s ++ "."


unaryOperation : UnaryOperation -> String
unaryOperation op =
    case op of
        Negate ->
            "-"

        PostfixIncrement ->
            "++"

        PostfixDecrement ->
            "--"

        PrefixIncrement ->
            "(++)"

        PrefixDecrement ->
            "(--)"

        Plus ->
            "+"

        Invert ->
            "~"

        Not ->
            "!"


declaration : Node Declaration -> String
declaration (Node _ decl) =
    case decl of
        ConstDeclaration tipe (Node _ name) value ->
            "const " ++ type_ tipe ++ " " ++ name ++ " = " ++ expr value ++ ";"

        FunctionDeclaration returnType (Node _ name) (Node _ args) statements ->
            let
                argsString : String
                argsString =
                    args
                        |> List.map (\( argType, Node _ argName ) -> type_ argType ++ " " ++ argName)
                        |> String.join ", "

                head : String
                head =
                    type_ returnType ++ " " ++ name ++ "(" ++ argsString ++ ") "
            in
            case statements of
                [] ->
                    head ++ "{}"

                _ ->
                    head ++ stat 0 (Node.empty (Block statements))

        UniformDeclaration tipe (Node _ name) ->
            "uniform " ++ type_ tipe ++ " " ++ name ++ ";"
