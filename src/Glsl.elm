module Glsl exposing
    ( Declaration(..), Statement(..), Type(..)
    , Expression(..), BinaryOperation(..), UnaryOperation(..), RelationOperation(..)
    )

{-|


# Types

@docs Declaration, Statement, Type
@docs Expression, BinaryOperation, UnaryOperation, RelationOperation

-}


{-| -}
type Declaration
    = FunctionDeclaration Type String (List ( Type, String )) (List Statement)
    | UniformDeclaration Type String
    | ConstDeclaration Type String Expression


type Expression
    = Bool Bool
    | Int Int
    | Float Float
    | Uint Int
    | Double Float
    | Variable String
    | Ternary Expression Expression Expression
    | UnaryOperation UnaryOperation Expression
    | BinaryOperation Expression BinaryOperation Expression
    | Call Expression (List Expression)
    | Dot Expression String


type BinaryOperation
    = -- 2
      ArraySubscript
      -- 4
    | By
    | Div
    | Mod
    | -- 5
      Add
    | Subtract
      -- 6
    | ShiftLeft
    | ShiftRight
      -- 7 and 8
    | RelationOperation RelationOperation
      -- 9
    | BitwiseAnd
      -- 10
    | BitwiseOr
      -- 11
    | BitwiseXor
      -- 12
    | And
      -- 13
    | Xor
      -- 14
    | Or
      -- 16
    | Assign
    | ComboAdd
    | ComboSubtract
    | ComboBy
    | ComboDiv
    | ComboMod
    | ComboLeftShift
    | ComboRightShift
    | ComboBitwiseAnd
    | ComboBitwiseXor
    | ComboBitwiseOr
      -- 17
    | Comma


type RelationOperation
    = -- 7
      LessThan
    | GreaterThan
    | LessThanOrEquals
    | GreaterThanOrEquals
      -- 8
    | Equals
    | NotEquals


type UnaryOperation
    = -- 2
      PostfixIncrement
    | PostfixDecrement
      -- 3
    | PrefixIncrement
    | PrefixDecrement
    | Plus
    | Negate
    | Invert
    | Not



-- Typed statements


type Statement
    = If Expression Statement
    | IfElse Expression Statement Statement
    | For (Maybe Statement) Expression Expression Statement
    | Return Expression
    | Break
    | Continue
    | ExpressionStatement Expression
    | Decl Type String (Maybe Expression)
    | Nop
    | Block (List Statement)


type Type
    = --
      -- GLSL 1.20
      Tvoid
      -- bool
    | Tbool
    | Tbvec2
    | Tbvec3
    | Tbvec4
      -- int
    | Tint
    | Tivec2
    | Tivec3
    | Tivec4
      -- float
    | TFloat
    | Tvec2
    | Tvec3
    | Tvec4
      -- matrices
    | Tmat2
    | Tmat3
    | Tmat4
      -- samplers
    | Tsampler1D
    | Tsampler2D
    | Tsampler3D
    | TsamplerCube
      --
      -- GLSL 3.30
      -- uint
    | Tuint
    | Tuvec2
    | Tuvec3
    | Tuvec4
      -- matrix
    | Tmat2x2
    | Tmat2x3
    | Tmat2x4
    | Tmat3x2
    | Tmat3x3
    | Tmat3x4
    | Tmat4x2
    | Tmat4x3
    | Tmat4x4
      -- samplers
    | Tsampler2DRect
    | Tsampler1DShadow
    | Tsampler2DShadow
    | Tsampler2DRectShadow
    | Tsampler1DArray
    | Tsampler2DArray
    | Tsampler1DArrayShadow
    | Tsampler2DArrayShadow
    | TsamplerBuffer
    | Tsampler2DMS
    | Tsampler2DMSArray
      -- integer sampler types
    | Tisampler1D
    | Tisampler2D
    | Tisampler3D
    | TisamplerCube
    | Tisampler2DRect
    | Tisampler1DArray
    | Tisampler2DArray
    | TisamplerBuffer
    | Tisampler2DMS
    | Tisampler2DMSArray
      -- unsigned integer sampler types
    | Tusampler1D
    | Tusampler2D
    | Tusampler3D
    | TusamplerCube
    | Tusampler2DRect
    | Tusampler1DArray
    | Tusampler2DArray
    | TusamplerBuffer
    | Tusampler2DMS
    | Tusampler2DMSArray
      -- in/on
    | Tin Type
    | Tout Type
      --
      -- GLSL 4.60
      -- double
    | Tdouble
    | Tdvec2
    | Tdvec3
    | Tdvec4
    | Tdmat2
    | Tdmat3
    | Tdmat4
    | Tdmat2x2
    | Tdmat2x3
    | Tdmat2x4
    | Tdmat3x2
    | Tdmat3x3
    | Tdmat3x4
    | Tdmat4x2
    | Tdmat4x3
    | Tdmat4x4
      --  sampler/texture/image
    | Ttexture1D
    | Timage1D
    | Tcomparison
    | Ttexture1DArray
    | Timage1DArray
    | Ttexture2D
    | Timage2D
    | Ttexture2DArray
    | Timage2DArray
    | Ttexture2DMS
    | Timage2DMS
    | Ttexture2DMSArray
    | Timage2DMSArray
    | Ttexture2DRect
    | Timage2DRect
    | Ttexture3D
    | Timage3D
    | TtextureCube
    | TimageCube
    | TsamplerCubeShadow
    | TsamplerCubeArray
    | TtextureCubeArray
    | TimageCubeArray
    | TsamplerCubeArrayShadow
    | TtextureBuffer
    | TimageBuffer
    | TsubpassInput
    | TsubpassInputMS
      --  sampler/texture/image (signed int)
    | Titexture1DArray
    | Tiimage1DArray
    | Titexture2D
    | Tiimage2D
    | Titexture2DArray
    | Tiimage2DArray
    | Titexture2DMS
    | Tiimage2DMS
    | Titexture2DMSArray
    | Tiimage2DMSArray
    | Titexture2DRect
    | Tiimage2DRect
    | Titexture3D
    | Tiimage3D
    | TitextureCube
    | TiimageCube
    | TisamplerCubeArray
    | TitextureCubeArray
    | TiimageCubeArray
    | TitextureBuffer
    | TiimageBuffer
    | TisubpassInput
    | TisubpassInputMS
      --  sampler/texture/image (unsigned int)
    | Tutexture1DArray
    | Tuimage1DArray
    | Tutexture2D
    | Tuimage2D
    | Tutexture2DArray
    | Tuimage2DArray
    | Tutexture2DMS
    | Tuimage2DMS
    | Tutexture2DMSArray
    | Tuimage2DMSArray
    | Tutexture2DRect
    | Tuimage2DRect
    | Tutexture3D
    | Tuimage3D
    | TutextureCube
    | TuimageCube
    | TusamplerCubeArray
    | TutextureCubeArray
    | TuimageCubeArray
    | TutextureBuffer
    | TuimageBuffer
    | TusubpassInput
    | TusubpassInputMS
      -- sampler
    | Tsampler
    | TsamplerShadow
