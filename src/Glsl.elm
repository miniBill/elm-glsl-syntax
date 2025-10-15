module Glsl exposing
    ( Declaration(..), Precision(..), Statement(..), Type(..), ArgType(..)
    , Expression(..), BinaryOperation(..), UnaryOperation(..), RelationOperation(..)
    )

{-|


# Types

@docs Declaration, Precision, Statement, Type, ArgType
@docs Expression, BinaryOperation, UnaryOperation, RelationOperation

-}

import Glsl.Node exposing (Node)


{-| -}
type Declaration
    = FunctionDeclaration (Node Type) (Node String) (Node (List ( Node ArgType, Node String ))) (List (Node Statement))
    | UniformDeclaration (Node Type) (Node String)
    | ConstDeclaration (Node Type) (Node String) (Node Expression)
    | PrecisionDeclaration (Node Precision) (Node Type)


type Precision
    = Highp


type Expression
    = Bool Bool
    | Int Int
    | Float Float
    | Variable String
    | Ternary (Node Expression) (Node Expression) (Node Expression)
    | UnaryOperation (Node UnaryOperation) (Node Expression)
    | BinaryOperation (Node Expression) (Node BinaryOperation) (Node Expression)
    | Call (Node Expression) (Node (List (Node Expression)))
    | Dot (Node Expression) (Node String)
    | Parens (Node Expression)


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
    = If (Node Expression) (Node Statement)
    | IfElse (Node Expression) (Node Statement) (Node Statement)
    | For (Maybe (Node Statement)) (Node Expression) (Node Expression) (Node Statement)
    | Return (Node Expression)
    | Break
    | Continue
    | ExpressionStatement (Node Expression)
    | Decl { const : Maybe (Node ()) } (Node Type) (Node String) (Maybe (Node Expression))
    | Block (List (Node Statement))


type ArgType
    = ArgIn (Node Type)
    | ArgOut (Node Type)
    | ArgInOut (Node Type)


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
    | Tfloat
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
