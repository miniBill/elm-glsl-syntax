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
    = TVoid
      -- bool
    | TBool
    | TBVec2
    | TBVec3
    | TBVec4
      -- int
    | TInt
    | TIVec2
    | TIVec3
    | TIVec4
      -- uint
    | TUint
    | TUVec2
    | TUVec3
    | TUVec4
      -- float
    | TFloat
    | TVec2
    | TVec3
    | TVec4
      -- double
    | TDouble
    | TDVec2
    | TDVec3
    | TDVec4
      -- mat
    | TMat2
    | TMat3
    | TMat4
    | TMat23
    | TMat24
    | TMat32
    | TMat34
    | TMat42
    | TMat43
    | TDMat2
    | TDMat3
    | TDMat4
    | TDMat23
    | TDMat24
    | TDMat32
    | TDMat34
    | TDMat42
    | TDMat43
      -- Sampler/image
    | TSampler1D
    | TImage1D
    | TSampler2D
    | TImage2D
    | TSampler3D
    | TImage3D
    | TSamplerCube
    | TImageCube
    | TSampler2DRect
    | TImage2DRect
    | TSampler1DArray
    | TImage1DArray
    | TSampler2DArray
    | TImage2DArray
    | TSamplerBuffer
    | TImageBuffer
    | TSampler2DMS
    | TImage2DMS
    | TSampler2DMSArray
    | TImage2DMSArray
    | TSamplerCubeArray
    | TImageCubeArray
    | TSampler1DShadow
    | TSampler2DShadow
    | TSampler2DRectShadow
    | TSampler1DArrayShadow
    | TSampler2DArrayShadow
    | TSamplerCubeShadow
    | TSamplerCubeArrayShadow
      -- in/on
    | TIn Type
    | TOut Type
