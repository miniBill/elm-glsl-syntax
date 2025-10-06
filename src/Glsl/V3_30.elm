module Glsl.V3_30 exposing (Type(..))


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
