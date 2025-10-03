module Glsl.V1_10 exposing (Type(..))


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
