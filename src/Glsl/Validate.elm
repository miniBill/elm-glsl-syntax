module Glsl.Validate exposing (Error(..), asV1_10Type, asV3_30Type)

import Glsl
import Glsl.Node exposing (Node(..))
import Glsl.V1_10
import Glsl.V3_30


type Error
    = RequiresV3_30
    | RequiresV4_60


asV1_10Type : Glsl.Type -> Result Error Glsl.V1_10.Type
asV1_10Type tipe =
    case tipe of
        Glsl.Tbool ->
            Ok Glsl.V1_10.Tbool

        Glsl.Tvoid ->
            Ok Glsl.V1_10.Tvoid

        Glsl.Tbvec2 ->
            Ok Glsl.V1_10.Tbvec2

        Glsl.Tbvec3 ->
            Ok Glsl.V1_10.Tbvec3

        Glsl.Tbvec4 ->
            Ok Glsl.V1_10.Tbvec4

        Glsl.Tint ->
            Ok Glsl.V1_10.Tint

        Glsl.Tivec2 ->
            Ok Glsl.V1_10.Tivec2

        Glsl.Tivec3 ->
            Ok Glsl.V1_10.Tivec3

        Glsl.Tivec4 ->
            Ok Glsl.V1_10.Tivec4

        Glsl.Tfloat ->
            Ok Glsl.V1_10.Tfloat

        Glsl.Tvec2 ->
            Ok Glsl.V1_10.Tvec2

        Glsl.Tvec3 ->
            Ok Glsl.V1_10.Tvec3

        Glsl.Tvec4 ->
            Ok Glsl.V1_10.Tvec4

        Glsl.Tmat2 ->
            Ok Glsl.V1_10.Tmat2

        Glsl.Tmat3 ->
            Ok Glsl.V1_10.Tmat3

        Glsl.Tmat4 ->
            Ok Glsl.V1_10.Tmat4

        Glsl.Tsampler1D ->
            Ok Glsl.V1_10.Tsampler1D

        Glsl.Tsampler2D ->
            Ok Glsl.V1_10.Tsampler2D

        Glsl.Tsampler3D ->
            Ok Glsl.V1_10.Tsampler3D

        Glsl.TsamplerCube ->
            Ok Glsl.V1_10.TsamplerCube

        Glsl.Tuint ->
            Err RequiresV3_30

        Glsl.Tuvec2 ->
            Err RequiresV3_30

        Glsl.Tuvec3 ->
            Err RequiresV3_30

        Glsl.Tuvec4 ->
            Err RequiresV3_30

        Glsl.Tmat2x2 ->
            Err RequiresV3_30

        Glsl.Tmat2x3 ->
            Err RequiresV3_30

        Glsl.Tmat2x4 ->
            Err RequiresV3_30

        Glsl.Tmat3x2 ->
            Err RequiresV3_30

        Glsl.Tmat3x3 ->
            Err RequiresV3_30

        Glsl.Tmat3x4 ->
            Err RequiresV3_30

        Glsl.Tmat4x2 ->
            Err RequiresV3_30

        Glsl.Tmat4x3 ->
            Err RequiresV3_30

        Glsl.Tmat4x4 ->
            Err RequiresV3_30

        Glsl.Tsampler2DRect ->
            Err RequiresV3_30

        Glsl.Tsampler1DShadow ->
            Err RequiresV3_30

        Glsl.Tsampler2DShadow ->
            Err RequiresV3_30

        Glsl.Tsampler2DRectShadow ->
            Err RequiresV3_30

        Glsl.Tsampler1DArray ->
            Err RequiresV3_30

        Glsl.Tsampler2DArray ->
            Err RequiresV3_30

        Glsl.Tsampler1DArrayShadow ->
            Err RequiresV3_30

        Glsl.Tsampler2DArrayShadow ->
            Err RequiresV3_30

        Glsl.TsamplerBuffer ->
            Err RequiresV3_30

        Glsl.Tsampler2DMS ->
            Err RequiresV3_30

        Glsl.Tsampler2DMSArray ->
            Err RequiresV3_30

        Glsl.Tisampler1D ->
            Err RequiresV3_30

        Glsl.Tisampler2D ->
            Err RequiresV3_30

        Glsl.Tisampler3D ->
            Err RequiresV3_30

        Glsl.TisamplerCube ->
            Err RequiresV3_30

        Glsl.Tisampler2DRect ->
            Err RequiresV3_30

        Glsl.Tisampler1DArray ->
            Err RequiresV3_30

        Glsl.Tisampler2DArray ->
            Err RequiresV3_30

        Glsl.TisamplerBuffer ->
            Err RequiresV3_30

        Glsl.Tisampler2DMS ->
            Err RequiresV3_30

        Glsl.Tisampler2DMSArray ->
            Err RequiresV3_30

        Glsl.Tusampler1D ->
            Err RequiresV3_30

        Glsl.Tusampler2D ->
            Err RequiresV3_30

        Glsl.Tusampler3D ->
            Err RequiresV3_30

        Glsl.TusamplerCube ->
            Err RequiresV3_30

        Glsl.Tusampler2DRect ->
            Err RequiresV3_30

        Glsl.Tusampler1DArray ->
            Err RequiresV3_30

        Glsl.Tusampler2DArray ->
            Err RequiresV3_30

        Glsl.TusamplerBuffer ->
            Err RequiresV3_30

        Glsl.Tusampler2DMS ->
            Err RequiresV3_30

        Glsl.Tusampler2DMSArray ->
            Err RequiresV3_30

        Glsl.Tin _ ->
            Err RequiresV3_30

        Glsl.Tout _ ->
            Err RequiresV3_30

        Glsl.Tdouble ->
            Err RequiresV4_60

        Glsl.Tdvec2 ->
            Err RequiresV4_60

        Glsl.Tdvec3 ->
            Err RequiresV4_60

        Glsl.Tdvec4 ->
            Err RequiresV4_60

        Glsl.Tdmat2 ->
            Err RequiresV4_60

        Glsl.Tdmat3 ->
            Err RequiresV4_60

        Glsl.Tdmat4 ->
            Err RequiresV4_60

        Glsl.Tdmat2x2 ->
            Err RequiresV4_60

        Glsl.Tdmat2x3 ->
            Err RequiresV4_60

        Glsl.Tdmat2x4 ->
            Err RequiresV4_60

        Glsl.Tdmat3x2 ->
            Err RequiresV4_60

        Glsl.Tdmat3x3 ->
            Err RequiresV4_60

        Glsl.Tdmat3x4 ->
            Err RequiresV4_60

        Glsl.Tdmat4x2 ->
            Err RequiresV4_60

        Glsl.Tdmat4x3 ->
            Err RequiresV4_60

        Glsl.Tdmat4x4 ->
            Err RequiresV4_60

        Glsl.Ttexture1D ->
            Err RequiresV4_60

        Glsl.Timage1D ->
            Err RequiresV4_60

        Glsl.Ttexture1DArray ->
            Err RequiresV4_60

        Glsl.Timage1DArray ->
            Err RequiresV4_60

        Glsl.Ttexture2D ->
            Err RequiresV4_60

        Glsl.Timage2D ->
            Err RequiresV4_60

        Glsl.Ttexture2DArray ->
            Err RequiresV4_60

        Glsl.Timage2DArray ->
            Err RequiresV4_60

        Glsl.Ttexture2DMS ->
            Err RequiresV4_60

        Glsl.Timage2DMS ->
            Err RequiresV4_60

        Glsl.Ttexture2DMSArray ->
            Err RequiresV4_60

        Glsl.Timage2DMSArray ->
            Err RequiresV4_60

        Glsl.Ttexture2DRect ->
            Err RequiresV4_60

        Glsl.Timage2DRect ->
            Err RequiresV4_60

        Glsl.Ttexture3D ->
            Err RequiresV4_60

        Glsl.Timage3D ->
            Err RequiresV4_60

        Glsl.TtextureCube ->
            Err RequiresV4_60

        Glsl.TimageCube ->
            Err RequiresV4_60

        Glsl.TsamplerCubeShadow ->
            Err RequiresV4_60

        Glsl.TsamplerCubeArray ->
            Err RequiresV4_60

        Glsl.TtextureCubeArray ->
            Err RequiresV4_60

        Glsl.TimageCubeArray ->
            Err RequiresV4_60

        Glsl.TsamplerCubeArrayShadow ->
            Err RequiresV4_60

        Glsl.TtextureBuffer ->
            Err RequiresV4_60

        Glsl.TimageBuffer ->
            Err RequiresV4_60

        Glsl.TsubpassInput ->
            Err RequiresV4_60

        Glsl.TsubpassInputMS ->
            Err RequiresV4_60

        Glsl.Titexture1DArray ->
            Err RequiresV4_60

        Glsl.Tiimage1DArray ->
            Err RequiresV4_60

        Glsl.Titexture2D ->
            Err RequiresV4_60

        Glsl.Tiimage2D ->
            Err RequiresV4_60

        Glsl.Titexture2DArray ->
            Err RequiresV4_60

        Glsl.Tiimage2DArray ->
            Err RequiresV4_60

        Glsl.Titexture2DMS ->
            Err RequiresV4_60

        Glsl.Tiimage2DMS ->
            Err RequiresV4_60

        Glsl.Titexture2DMSArray ->
            Err RequiresV4_60

        Glsl.Tiimage2DMSArray ->
            Err RequiresV4_60

        Glsl.Titexture2DRect ->
            Err RequiresV4_60

        Glsl.Tiimage2DRect ->
            Err RequiresV4_60

        Glsl.Titexture3D ->
            Err RequiresV4_60

        Glsl.Tiimage3D ->
            Err RequiresV4_60

        Glsl.TitextureCube ->
            Err RequiresV4_60

        Glsl.TiimageCube ->
            Err RequiresV4_60

        Glsl.TisamplerCubeArray ->
            Err RequiresV4_60

        Glsl.TitextureCubeArray ->
            Err RequiresV4_60

        Glsl.TiimageCubeArray ->
            Err RequiresV4_60

        Glsl.TitextureBuffer ->
            Err RequiresV4_60

        Glsl.TiimageBuffer ->
            Err RequiresV4_60

        Glsl.TisubpassInput ->
            Err RequiresV4_60

        Glsl.TisubpassInputMS ->
            Err RequiresV4_60

        Glsl.Tutexture1DArray ->
            Err RequiresV4_60

        Glsl.Tuimage1DArray ->
            Err RequiresV4_60

        Glsl.Tutexture2D ->
            Err RequiresV4_60

        Glsl.Tuimage2D ->
            Err RequiresV4_60

        Glsl.Tutexture2DArray ->
            Err RequiresV4_60

        Glsl.Tuimage2DArray ->
            Err RequiresV4_60

        Glsl.Tutexture2DMS ->
            Err RequiresV4_60

        Glsl.Tuimage2DMS ->
            Err RequiresV4_60

        Glsl.Tutexture2DMSArray ->
            Err RequiresV4_60

        Glsl.Tuimage2DMSArray ->
            Err RequiresV4_60

        Glsl.Tutexture2DRect ->
            Err RequiresV4_60

        Glsl.Tuimage2DRect ->
            Err RequiresV4_60

        Glsl.Tutexture3D ->
            Err RequiresV4_60

        Glsl.Tuimage3D ->
            Err RequiresV4_60

        Glsl.TutextureCube ->
            Err RequiresV4_60

        Glsl.TuimageCube ->
            Err RequiresV4_60

        Glsl.TusamplerCubeArray ->
            Err RequiresV4_60

        Glsl.TutextureCubeArray ->
            Err RequiresV4_60

        Glsl.TuimageCubeArray ->
            Err RequiresV4_60

        Glsl.TutextureBuffer ->
            Err RequiresV4_60

        Glsl.TuimageBuffer ->
            Err RequiresV4_60

        Glsl.TusubpassInput ->
            Err RequiresV4_60

        Glsl.TusubpassInputMS ->
            Err RequiresV4_60

        Glsl.Tsampler ->
            Err RequiresV4_60

        Glsl.TsamplerShadow ->
            Err RequiresV4_60


asV3_30Type : Glsl.Type -> Result Error Glsl.V3_30.Type
asV3_30Type tipe =
    case tipe of
        Glsl.Tbool ->
            Ok Glsl.V3_30.Tbool

        Glsl.Tvoid ->
            Ok Glsl.V3_30.Tvoid

        Glsl.Tbvec2 ->
            Ok Glsl.V3_30.Tbvec2

        Glsl.Tbvec3 ->
            Ok Glsl.V3_30.Tbvec3

        Glsl.Tbvec4 ->
            Ok Glsl.V3_30.Tbvec4

        Glsl.Tint ->
            Ok Glsl.V3_30.Tint

        Glsl.Tivec2 ->
            Ok Glsl.V3_30.Tivec2

        Glsl.Tivec3 ->
            Ok Glsl.V3_30.Tivec3

        Glsl.Tivec4 ->
            Ok Glsl.V3_30.Tivec4

        Glsl.Tfloat ->
            Ok Glsl.V3_30.Tfloat

        Glsl.Tvec2 ->
            Ok Glsl.V3_30.Tvec2

        Glsl.Tvec3 ->
            Ok Glsl.V3_30.Tvec3

        Glsl.Tvec4 ->
            Ok Glsl.V3_30.Tvec4

        Glsl.Tmat2 ->
            Ok Glsl.V3_30.Tmat2

        Glsl.Tmat3 ->
            Ok Glsl.V3_30.Tmat3

        Glsl.Tmat4 ->
            Ok Glsl.V3_30.Tmat4

        Glsl.Tsampler1D ->
            Ok Glsl.V3_30.Tsampler1D

        Glsl.Tsampler2D ->
            Ok Glsl.V3_30.Tsampler2D

        Glsl.Tsampler3D ->
            Ok Glsl.V3_30.Tsampler3D

        Glsl.TsamplerCube ->
            Ok Glsl.V3_30.TsamplerCube

        Glsl.Tuint ->
            Ok Glsl.V3_30.Tuint

        Glsl.Tuvec2 ->
            Ok Glsl.V3_30.Tuvec2

        Glsl.Tuvec3 ->
            Ok Glsl.V3_30.Tuvec3

        Glsl.Tuvec4 ->
            Ok Glsl.V3_30.Tuvec4

        Glsl.Tmat2x2 ->
            Ok Glsl.V3_30.Tmat2x2

        Glsl.Tmat2x3 ->
            Ok Glsl.V3_30.Tmat2x3

        Glsl.Tmat2x4 ->
            Ok Glsl.V3_30.Tmat2x4

        Glsl.Tmat3x2 ->
            Ok Glsl.V3_30.Tmat3x2

        Glsl.Tmat3x3 ->
            Ok Glsl.V3_30.Tmat3x3

        Glsl.Tmat3x4 ->
            Ok Glsl.V3_30.Tmat3x4

        Glsl.Tmat4x2 ->
            Ok Glsl.V3_30.Tmat4x2

        Glsl.Tmat4x3 ->
            Ok Glsl.V3_30.Tmat4x3

        Glsl.Tmat4x4 ->
            Ok Glsl.V3_30.Tmat4x4

        Glsl.Tsampler2DRect ->
            Ok Glsl.V3_30.Tsampler2DRect

        Glsl.Tsampler1DShadow ->
            Ok Glsl.V3_30.Tsampler1DShadow

        Glsl.Tsampler2DShadow ->
            Ok Glsl.V3_30.Tsampler2DShadow

        Glsl.Tsampler2DRectShadow ->
            Ok Glsl.V3_30.Tsampler2DRectShadow

        Glsl.Tsampler1DArray ->
            Ok Glsl.V3_30.Tsampler1DArray

        Glsl.Tsampler2DArray ->
            Ok Glsl.V3_30.Tsampler2DArray

        Glsl.Tsampler1DArrayShadow ->
            Ok Glsl.V3_30.Tsampler1DArrayShadow

        Glsl.Tsampler2DArrayShadow ->
            Ok Glsl.V3_30.Tsampler2DArrayShadow

        Glsl.TsamplerBuffer ->
            Ok Glsl.V3_30.TsamplerBuffer

        Glsl.Tsampler2DMS ->
            Ok Glsl.V3_30.Tsampler2DMS

        Glsl.Tsampler2DMSArray ->
            Ok Glsl.V3_30.Tsampler2DMSArray

        Glsl.Tisampler1D ->
            Ok Glsl.V3_30.Tisampler1D

        Glsl.Tisampler2D ->
            Ok Glsl.V3_30.Tisampler2D

        Glsl.Tisampler3D ->
            Ok Glsl.V3_30.Tisampler3D

        Glsl.TisamplerCube ->
            Ok Glsl.V3_30.TisamplerCube

        Glsl.Tisampler2DRect ->
            Ok Glsl.V3_30.Tisampler2DRect

        Glsl.Tisampler1DArray ->
            Ok Glsl.V3_30.Tisampler1DArray

        Glsl.Tisampler2DArray ->
            Ok Glsl.V3_30.Tisampler2DArray

        Glsl.TisamplerBuffer ->
            Ok Glsl.V3_30.TisamplerBuffer

        Glsl.Tisampler2DMS ->
            Ok Glsl.V3_30.Tisampler2DMS

        Glsl.Tisampler2DMSArray ->
            Ok Glsl.V3_30.Tisampler2DMSArray

        Glsl.Tusampler1D ->
            Ok Glsl.V3_30.Tusampler1D

        Glsl.Tusampler2D ->
            Ok Glsl.V3_30.Tusampler2D

        Glsl.Tusampler3D ->
            Ok Glsl.V3_30.Tusampler3D

        Glsl.TusamplerCube ->
            Ok Glsl.V3_30.TusamplerCube

        Glsl.Tusampler2DRect ->
            Ok Glsl.V3_30.Tusampler2DRect

        Glsl.Tusampler1DArray ->
            Ok Glsl.V3_30.Tusampler1DArray

        Glsl.Tusampler2DArray ->
            Ok Glsl.V3_30.Tusampler2DArray

        Glsl.TusamplerBuffer ->
            Ok Glsl.V3_30.TusamplerBuffer

        Glsl.Tusampler2DMS ->
            Ok Glsl.V3_30.Tusampler2DMS

        Glsl.Tusampler2DMSArray ->
            Ok Glsl.V3_30.Tusampler2DMSArray

        Glsl.Tin (Node r c) ->
            Result.map (\v -> Glsl.V3_30.Tin (Node r v)) (asV3_30Type c)

        Glsl.Tout (Node r c) ->
            Result.map (\v -> Glsl.V3_30.Tout (Node r v)) (asV3_30Type c)

        Glsl.Tdouble ->
            Err RequiresV4_60

        Glsl.Tdvec2 ->
            Err RequiresV4_60

        Glsl.Tdvec3 ->
            Err RequiresV4_60

        Glsl.Tdvec4 ->
            Err RequiresV4_60

        Glsl.Tdmat2 ->
            Err RequiresV4_60

        Glsl.Tdmat3 ->
            Err RequiresV4_60

        Glsl.Tdmat4 ->
            Err RequiresV4_60

        Glsl.Tdmat2x2 ->
            Err RequiresV4_60

        Glsl.Tdmat2x3 ->
            Err RequiresV4_60

        Glsl.Tdmat2x4 ->
            Err RequiresV4_60

        Glsl.Tdmat3x2 ->
            Err RequiresV4_60

        Glsl.Tdmat3x3 ->
            Err RequiresV4_60

        Glsl.Tdmat3x4 ->
            Err RequiresV4_60

        Glsl.Tdmat4x2 ->
            Err RequiresV4_60

        Glsl.Tdmat4x3 ->
            Err RequiresV4_60

        Glsl.Tdmat4x4 ->
            Err RequiresV4_60

        Glsl.Ttexture1D ->
            Err RequiresV4_60

        Glsl.Timage1D ->
            Err RequiresV4_60

        Glsl.Ttexture1DArray ->
            Err RequiresV4_60

        Glsl.Timage1DArray ->
            Err RequiresV4_60

        Glsl.Ttexture2D ->
            Err RequiresV4_60

        Glsl.Timage2D ->
            Err RequiresV4_60

        Glsl.Ttexture2DArray ->
            Err RequiresV4_60

        Glsl.Timage2DArray ->
            Err RequiresV4_60

        Glsl.Ttexture2DMS ->
            Err RequiresV4_60

        Glsl.Timage2DMS ->
            Err RequiresV4_60

        Glsl.Ttexture2DMSArray ->
            Err RequiresV4_60

        Glsl.Timage2DMSArray ->
            Err RequiresV4_60

        Glsl.Ttexture2DRect ->
            Err RequiresV4_60

        Glsl.Timage2DRect ->
            Err RequiresV4_60

        Glsl.Ttexture3D ->
            Err RequiresV4_60

        Glsl.Timage3D ->
            Err RequiresV4_60

        Glsl.TtextureCube ->
            Err RequiresV4_60

        Glsl.TimageCube ->
            Err RequiresV4_60

        Glsl.TsamplerCubeShadow ->
            Err RequiresV4_60

        Glsl.TsamplerCubeArray ->
            Err RequiresV4_60

        Glsl.TtextureCubeArray ->
            Err RequiresV4_60

        Glsl.TimageCubeArray ->
            Err RequiresV4_60

        Glsl.TsamplerCubeArrayShadow ->
            Err RequiresV4_60

        Glsl.TtextureBuffer ->
            Err RequiresV4_60

        Glsl.TimageBuffer ->
            Err RequiresV4_60

        Glsl.TsubpassInput ->
            Err RequiresV4_60

        Glsl.TsubpassInputMS ->
            Err RequiresV4_60

        Glsl.Titexture1DArray ->
            Err RequiresV4_60

        Glsl.Tiimage1DArray ->
            Err RequiresV4_60

        Glsl.Titexture2D ->
            Err RequiresV4_60

        Glsl.Tiimage2D ->
            Err RequiresV4_60

        Glsl.Titexture2DArray ->
            Err RequiresV4_60

        Glsl.Tiimage2DArray ->
            Err RequiresV4_60

        Glsl.Titexture2DMS ->
            Err RequiresV4_60

        Glsl.Tiimage2DMS ->
            Err RequiresV4_60

        Glsl.Titexture2DMSArray ->
            Err RequiresV4_60

        Glsl.Tiimage2DMSArray ->
            Err RequiresV4_60

        Glsl.Titexture2DRect ->
            Err RequiresV4_60

        Glsl.Tiimage2DRect ->
            Err RequiresV4_60

        Glsl.Titexture3D ->
            Err RequiresV4_60

        Glsl.Tiimage3D ->
            Err RequiresV4_60

        Glsl.TitextureCube ->
            Err RequiresV4_60

        Glsl.TiimageCube ->
            Err RequiresV4_60

        Glsl.TisamplerCubeArray ->
            Err RequiresV4_60

        Glsl.TitextureCubeArray ->
            Err RequiresV4_60

        Glsl.TiimageCubeArray ->
            Err RequiresV4_60

        Glsl.TitextureBuffer ->
            Err RequiresV4_60

        Glsl.TiimageBuffer ->
            Err RequiresV4_60

        Glsl.TisubpassInput ->
            Err RequiresV4_60

        Glsl.TisubpassInputMS ->
            Err RequiresV4_60

        Glsl.Tutexture1DArray ->
            Err RequiresV4_60

        Glsl.Tuimage1DArray ->
            Err RequiresV4_60

        Glsl.Tutexture2D ->
            Err RequiresV4_60

        Glsl.Tuimage2D ->
            Err RequiresV4_60

        Glsl.Tutexture2DArray ->
            Err RequiresV4_60

        Glsl.Tuimage2DArray ->
            Err RequiresV4_60

        Glsl.Tutexture2DMS ->
            Err RequiresV4_60

        Glsl.Tuimage2DMS ->
            Err RequiresV4_60

        Glsl.Tutexture2DMSArray ->
            Err RequiresV4_60

        Glsl.Tuimage2DMSArray ->
            Err RequiresV4_60

        Glsl.Tutexture2DRect ->
            Err RequiresV4_60

        Glsl.Tuimage2DRect ->
            Err RequiresV4_60

        Glsl.Tutexture3D ->
            Err RequiresV4_60

        Glsl.Tuimage3D ->
            Err RequiresV4_60

        Glsl.TutextureCube ->
            Err RequiresV4_60

        Glsl.TuimageCube ->
            Err RequiresV4_60

        Glsl.TusamplerCubeArray ->
            Err RequiresV4_60

        Glsl.TutextureCubeArray ->
            Err RequiresV4_60

        Glsl.TuimageCubeArray ->
            Err RequiresV4_60

        Glsl.TutextureBuffer ->
            Err RequiresV4_60

        Glsl.TuimageBuffer ->
            Err RequiresV4_60

        Glsl.TusubpassInput ->
            Err RequiresV4_60

        Glsl.TusubpassInputMS ->
            Err RequiresV4_60

        Glsl.Tsampler ->
            Err RequiresV4_60

        Glsl.TsamplerShadow ->
            Err RequiresV4_60
