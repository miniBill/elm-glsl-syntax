module ShaderToy exposing (suite)

import ErrorUtils
import Expect
import Glsl exposing (BinaryOperation(..), Declaration(..), Expression(..), Statement(..), Type(..), UnaryOperation(..))
import Glsl.Parser
import IsAlmostEquals
import Parser.Advanced
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "ShaderTody examples"
        [ hexagonal
        ]


hexagonal : Test
hexagonal =
    check "https://www.shadertoy.com/view/4d2GzV"
        """
const float i3 = 0.5773502691896258;

vec4 pick3(vec4 a, vec4 b, vec4 c, float u) {
    float v = fract(u * 0.3333333333333);
    return mix(mix(a, b, step(0.3, v)), c, step(0.6, v));
}

vec4 closestHexCenters(vec2 p) {
    vec2 pi = floor(p);
    vec2 pf = fract(p);

    vec4 nn = pick3(vec4(0.0, 0.0, 2.0,  1.0),
                    vec4(1.0, 1.0, 0.0, -1.0),
                    vec4(1.0, 0.0, 0.0,  1.0),
                    pi.x + pi.y);

    return ( mix(nn, nn.yxwz, step(pf.x, pf.y)) +
             vec4(pi, pi) );
}

const mat2 cart2tri = mat2(1.0, 0.0, i3, 2.0*i3);

const float s3 = 1.7320508075688772;

const mat2 tri2cart = mat2(1.0, 0.0, -0.5, 0.5*s3);

float hash(vec2 pos) {
    // return texture(iChannel0, fract(pos/511.0)).x;
    return fract(pos/511.0).x;
}

vec3 perpBisector(vec2 p1, vec2 p2) {
    vec2 p21 = p2-p1;
    vec3 pa = vec3(p1+0.5*p21, 1.0);
    vec3 pb = vec3(pa.x-p21.y, pa.y+p21.x, 1.0);
    vec3 l = cross(pa, pb);
    return l * inversesqrt(dot(l.xy, l.xy));
}


void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
    // float scl = (0.03 + 0.02*(1.0+sin(0.31*iTime))) * 200.0 / min(iResolution.x, iResolution.y);
    float scl = 0.;
    float iTime = 0.;

    float cx = 2.0 * cos(iTime*0.3) + 1.0 * cos(iTime*0.7+2.0);
    float cy = 4.0 * sin(iTime*0.4) + 0.3 * sin(iTime*1.2+4.0);
    float theta = 0.05*iTime;

    //vec2 pos = (fragCoord.xy - 0.5*iResolution.xy)*scl + vec2(cx, cy);
    vec2 pos = fragCoord.xy*scl + vec2(cx, cy);

    float ct = cos(theta);
    float st = sin(theta);

    pos = mat2(ct, -st, st, ct) * pos;
    vec3 L = vec3(i3, -i3, i3);
    L.xy = mat2(ct, -st, st, ct) * L.xy;

    vec4 h = closestHexCenters(cart2tri*pos);

    vec2 q1 = tri2cart * h.xy;

    float s = step(hash(h.xy), 0.5)*2.0-1.0;

    vec2 d1 = pos - q1;

    float l = min(min(distance(d1, vec2(s*-1.0, 0.0)),
                      distance(d1, vec2(s*0.5, 0.5*s3))),
                      distance(d1, vec2(s*0.5, -0.5*s3)));

    const float r = 0.5;

    vec4 truchet = vec4(vec3(smoothstep(0.1+scl, 0.1, abs(l-r))), 1.0);

    vec2 q2 = tri2cart * h.zw;

    vec4 rgb = pick3(vec4(1.0, 0.0, 0.0, 1.0),
                     vec4(0.0, 1.0, 0.0, 1.0),
                     vec4(0.0, 0.0, 1.0, 1.0),
                     h.x);

    vec3 line = perpBisector(q1, q2);

    float d = dot(vec3(pos, 1.0), line);

    vec4 black = vec4(vec3(0.), 1.0);

    vec2 nxy = mix(0.4*line.xy, vec2(0.0), smoothstep(0.3, 0.3+scl, d));

    vec3 N = vec3(nxy, sqrt(1.0-dot(nxy,nxy)));
    float ln = clamp(0.0, 1.0, dot(L, N));

    vec4 lite = mix(rgb, vec4(1.0), clamp(0.0, 1.0, 2.0*ln-1.0));
    vec4 dark = mix(black, rgb, clamp(0.0, 1.0, 2.0*ln));
    vec4 lrgb = mix(dark, lite, step(0.5, ln));

    vec4 crgb = mix(black, lrgb, smoothstep(0.01, 0.01+scl, d));

    float t = fract(0.04*iTime);
    float invb = 4.0;

    float k = smoothstep(0.0, 1.0, min(t*invb-0.5*invb+1.0, -t*invb+invb));

    fragColor = mix(crgb, truchet, k);

}
"""
    <|
        [ const float "i3" (f 0.5773502691896258)
        , func vec4 "pick3" [ vec4 "a", vec4 "b", vec4 "c", float "u" ] <|
            [ decl float "v" (call "fract" [ by (var "u") (f (1 / 3)) ])
            , Return
                (call "mix"
                    [ call "mix" [ var "a", var "b", call "step" [ f 0.3, var "v" ] ]
                    , var "c"
                    , call "step" [ f 0.6, var "v" ]
                    ]
                )
            ]
        , func vec4 "closestHexCenters" [ vec2 "p" ] <|
            [ decl vec2 "pi" (call "floor" [ var "p" ])
            , decl vec2 "pf" (call "fract" [ var "p" ])
            , decl vec4 "nn" <|
                call "pick3"
                    [ call "vec4" [ f 0, f 0, f 2, f 1 ]
                    , call "vec4" [ f 1, f 1, f 0, f -1 ]
                    , call "vec4" [ f 1, f 0, f 0, f 1 ]
                    , add (dot "pi" "x") (dot "pi" "y")
                    ]
            , Return
                (add
                    (call "mix"
                        [ var "nn"
                        , dot "nn" "yxwz"
                        , call "step" [ dot "pf" "x", dot "pf" "y" ]
                        ]
                    )
                    (call "vec4" [ var "pi", var "pi" ])
                )
            ]
        , const mat2 "cart2tri" <|
            call "mat2" [ f 1, f 0, var "i3", by (f 2) (var "i3") ]
        , const float "s3" (f 1.7320508075688772)
        , const mat2 "tri2cart" <|
            call "mat2" [ f 1, f 0, f -0.5, by (f 0.5) (var "s3") ]
        , func float "hash" [ vec2 "pos" ] <|
            [ Return (Dot (call "fract" [ div (var "pos") (f 511) ]) "x")
            ]
        , func vec3 "perpBisector" [ vec2 "p1", vec2 "p2" ] <|
            [ decl vec2 "p21" <| subtract (var "p2") (var "p1")
            , decl vec3 "pa" <|
                call "vec3"
                    [ add (var "p1") (by (f 0.5) (var "p21"))
                    , f 1
                    ]
            , decl vec3 "pb" <|
                call "vec3"
                    [ subtract (dot "pa" "x") (dot "p21" "y")
                    , add (dot "pa" "y") (dot "p21" "x")
                    , f 1
                    ]
            , decl vec3 "l" <| call "cross" [ var "pa", var "pb" ]
            , Return
                (by
                    (var "l")
                    (call "inversesqrt"
                        [ call "dot" [ dot "l" "xy", dot "l" "xy" ]
                        ]
                    )
                )
            ]
        , func void "mainImage" [ out vec4 "fragColor", in_ vec2 "fragCoord" ] <|
            [ decl float "scl" (f 0)
            , decl float "iTime" (f 0)
            , decl float "cx" <|
                add
                    (by
                        (f 2)
                        (call "cos" [ by (var "iTime") (f 0.3) ])
                    )
                    (by
                        (f 1)
                        (call "cos" [ add (by (var "iTime") (f 0.7)) (f 2) ])
                    )
            , decl float "cy" <|
                add
                    (by
                        (f 4)
                        (call "sin" [ by (var "iTime") (f 0.4) ])
                    )
                    (by
                        (f 0.3)
                        (call "sin" [ add (by (var "iTime") (f 1.2)) (f 4) ])
                    )
            , decl float "theta" <| by (f 0.05) (var "iTime")
            , decl vec2 "pos" <|
                add
                    (by (dot "fragCoord" "xy") (var "scl"))
                    (call "vec2" [ var "cx", var "cy" ])
            , decl float "ct" <| call "cos" [ var "theta" ]
            , decl float "st" <| call "sin" [ var "theta" ]
            , assign (var "pos") <|
                by
                    (call "mat2" [ var "ct", negate (var "st"), var "st", var "ct" ])
                    (var "pos")
            , decl vec3 "L" <|
                call "vec3" [ var "i3", negate (var "i3"), var "i3" ]
            , assign (dot "L" "xy") <|
                by
                    (call "mat2"
                        [ var "ct"
                        , negate (var "st")
                        , var "st"
                        , var "ct"
                        ]
                    )
                    (dot "L" "xy")
            , decl vec4 "h" <| call "closestHexCenters" [ by (var "cart2tri") (var "pos") ]
            , decl vec2 "q1" <| by (var "tri2cart") (dot "h" "xy")
            , decl float "s" <|
                subtract
                    (by (call "step" [ call "hash" [ dot "h" "xy" ], f 0.5 ]) (f 2))
                    (f 1)
            , decl vec2 "d1" (subtract (var "pos") (var "q1"))
            , decl float "l" <|
                call "min"
                    [ call "min"
                        [ call "distance"
                            [ var "d1"
                            , call "vec2" [ by (var "s") (f -1), f 0 ]
                            ]
                        , call "distance"
                            [ var "d1"
                            , call "vec2" [ by (var "s") (f 0.5), by (f 0.5) (var "s3") ]
                            ]
                        ]
                    , call "distance"
                        [ var "d1"
                        , call "vec2" [ by (var "s") (f 0.5), by (f -0.5) (var "s3") ]
                        ]
                    ]
            , decl float "r" (f 0.5)
            , decl vec4 "truchet" <|
                call "vec4"
                    [ call "vec3"
                        [ call "smoothstep"
                            [ add (f 0.1) (var "scl")
                            , f 0.1
                            , call "abs" [ subtract (var "l") (var "r") ]
                            ]
                        ]
                    , f 1
                    ]
            , decl vec2 "q2" <| by (var "tri2cart") (dot "h" "zw")
            , decl vec4 "rgb" <|
                call "pick3"
                    [ call "vec4" [ f 1, f 0, f 0, f 1 ]
                    , call "vec4" [ f 0, f 1, f 0, f 1 ]
                    , call "vec4" [ f 0, f 0, f 1, f 1 ]
                    , dot "h" "x"
                    ]
            , decl vec3 "line" <| call "perpBisector" [ var "q1", var "q2" ]
            , decl float "d" <| call "dot" [ call "vec3" [ var "pos", f 1 ], var "line" ]
            , decl vec4 "black" <| call "vec4" [ call "vec3" [ f 0 ], f 1 ]
            , decl vec2 "nxy" <|
                call "mix"
                    [ by (f 0.4) (dot "line" "xy")
                    , call "vec2" [ f 0 ]
                    , call "smoothstep" [ f 0.3, add (f 0.3) (var "scl"), var "d" ]
                    ]
            , decl vec3 "N" <|
                call "vec3"
                    [ var "nxy"
                    , call "sqrt" [ subtract (f 1) (call "dot" [ var "nxy", var "nxy" ]) ]
                    ]
            , decl float "ln" <|
                call "clamp" [ f 0, f 1, call "dot" [ var "L", var "N" ] ]
            , decl vec4 "lite" <|
                call "mix"
                    [ var "rgb"
                    , call "vec4" [ f 1 ]
                    , call "clamp" [ f 0, f 1, subtract (by (f 2) (var "ln")) (f 1) ]
                    ]
            , decl vec4 "dark" <|
                call "mix"
                    [ var "black"
                    , var "rgb"
                    , call "clamp" [ f 0, f 1, by (f 2) (var "ln") ]
                    ]
            , decl vec4 "lrgb" <|
                call "mix"
                    [ var "dark"
                    , var "lite"
                    , call "step" [ f 0.5, var "ln" ]
                    ]
            , decl vec4 "crgb" <|
                call "mix"
                    [ var "black"
                    , var "lrgb"
                    , call "smoothstep" [ f 0.01, add (f 0.01) (var "scl"), var "d" ]
                    ]
            , decl float "t" <| call "fract" [ by (f 0.04) (var "iTime") ]
            , decl float "invb" (f 4)
            , decl float "k" <|
                call "smoothstep"
                    [ f 0
                    , f 1
                    , call "min"
                        [ add
                            (subtract
                                (by (var "t") (var "invb"))
                                (by (f 0.5) (var "invb"))
                            )
                            (f 1)
                        , add
                            (by
                                (negate (var "t"))
                                (var "invb")
                            )
                            (var "invb")
                        ]
                    ]
            , assign (var "fragColor") <| call "mix" [ var "crgb", var "truchet", var "k" ]
            ]
        ]


const : (String -> ( Type, a )) -> String -> Expression -> Declaration
const t n v =
    ConstDeclaration (Tuple.first (t "")) n v


decl : (String -> ( Type, a )) -> String -> Expression -> Statement
decl t n v =
    Decl (Tuple.first (t "")) n (Just v)


func : (String -> ( Type, a )) -> String -> List ( Type, String ) -> List Statement -> Declaration
func t n a s =
    FunctionDeclaration (Tuple.first (t "")) n a s


void : a -> ( Type, a )
void s =
    ( TVoid, s )


float : a -> ( Type, a )
float s =
    ( TFloat, s )


vec2 : a -> ( Type, a )
vec2 s =
    ( TVec2, s )


vec3 : a -> ( Type, a )
vec3 s =
    ( TVec3, s )


vec4 : a -> ( Type, a )
vec4 s =
    ( TVec4, s )


mat2 : a -> ( Type, a )
mat2 s =
    ( TMat2, s )


in_ : (String -> ( Type, a )) -> a -> ( Type, a )
in_ t n =
    ( TIn (Tuple.first (t "")), n )


out : (String -> ( Type, a )) -> a -> ( Type, a )
out t n =
    ( TOut (Tuple.first (t "")), n )


var : String -> Expression
var =
    Variable


f : Float -> Expression
f =
    Float


negate : Expression -> Expression
negate =
    UnaryOperation Negate


add : Expression -> Expression -> Expression
add l r =
    BinaryOperation l Add r


subtract : Expression -> Expression -> Expression
subtract l r =
    BinaryOperation l Subtract r


by : Expression -> Expression -> Expression
by l r =
    BinaryOperation l By r


div : Expression -> Expression -> Expression
div l r =
    BinaryOperation l Div r


assign : Expression -> Expression -> Statement
assign l r =
    ExpressionStatement (BinaryOperation l Assign r)


dot : String -> String -> Expression
dot v =
    Dot (Variable v)


call : String -> List Expression -> Expression
call v =
    Call (Variable v)


check : String -> String -> List Declaration -> Test
check label input expected =
    test label <| \_ ->
    case Parser.Advanced.run Glsl.Parser.file input of
        Err e ->
            ErrorUtils.errorsToString input e
                |> Expect.fail

        Ok ( _, actual ) ->
            actual
                |> IsAlmostEquals.list IsAlmostEquals.declaration expected
                |> IsAlmostEquals.toExpectation
