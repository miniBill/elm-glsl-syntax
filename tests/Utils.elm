module Utils exposing (add, assign, by, call, const, constDecl, decl, div, dot, expr, f, float, func, i, in_, int, mat2, negate_, out, return, subtract, var, vec2, vec3, vec4, void)

import Glsl exposing (ArgType(..), BinaryOperation(..), Declaration(..), Expression(..), Statement(..), Type(..), UnaryOperation(..))
import Glsl.Node as Node exposing (Node)


const : (String -> ( Type, a )) -> String -> Node Expression -> Node Declaration
const t n v =
    Node.empty (ConstDeclaration (Node.empty (Tuple.first (t ""))) (Node.empty n) v)


constDecl : (String -> ( Type, a )) -> String -> Node Expression -> Node Statement
constDecl t n v =
    Node.empty (Decl { const = Just (Node.empty ()) } (Node.empty (Tuple.first (t ""))) (Node.empty n) (Just v))


decl : (String -> ( Type, a )) -> String -> Node Expression -> Node Statement
decl t n v =
    Node.empty (Decl { const = Nothing } (Node.empty (Tuple.first (t ""))) (Node.empty n) (Just v))


func : (String -> ( Type, a )) -> String -> List ( ArgType, String ) -> List (Node Statement) -> Node Declaration
func t n a s =
    Node.empty
        (FunctionDeclaration
            (Node.empty (Tuple.first (t "")))
            (Node.empty n)
            (a
                |> List.map
                    (\( an, av ) -> ( Node.empty an, Node.empty av ))
                |> Node.empty
            )
            s
        )


void : a -> ( Type, a )
void s =
    ( Tvoid, s )


float : a -> ( Type, a )
float s =
    ( Tfloat, s )


int : a -> ( Type, a )
int s =
    ( Tint, s )


vec2 : a -> ( Type, a )
vec2 s =
    ( Tvec2, s )


vec3 : a -> ( Type, a )
vec3 s =
    ( Tvec3, s )


vec4 : a -> ( Type, a )
vec4 s =
    ( Tvec4, s )


mat2 : a -> ( Type, a )
mat2 s =
    ( Tmat2, s )


in_ : (String -> ( Type, a )) -> a -> ( ArgType, a )
in_ t n =
    ( ArgIn (Node.empty (Tuple.first (t ""))), n )


out : (String -> ( Type, a )) -> a -> ( ArgType, a )
out t n =
    ( ArgOut (Node.empty (Tuple.first (t ""))), n )


var : String -> Node Expression
var v =
    Node.empty (Variable v)


f : Float -> Node Expression
f v =
    Node.empty (Float v)


i : Int -> Node Expression
i v =
    Node.empty (Int v)


negate_ : Node Expression -> Node Expression
negate_ e =
    Node.combine UnaryOperation (Node.empty Negate) e


return : Node Expression -> Node Statement
return v =
    Node.empty (Return v)


add : Node Expression -> Node Expression -> Node Expression
add l r =
    Node.combine3 BinaryOperation l (Node.empty Add) r


subtract : Node Expression -> Node Expression -> Node Expression
subtract l r =
    Node.combine3 BinaryOperation l (Node.empty Subtract) r


by : Node Expression -> Node Expression -> Node Expression
by l r =
    Node.combine3 BinaryOperation l (Node.empty By) r


div : Node Expression -> Node Expression -> Node Expression
div l r =
    Node.combine3 BinaryOperation l (Node.empty Div) r


assign : Node Expression -> Node Expression -> Node Statement
assign l r =
    expr (Node.combine3 BinaryOperation l (Node.empty Assign) r)


expr : Node Expression -> Node Statement
expr e =
    Node.empty (ExpressionStatement e)


dot : String -> String -> Node Expression
dot v w =
    Node.combine Dot (var v) (Node.empty w)


call : String -> List (Node Expression) -> Node Expression
call v args =
    Node.combine Call (var v) (Node.empty args)
