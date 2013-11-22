(* binary operators *)
type op = Add | Sub | Mult | Div | Pow | IntDiv | Mod | Equal | Deriv | Integ | Eq | Neq | Less | Leq |
Greater | Geq | And | Or

(* unary operators *)
type preop = Sqrt | Sin | Cos | Tan | ASin | ACos | ATan | Log | Ln | Not

(* vector type: used for derivation *)
type vector =
    Vector of float list

(* expressions *)
type expr =
    Num of int
  | Real of float
  | Id of string
  | Binop of expr * op * expr
  | PreUnaop of preop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

(* statements *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Output of expr


(* declaration for varibles *)
type var_decl = {
    name : string;
    value : float;
}

(* declaration for math functions *)
type math_func_decl = {
    fname : string;
    unknowns : string list;
    formula : expr list;
}

(* declaration for functions *)
type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
}

(* definition for type of program *)
type program = string list * func_decl list
