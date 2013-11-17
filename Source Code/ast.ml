type op = Add | Sub | Mult | Div | Pow | IntDiv | Mod | Equal | Deriv | Integ | Eq | Neq | Less | Leq |
Greater | Geq | And | Or

type preop = Sqrt | Sin | Cos | Tan | ASin | ACos | ATan | Log | Ln | Not

type vector =
    Vector of float list

type expr =
    Num of int
  | Real of float
  | Id of string
  | Binop of expr * op * expr
  | PreUnaop of preop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type var_decl = {
    name : string;
    value : float;
}

type math_func_decl = {
    fname : string;
    uknowns : expr list;
    formula : expr list;
}

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
}

type program = string list * func_decl list
