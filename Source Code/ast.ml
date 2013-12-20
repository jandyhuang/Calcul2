(* binary operators *)
type op = Add | Sub | Mult | Div | Pow | Deriv | Integ | Eq | Neq | Less | Leq |
Greater | Geq | And | Or

(* unary operators *)
type preop = Sqrt | Sin | Cos | Tan | ASin | ACos | ATan | Log | Ln | Not

(* expressions *)
type expr =
    Real of float
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
  | Math_func of string * string list * expr

(* declaration for functions *)
type func_decl = {
    fname : string;
    formals : string list;
    body : stmt list;
}

(* definition for type of program *)
type program = func_decl list

