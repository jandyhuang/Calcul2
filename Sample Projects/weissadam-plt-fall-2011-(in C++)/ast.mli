(* Author: Adam Weiss, Bill Warner *)
type typespec =
    IntType
  | FloatType
  | StringType
  | TupleType
  | SetType
  | BoolType
  | NoType
  | TupleSeq of typespec list
  | SetSeq of typespec


type binop =
    Plus 
  | Minus
  | Times
  | Divide
  | NDivide
  | GrThan
  | LeThan
  | GrThanEq
  | LeThanEq
  | Equals
  | NotEquals
  | LogicalAnd
  | LogicalOr
  | Union
  | Cross
  | SetMinus
  | Intersect

type unop =
    Negative
  | Not
  | Cardinality

type expr =
    NullExpr
  | Assign   of string * expr
  | Binop    of expr * binop * expr
  | Unop     of unop * expr
  | Int      of int 
  | Boolean  of bool 
  | Float    of string
  | String   of string
  | Tuple    of expr list 
  | SetLiteral  of expr list
  | SetBuilder of expr * (string * expr) list 
  | SetRange of expr * expr
  | Id       of string
  | FuncCall of string * expr list  
    
type stmt =
    Block of stmt list
  | Decl of typespec * string * expr
  | Expr of expr 
  | Return of expr
  | While of expr * stmt
  | If of expr * stmt * stmt
  | Print of expr

type func_decl = {
  func_name : string;
  formals : (typespec * string) list;
  ret_type : typespec;
  body : stmt;
}

type program = {
  funcs   : func_decl list;
  globals : stmt list;
}

