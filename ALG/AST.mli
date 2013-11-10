type op=QE|EE|NE|SE|LE|S|L|PARA|INTERS|RELAT|SS|LL|SSE|LLE|PLUS|MINUS|MUL|DIV|PERC|TL|TS|TTL|TTS|TE|T|AND|OR


type var_type=
| INT_TYPE
|FLOAT
|ARRAY
|STRING
|VOID
|BOOLEAN
| POINT
|LINE
|POLYGON
|ELLIPSE


type expr=
	| NUM of float
	| INT of int
	| ID of string
	| Binop of expr * op * expr
	| Not of expr
	| Assign of string * expr
	| Call of string * expr list
	| String of string
	| PointEx of point
	| LineEx of line
	| PolygonEx of polygon
	| EllipseEx of ellipse



and point=
	| Point of expr * expr


and line=
	| Line of point * point
	| LineID of string * string

and polygon=
	| Polygon of point list
	|PolygonID of string list

and ellipse=
	| Ellipse of point * expr * expr
	| EllipseID of string * expr * expr



and stmt=
	| ExStmt of expr
	| Return of expr
	| Block of stmt list
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
	|BREAK
	|CONTINUE


and fdecl=
	{
		ftype:var_type;
		fname:string;
		formal_list: (var_type * string) list;
		locals: (var_type * string) list;
		body: stmt list
		
	}

type program=((var_type * string) list) * (fdecl list)


