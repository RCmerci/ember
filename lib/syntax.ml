type t = BOOL of bool
       | STRING of string
       | INT of int
       | FLOAT of float
       | ID of string
       | ARRAY of t list
       | LIST of t list
[@@deriving show]
