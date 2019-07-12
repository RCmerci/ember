open Core
open Lib
let _ =
  let lexbuf = Lexing.from_string "(let ((x '(1 3 5 7 9))) (do ((x x (cdr x)) (sum 0 (+ sum (car x)))) ((null? x) sum)))" in
  let syntax = try Parser.prog Lexer.read lexbuf  with
    | Parser.Error as e -> Out_channel.printf "cnum: %d\n" lexbuf.lex_start_p.pos_cnum;raise e
  in
  Syntax.show (Option.value_exn syntax) |> Out_channel.print_endline
