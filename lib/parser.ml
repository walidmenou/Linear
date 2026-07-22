open Util
open Ast

type 'a t = char list -> ('a * char list) option

let run p s = explode s |> p |> Option.get |> fst
let return x = fun input -> Some (x, input)
let none = fun _ -> None

let ( |*> ) p k =
 fun input -> match p input with Some (x, rest) -> (k x) rest | _ -> None

let ( <|> ) p q = fun input -> match p input with None -> q input | r -> r
let ( |>> ) p q = p |*> fun _ -> q

let ( <<| ) p q =
  p |*> fun x ->
  q |*> fun _ -> return x

let satisfies p =
 fun input ->
  match input with [] -> None | c :: cs -> if p c then Some (c, cs) else None

let char c = satisfies (fun x -> x = c)

let digit =
  satisfies (function '0' .. '9' -> true | _ -> false) |*> fun c ->
  return (Char.code c - Char.code '0')

let rec many (p : 'a t) : 'a list t = some p <|> return []

and some (q : 'a t) : 'a list t =
  q |*> fun x ->
  many q |*> fun xs -> return (x :: xs)

let map f p = p |*> fun r -> return (f r)
let maybe p = map (fun r -> Some r) p <|> return None
let sepby p sep = many (p <<| sep)
let spaces = many (char ' ' <|> char '\n' <|> char '\t')
let token p = p <<| spaces

let nat =
  some digit |*> fun xs ->
  return (List.fold_left (fun acc x -> (acc * 10) + x) 0 xs)

let int = char '-' |>> nat |*> (fun x -> return (-x)) <|> nat
let natural = token nat
let integer = token int

let keyword s =
  let rec check = function [] -> return () | c :: cs -> char c |>> check cs in
  token (check (explode s))

let between p1 p2 p = p1 |>> p <<| p2
let parenthesized p = between (token (char '(')) (token (char ')')) p
let alpha = satisfies (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

let alphanumeric =
  satisfies (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false)

let ident =
  token
    ( alpha |*> fun x ->
      many alphanumeric |*> fun xs -> return (ltos (x :: xs)) )

let int_lit = integer |*> fun x -> return (Lit (Int x))

let bool_lit =
  keyword "true" |>> return (Lit (Bool true))
  <|> (keyword "false" |>> return (Lit (Bool false)))

let unit_lit = keyword "()" |>> return (Lit Unit)

let addop =
  keyword "+" |*> (fun _ -> return Add) <|> (char '-' |*> fun _ -> return Sub)

let mulop =
  keyword "*" |*> (fun _ -> return Mul) <|> (char '/' |*> fun _ -> return Div)

let eqop = keyword "=" |*> fun _ -> return Equal

let cmpop =
  keyword "<="
  |*> (fun _ -> return Leq)
  <|> (keyword ">=" |*> fun _ -> return Geq)
  <|> (keyword "<" |*> fun _ -> return Less)
  <|> (keyword ">" |*> fun _ -> return Greater)

let chain_left op_p exp_p =
  exp_p |*> fun r ->
  many
    ( op_p |*> fun op ->
      exp_p |*> fun exp -> return (op, exp) )
  |*> fun rs ->
  return (List.fold_left (fun acc (op, exp) -> BinOp (acc, op, exp)) r rs)

let cmp_helper first pairs =
  let cmps, _ =
    List.fold_left
      (fun (cmps, prev) (op, exp) -> (BinOp (prev, op, exp) :: cmps, exp))
      ([], first) pairs
  in
  match List.rev cmps with
  | [] -> first
  | c :: cs -> List.fold_left (fun acc cmp -> BinOp (acc, And, cmp)) c cs

let chain_cmps op_p exp_p =
  exp_p |*> fun first ->
  many
    ( op_p |*> fun op ->
      exp_p |*> fun exp -> return (op, exp) )
  |*> fun pairs -> return (cmp_helper first pairs)

let val_expr = ident |*> fun x -> return (Val x)
let lit_expr = int_lit <|> bool_lit <|> unit_lit

let rec arith_expr input = chain_left eqop add_expr input
and add_expr input = chain_left addop mul_expr input
and mul_expr input = chain_left mulop atom_expr input
and atom_expr input = (lit_expr <|> val_expr <|> parenthesized expr) input
and cmp_expr input = chain_cmps cmpop arith_expr input

and app_expr input =
  ( atom_expr |*> fun f ->
    many atom_expr |*> fun args ->
    return (List.fold_left (fun acc arg -> App (acc, arg)) f args) )
    input

and if_expr input =
  ( keyword "if" |>> expr |*> fun exp1 ->
    keyword "then" |>> expr |*> fun exp2 ->
    keyword "else" |>> expr |*> fun exp3 -> return (If (exp1, exp2, exp3)) )
    input

and fun_expr input =
  ( keyword "fun" |>> ident |*> fun id ->
    keyword "->" |>> expr |*> fun exp -> return (Fun (id, exp)) )
    input

and let_expr input =
  ( keyword "let" |>> ident |*> fun id ->
    keyword "=" |>> expr |*> fun exp1 ->
    keyword "in" |>> expr |*> fun exp2 -> return (Let (id, exp1, exp2)) )
    input

and expr input =
  (if_expr <|> fun_expr <|> let_expr <|> arith_expr <|> app_expr) input
