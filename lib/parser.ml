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

let int = char '-' |>> nat
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
  char '+' |*> (fun _ -> return Add) <|> (char '-' |*> fun _ -> return Sub)

let mulop =
  char '*' |*> (fun _ -> return Mul) <|> (char '/' |*> fun _ -> return Div)

let chain_left op exp =
  exp |*> fun r ->
  many
    ( op |*> fun o ->
      exp |*> fun e -> return (o, e) )
  |*> fun rs ->
  return (List.fold_left (fun acc (o, e) -> BinOp (acc, o, e)) r rs)

let rec expr input = chain_left addop mul_expr input
and mul_expr input = chain_left mulop factor input

and factor input =
  (integer |*> (fun x -> return (Lit (Int x))) <|> parenthesized expr) input
