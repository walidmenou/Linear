type 'a t = char list -> ('a * char list) option

let result x = fun input -> Some (x, input)
let none = fun _ -> None

let ( |*> ) p k =
 fun input -> match p input with Some (x, rest) -> k x rest | _ -> None

let ( <|> ) p q = fun input -> match p input with None -> q input | r -> r

let satisfy p =
 fun input ->
  match input with [] -> None | c :: cs -> if p c then Some (c, cs) else None

let char c = satisfy (fun x -> x = c)

let digit =
  satisfy (function '0' .. '9' -> true | _ -> false) |*> fun c ->
  result (Char.code c - Char.code '0')

let ( |>> ) p q = p |*> fun _ -> q

let ( <<| ) p q =
  p |*> fun x ->
  q |*> fun _ -> result x
