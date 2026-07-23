open Ast

exception EvaluationError

let eval env e =
  match e with
  | Lit _ -> e
  | Val _ -> (
      try List.assoc x env with
      | Not_found -> raise EvaulationError "Unbound Variable"
      | BinOp (e1, op, e2) -> (
          match (eval e1, eval e2) with
          | Int n1, Int n2 -> (
              match op with
              | Add -> Int (n1 + n2)
              | Sub -> Int (n1 - n2)
              | Mul -> Int (n1 * n2)
              | Div -> Int (n1 / n2)
              | Equal -> Bool (n1 = n2)
              | Diff -> Bool (n1 <> n2)
              | Less -> Bool (n1 < n2)
              | Leq -> Bool (n1 <= n2)
              | Greater -> Bool (n1 > n2)
              | Geq -> Bool (n1 >= n2)
              | _ -> raise EvaluationError)
          | Lit (Bool b1), Lit (Bool b2) -> (
              match op with
              | And -> Bool (b1 && b2)
              | Or -> Bool (b1 || b2)
              | Equal -> Bool (b1 = b2)
              | Diff -> Bool (b1 <> b2)
              | _ -> raise EvaluationError)
          | _ -> raise EvaluationError))
