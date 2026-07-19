(* Monad Type *)
type 'a t = char list -> ('a * char list) option

(* Wrapper Function: always succeeds without consuming the input *)
val result : 'a -> 'a t

(* Wrapper Function: always returns None *)
val none : 'a t

(* Sequence Operator: sequences two parsers *)
val ( |*> ) : 'a t -> ('a -> 'b t) -> 'b t

(* Choice Operator: Runs the first parser, fallsback on the second *)
val ( <|> ) : 'a t -> 'a t -> 'a t

(* Parses a single character matching a predicate *)
val satisfy : (char -> bool) -> char t

(* Parses a specific character *)
val char : char -> char t

(* Parses a single digit and returns its integer value *)
val digit : int t

(* Sequences two parsers ignoring the result of the left/right parser *)
val ( |>> ) : 'a t -> 'b t -> 'b t
val ( <<| ) : 'a t -> 'b t -> 'a t
