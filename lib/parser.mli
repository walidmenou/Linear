type 'a t = char list -> ('a * char list) option
(** Monad Type *)

val return : 'a -> 'a t
(** Wrapper Function: always succeeds without consuming the input *)

val none : 'a t
(** Wrapper Function: always returns None *)

val ( |*> ) : 'a t -> ('a -> 'b t) -> 'b t
(** Sequence Operator: sequences two parsers *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** Choice Operator: Runs the first parser, fallsback on the second *)

val satisfies : (char -> bool) -> char t
(** Parses a single character matching a predicate *)

val char : char -> char t
(** Parses a specific character *)

val digit : int t
(** Parses a single digit and returns its integer value *)

val ( |>> ) : 'a t -> 'b t -> 'b t
(** Sequences two parsers ignoring the result of the left/right parser *)

val ( <<| ) : 'a t -> 'b t -> 'a t

val many : 'a t -> 'a list t
(** Repeats the given parser zero or more times *)

val some : 'a t -> 'a list t
(** Repeats the given parser one or more times *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Applies the given function to the result of the parser *)

val maybe : 'a t -> 'a option t
(** Always succeeds, returns `None` if the given parser fails and Some x if it succeds *)

val number : int t
(** Parses a string representing a number and converts it to an integer *)

val spaces : char list t
(** Parses a sequence of contiguous spaces *)

val token : 'a t -> 'a t
(** Runs the parser and consumes all spaces that come after it *)
val keyword : string -> unit t
(** Parses the given keyword string *)

val between : 'a t -> 'b t -> 'c t -> 'c t
(** Parses the expression between the first two parsers *)

val parenthesized : 'a t -> 'a t
(** Parses expressions of the form: `( e )` *)

val alpha : char t
(** Parses a lowercase or uppercase character *)

val alphanumeric : char t
(** Parses a lowercase or uppercase character or a digit *)

val ident : string t
(** parses an identifier: alpha character followed by alphanumeric characters *)

val int_lit : Ast.expr t
(** Parses an integer Literal expression, e.g: "123" *)

val bool_lit : Ast.expr t
(** Parses an boolean Literal expression, e.g: "true"*)

val unit_lit : Ast.expr t
(** Parses () *)
