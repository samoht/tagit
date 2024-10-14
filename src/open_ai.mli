val request : ?max_tokens:int -> ?system:string -> string -> string option

val request_parts :
  ?max_tokens:int -> ?system:string -> string -> string list -> string option
