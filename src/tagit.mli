type t
type status = Skip | No_headers | Invalid_headers | File of t

val body : t -> string
val description : t -> string option
val tags : t -> string list option
val image : t -> string option
val alt : t -> string option
val with_description : t -> string -> t
val with_tags : t -> string list -> t
val with_alt : t -> string -> t
val of_markdown : string -> status
val to_markdown : t -> string

module Query : sig
  val description : ?limit:int -> string -> string option
  val tags : valid_tags:string list -> string -> string list option
  val alt : string -> string option
end

module Token : sig
  val set_open_ai_key : string -> unit
end
