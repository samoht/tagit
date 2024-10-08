type t
type status = Skip | No_headers | Invalid_headers | File of t

val body : t -> string
val description : t -> string option
val tags : t -> string list option
val with_description : t -> string -> t
val with_tags : t -> string list -> t
val of_markdown : string -> status
val to_markdown : t -> string

module Query : sig
  val description : string -> string option
  val tags : string -> string list option
end

module Token : sig
  val set_open_ai_key : string -> unit
end
