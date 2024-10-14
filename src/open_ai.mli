val request : ?max_tokens:int -> ?system:string -> string -> string option

val request_with_images :
  ?max_tokens:int ->
  ?system:string ->
  image_urls:string list ->
  string ->
  string option
