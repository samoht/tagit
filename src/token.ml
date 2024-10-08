open Astring

module Open_ai = struct
  let token = ref None
  let set x = token := Some x
  let token_env () = Sys.getenv_opt "OPEN_AI_KEY"

  let token_file () =
    let f = Sys.getenv "HOME" ^ "/.config/tagit/config" in
    let ic = open_in f in
    let n = in_channel_length ic in
    let str = really_input_string ic n in
    match String.cut ~sep:"=" str with
    | Some ("open_ai_key", key) -> String.trim key
    | Some _ | None ->
        failwith "cannot read open_ai_key in ~/.config/tagit/config"

  let t =
    lazy
      (match !token with
      | Some x -> x
      | None -> ( match token_env () with Some x -> x | None -> token_file ()))
end
