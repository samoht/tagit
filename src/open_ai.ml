open Cohttp

module Request = struct
  type type_text = { type_ : string; [@key "type"] text : string }
  [@@deriving yojson]

  type url = { url : string } [@@deriving yojson]

  type type_image_url = { type_ : string; [@key "type"] image_url : url }
  [@@deriving yojson]

  type part = Text of string | Image_url of string

  let part_to_yojson = function
    | Text t -> type_text_to_yojson { type_ = "text"; text = t }
    | Image_url i ->
        type_image_url_to_yojson
          { type_ = "image_url"; image_url = { url = i } }

  let part_of_yojson json =
    match type_text_of_yojson json with
    | Ok t -> Ok (Text t.text)
    | Error _ -> (
        match type_image_url_of_yojson json with
        | Ok t -> Ok (Image_url t.image_url.url)
        | Error _ as e -> e)

  type content = Text of string | Parts of part list
  type text = string [@@deriving yojson]
  type parts = part list [@@deriving yojson]

  let content_to_yojson = function
    | Text t -> text_to_yojson t
    | Parts ps -> parts_to_yojson ps

  let content_of_yojson json =
    match text_of_yojson json with
    | Ok t -> Ok (Text t)
    | Error _ -> (
        match parts_of_yojson json with
        | Ok ps -> Ok (Parts ps)
        | Error _ as e -> e)

  type message = {
    role : string;
    content : content;
    name : string option; [@default None]
  }
  [@@deriving yojson]

  let system_message content = { content; role = "system"; name = None }
  let user_message content = { content; role = "user"; name = None }

  type format = Text | Json_format | Json_schema [@@warning "-37"]

  let format_to_yojson : format -> Yojson.Safe.t = function
    | Text -> `Assoc [ ("type", `String "text") ]
    | _ -> failwith "format_of_yosjon: TODO"

  let format_of_yojson _ = failwith "format_to_yojson: TODO"

  type t = {
    model : string;
    messages : message list;
    store : bool option; [@default None]
    temperature : float option; [@default None]
    max_tokens : int option; [@default None]
    top_p : float option; [@default None]
    frequency_penalty : float option; [@default None]
    presence_penalty : float option; [@default None]
    response_format : format option; [@default None]
  }
  [@@deriving yojson]

  let v ?(model = "gpt-4o") ?temperature ?max_tokens ?top_p ?store
      ?frequency_penalty ?presence_penalty ?response_format ?system user =
    let system =
      match system with None -> [] | Some s -> [ system_message s ]
    in
    let user = [ user_message user ] in
    {
      model;
      store;
      messages = system @ user;
      temperature;
      max_tokens;
      top_p;
      frequency_penalty;
      presence_penalty;
      response_format;
    }
end

module Response = struct
  type finish_reason =
    | Stop
    | Length
    | Content_filter
    | Tools_call
    | Function_call

  let finish_reason_to_yojson : finish_reason -> Yojson.Safe.t = function
    | Stop -> `String "stop"
    | Length -> `String "length"
    | Content_filter -> `String "content_filter"
    | Tools_call -> `String "tools_call"
    | Function_call -> `String "function_call"

  let finish_reason_of_yojson = function
    | `String "stop" -> Ok Stop
    | `String "length" -> Ok Length
    | `String "content_filter" -> Ok Content_filter
    | `String "tools_call" -> Ok Tools_call
    | `String "function_call" -> Ok Function_call
    | _ -> Error "finish_reason_of_yojson"

  type message = {
    content : string option; [@default None]
    refusal : string option; [@default None]
    tools_calls : Yojson.Safe.t list; [@default []]
    role : string;
    function_call : Yojson.Safe.t list; [@default []]
  }
  [@@deriving yojson]

  type choice = {
    finish_reason : finish_reason;
    index : int;
    message : message;
    logprobs : Yojson.Safe.t option; [@default None]
  }
  [@@deriving yojson]

  type completion_token_details = {
    audio_tokens : int option; [@default None]
    reasoning_tokens : int option; [@default None]
  }
  [@@deriving yojson]

  type prompt_tokens_details = {
    audio_tokens : int option; [@default None]
    cached_tokens : int option; [@default None]
  }
  [@@deriving yojson]

  type usage = {
    completion_tokens : int;
    prompt_tokens : int;
    total_tokens : int;
    completion_tokens_details : completion_token_details;
    prompt_tokens_details : prompt_tokens_details;
  }
  [@@deriving yojson]

  type t = {
    id : string;
    choices : choice list;
    created : int;
    model : string;
    service_tier : string option; [@default None]
    system_fingerprint : string;
    object_ : string; [@key "object"]
    usage : usage;
  }
  [@@deriving yojson]
end

let uri = Uri.of_string "https://api.openai.com/v1/chat/completions"

module Backoff = struct
  let delay = ref 0.1
  let retry_count = ref 0

  let reset () =
    delay := max 0.1 (!delay /. 2.);
    retry_count := 0

  let block clock =
    incr retry_count;
    Eio.traceln "Backoff(%d): waiting for %.1f seconds." !retry_count !delay;
    Eio.Time.sleep clock !delay;
    delay := !delay *. 2.0
end

let rec make_request ~client ~clock ~sw req =
  let headers =
    Header.init () |> fun h ->
    Header.add h "Content-Type" "application/json" |> fun h ->
    Header.add h "Authorization" ("Bearer " ^ Lazy.force Token.Open_ai.t)
  in
  let req_str = req |> Request.to_yojson |> Yojson.Safe.to_string in
  let response, resp =
    Cohttp_eio.Client.post client ~headers ~sw uri
      ~body:(Cohttp_eio.Body.of_string req_str)
  in
  let resp_str = Eio.Flow.read_all resp in
  match response.status with
  | `OK ->
      Backoff.reset ();
      let response = Yojson.Safe.from_string resp_str in
      Response.of_yojson response
  | `Too_many_requests ->
      Backoff.block clock;
      make_request ~client ~clock ~sw req
  | s ->
      let err = Cohttp.Code.code_of_status s in
      Fmt.epr "REQUEST : %s\n%!" req_str;
      Fmt.epr "RESPONSE: %s\n%!" resp_str;
      Error (Fmt.str "Error: %d" err)

let authenticator =
  match Ca_certs.authenticator () with
  | Ok x -> x
  | Error (`Msg m) ->
      Fmt.failwith "Failed to create system store X509 authenticator: %s" m

let https =
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Error (`Msg msg) -> failwith ("tls configuration problem: " ^ msg)
    | Ok tls_config -> tls_config
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let run req =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client = Cohttp_eio.Client.make ~https:(Some https) env#net in
  let clock = env#clock in
  Eio.Switch.run @@ fun sw -> make_request ~client ~clock ~sw req

let replace_unicode_chars s =
  let buf = Buffer.create (String.length s) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec aux () =
    match Uutf.decode decoder with
    | `Uchar uchar ->
        let code = Uchar.to_int uchar in
        let ascii_char =
          match code with
          | 0x2019 -> '\''
          | 0x2018 -> '\''
          | 0x201C -> '"'
          | 0x201D -> '"'
          | _ ->
              if code < 128 then Char.chr code (* ASCII character *)
              else '?' (* Replace other non-ASCII chars with '?' *)
        in
        Buffer.add_char buf ascii_char;
        aux ()
    | `End -> ()
    | `Malformed _ ->
        Buffer.add_char buf '?';
        (* Handle malformed sequences *)
        aux ()
    | `Await -> assert false (* Not used with `String` source *)
  in
  aux ();
  Buffer.contents buf

let normalise s =
  let s = Uunf_string.normalize_utf_8 `NFD s in
  replace_unicode_chars s

let request_content ?max_tokens ?system c =
  let system : Request.content option =
    match system with None -> None | Some t -> Some (Text t)
  in
  let request = Request.v ?max_tokens ?system c in
  match run request with
  | Error e -> failwith e
  | Ok r -> (
      match (List.hd r.choices).message.content with
      | None -> None
      | Some s -> Some (normalise s))

let request ?max_tokens ?system txt =
  request_content ?max_tokens ?system (Text txt)

(* FIXME: *)
let prefix = "https://tarides.com/blog/images/"

let request_with_images ?max_tokens ?system ~image_urls txt =
  let image_url img = Request.Image_url (prefix ^ img) in
  let images = List.map image_url image_urls in
  request_content ?max_tokens ?system (Parts (Text txt :: images))
