open Astring

type t = {
  headers : string;
  description : string option;
  tags : string list option;
  body : string;
}

let body t = t.body
let description t = t.description
let tags t = t.tags
let with_description t d = { t with description = Some d }
let with_tags t ts = { t with tags = Some ts }

type status = Skip | No_headers | Invalid_headers | File of t

let tags_of_string s =
  let s = String.cuts ~sep:"," s in
  List.map String.trim s

let sep = "---\n"

let yaml_value ls key f =
  match List.assoc_opt key ls with
  | None -> None
  | Some (`String s) -> Some (f s)
  | Some _ -> None

(* this doesn't look great, but we do not want to edit existing
   headers, just remove the one we will most probably edit *)
let surgery key str =
  match String.cut ~sep:(key ^ ":") str with
  | None -> str
  | Some (pre, post) -> (
      let n = String.length post in
      let rec loop i =
        if i = n then None
        else if i + 1 = n && post.[i] = '\n' then None
        else if i + 1 < n && post.[i] = '\n' && i + 1 < n && post.[i + 1] <> ' '
        then Some i
        else loop (i + 1)
      in
      match loop 0 with
      | None -> pre
      | Some i ->
          let post = String.sub post ~start:(i + 1) ~stop:n in
          pre ^ String.Sub.to_string post)

let of_markdown s =
  match String.cut ~sep s with
  | None -> No_headers
  | Some ("", rest) -> (
      match String.cut ~sep rest with
      | None -> No_headers
      | Some (headers, body) -> (
          match Yaml.of_string headers with
          | Ok (`O meta) ->
              let description = yaml_value meta "description" Fun.id in
              let tags = yaml_value meta "tags" tags_of_string in
              let headers = surgery "description" (surgery "tags" headers) in
              File { headers; tags; description; body }
          | Ok _ -> Invalid_headers
          | Error (`Msg _) -> Invalid_headers))
  | Some _ -> No_headers

let to_markdown t =
  let descr =
    match t.description with
    | None -> ""
    | Some d -> Yaml.to_string_exn (`O [ ("description", `String d) ])
  in
  let tags =
    match t.tags with
    | None -> ""
    | Some ts ->
        let ts = String.concat ~sep:", " ts in
        Yaml.to_string_exn (`O [ ("tags", `String ts) ])
  in
  String.concat ~sep:"" [ sep; t.headers; tags; descr; sep; t.body ]

module Query = struct
  let summary_prompt =
    "Generate a concise, SEO-friendly HTML meta description (under 160 \
     characters) for a blog post on Tarides.com. The description should focus \
     on what Tarides contributed or achieved in the context of the specific \
     event, product, or technology being discussed. Avoid any promotional \
     language or vague statements. Clearly state the key facts or actions from \
     the post. Do not use 'Tarides ...' in the description as it's already \
     clear it's posted on Tarides blog."

  let tags_prompt blog_tags =
    Fmt.str
      "Analyze the given blog post text and pick the best at most 3 tags from \
       this list: %s. Each tag should be separated by a comma. Each tag should \
       accurately reflect key technical themes, topics, or focus areas \
       discussed in the  Ensure the tags are relevant to the post's core \
       message and align with popular search trends in the tech industry."
      (String.concat ~sep:", " blog_tags)

  let description ?(limit = 160) str =
    let rec aux i =
      match
        Open_ai.request ~max_tokens:(limit - i) ~system:summary_prompt str
      with
      | Some d when String.length d <= limit -> Some d
      | Some _ -> aux (i + 1)
      | None -> None
    in
    aux 0

  let tags ~valid_tags str =
    let tags_prompt = tags_prompt valid_tags in
    match Open_ai.request ~max_tokens:80 ~system:tags_prompt str with
    | Some s ->
        let tags = tags_of_string s in
        let tags = List.filter (fun t -> List.mem t valid_tags) tags in
        let tags = List.sort_uniq String.compare tags in
        Some tags
    | None -> None
end

module Token = struct
  let set_open_ai_key = Token.Open_ai.set
end
