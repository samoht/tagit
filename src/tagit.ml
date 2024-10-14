open Astring

type t = {
  headers : string;
  description : string option;
  tags : string list option;
  image : string option;
  alt : string option;
  body : string;
}

let body t = t.body
let description t = t.description
let tags t = t.tags
let image t = t.image
let alt t = t.alt
let with_description t d = { t with description = Some d }
let with_tags t ts = { t with tags = Some ts }
let with_alt t alt = { t with alt = Some alt }

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
              let image = yaml_value meta "image" Fun.id in
              let alt = yaml_value meta "image-alt" Fun.id in
              let headers =
                headers
                |> surgery "description"
                |> surgery "tags"
                |> surgery "image"
                |> surgery "image-alt"
              in
              File { headers; tags; description; body; image; alt }
          | Ok _ -> Invalid_headers
          | Error (`Msg _) -> Invalid_headers))
  | Some _ -> No_headers

let to_markdown t =
  let yaml key value = Yaml.to_string_exn (`O [ (key, `String value) ]) in
  let descr =
    match t.description with None -> "" | Some d -> yaml "description" d
  in
  let tags =
    match t.tags with
    | None -> ""
    | Some ts -> yaml "tags" (String.concat ~sep:", " ts)
  in
  let image = match t.image with None -> "" | Some i -> yaml "image" i in
  let image_alt =
    match t.alt with None -> "" | Some a -> yaml "image-alt" a
  in
  String.concat ~sep:""
    [ sep; t.headers; image; image_alt; tags; descr; sep; t.body ]

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

  let alt img =
    let descr =
      "Describe the visual content of the image, detailing any objects, \
       people, or text visible. Avoid interpretations and focus only on the \
       observable elements. Be concise but specific enough for someone who \
       cannot see the image to understand its components. Use at most 50 \
       characters."
    in
    Open_ai.request_with_images descr ~image_urls:[ img ]
end

module Token = struct
  let set_open_ai_key = Token.Open_ai.set
end
