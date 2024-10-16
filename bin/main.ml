let setup_log style_renderer token level =
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  Fmt_tty.setup_std_outputs ?style_renderer ();
  match token with None -> () | Some t -> Tagit.Token.set_open_ai_key t

let color c = Fmt.(styled (`Fg c) string)
let yellow = color `Yellow
let green = color `Green
let red = color `Red
let bold = Fmt.(styled `Bold string)
let pr_skip file = Fmt.pr "%a %s\n%!" yellow "[SKIP]" file
let pr_error file = Fmt.pr "%a %s\n%!" red "[ERROR]" file
let pr_updated file = Fmt.pr "%a %s\n%!" green "[UPDATED]" file

let valid_tags =
  [
    "Cybersecurity";
    "OCaml";
    "Memory Safety";
    "Functional Programming";
    "Multicore";
    "Open Source";
    "Events";
    "Performance";
  ]

type flags = {
  in_place : bool;
  fixup : bool;
  force : bool;
  tags : bool;
  description : bool;
  alt : bool;
}

let process_one_file { in_place; fixup; force; tags; description; alt } file =
  let ic = open_in_bin file in
  let input = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match Tagit.of_markdown input with
  | Skip -> pr_skip file
  | No_headers | Invalid_headers -> pr_error file
  | File t ->
      let body = Tagit.body t in
      let with_description t =
        match Tagit.Query.description body with
        | None -> t
        | Some descr -> Tagit.with_description t descr
      in
      let with_tags t =
        match Tagit.Query.tags ~valid_tags body with
        | None -> t
        | Some tags -> Tagit.with_tags t tags
      in
      let with_alt t =
        match Tagit.image t with
        | None -> t
        | Some img -> (
            match Tagit.Query.alt img with
            | None -> t
            | Some alt -> Tagit.with_alt t alt)
      in
      let t =
        match Tagit.alt t with
        | None -> if alt then with_alt t else t
        | Some _ -> if alt && force then with_alt t else t
      in
      let t =
        match Tagit.description t with
        | None -> if description then with_description t else t
        | Some d ->
            if description && force then with_description t
            else if description && fixup && String.length d > 160 then
              with_description t
            else t
      in
      let t =
        match Tagit.tags t with
        | None -> if tags then with_tags t else t
        | Some ts ->
            if tags && force then with_tags t
            else if
              tags
              && fixup
              && List.exists (fun tag -> not (List.mem tag valid_tags)) ts
            then with_tags t
            else t
      in
      let output = Tagit.to_markdown t in
      if in_place then (
        if input = output then pr_skip file
        else
          let oc = open_out_bin file in
          output_string oc output;
          close_out oc;
          pr_updated file)
      else
        let () =
          if alt then
            match Tagit.alt t with
            | Some alt -> Fmt.pr "%a %s\n" bold "ALT:" alt
            | None -> ()
        in
        let () =
          if description then
            match Tagit.description t with
            | Some descr -> Fmt.pr "%a %s\n" bold "DESCRIPTION:" descr
            | None -> ()
        in
        let () =
          if tags then
            match Tagit.tags t with
            | Some tags ->
                Fmt.pr "%a: %s\n" bold "TAGS:" (String.concat ", " tags)
            | None -> ()
        in
        ()

let tagit () options files = List.iter (process_one_file options) files

open Cmdliner

let in_place =
  let doc = "Edit the input file in place." in
  Arg.(value & flag & info [ "i"; "in-place" ] ~doc)

let fixup =
  let doc = "Fixup descriptions (for instance if they are too long)." in
  Arg.(value & flag & info [ "fixup" ] ~doc)

let force =
  let doc = "Force update." in
  Arg.(value & flag & info [ "f"; "force" ] ~doc)

let tags =
  let doc = "Generate SEO-friendly tags for the blog post." in
  Arg.(value & flag & info [ "t"; "tags" ] ~doc)

let description =
  let doc = "Generate SEO-friendly description for the blog post." in
  Arg.(value & flag & info [ "d"; "description" ] ~doc)

let alt =
  let doc = "Generate SEO-friendly alt text for images." in
  Arg.(value & flag & info [ "a"; "alt" ] ~doc)

let flags =
  Term.(
    const (fun in_place fixup force tags description alt ->
        { in_place; fixup; force; tags; description; alt })
    $ in_place
    $ fixup
    $ force
    $ tags
    $ description
    $ alt)

let input_files =
  let doc = "Input file containing the blog post content." in
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)

let token =
  let doc =
    "The OpenAI token to use. By default it will try to read the one, stored \
     under `/.config/tagit/config`."
  in
  let env = Cmd.Env.info "OPEN_AI_KEY" in
  Arg.(value @@ opt (some string) None @@ info ~env ~doc [ "token" ])

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ token $ Logs_cli.level ())

(* The Cmdliner term representing the command *)
let term = Term.(const tagit $ setup_log $ flags $ input_files)

(* The command definition *)
let cmd =
  let info =
    let doc = "A tool to generate SEO tags and descriptions for blog posts." in
    Cmd.info "tagit" ~doc
  in
  Cmd.v info term

(* Running the command *)
let () = exit @@ Cmd.eval cmd
