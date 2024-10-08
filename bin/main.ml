let setup_log style_renderer token level =
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  Fmt_tty.setup_std_outputs ?style_renderer ();
  match token with None -> () | Some t -> Tagit.Token.set_open_ai_key t

let color c = Fmt.(styled (`Fg c) string)
let yellow = color `Yellow
let green = color `Green
let red = color `Red

let tagit () in_place tags descr file =
  let ic = open_in_bin file in
  let input = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match Tagit.of_markdown input with
  | Skip -> Fmt.pr "%a %s\n%!" yellow "[SKIP]" file
  | No_headers | Invalid_headers -> Fmt.pr "%a %s\n%!" red "[ERROR]" file
  | File t ->
      let body = Tagit.body t in
      let t =
        if descr then
          match Tagit.Query.description body with
          | None -> t
          | Some descr -> Tagit.with_description t descr
        else t
      in
      let t =
        if tags then
          match Tagit.Query.tags body with
          | None -> t
          | Some tags -> Tagit.with_tags t tags
        else t
      in
      let output = Tagit.to_markdown t in
      if in_place then (
        let oc = open_out_bin file in
        output_string oc output;
        close_out oc;
        Fmt.pr "%a %s\n%!" green "[UPDATED]" file)
      else
        let () =
          let mode = if descr then "query" else "current" in
          match Tagit.description t with
          | Some descr -> Fmt.pr "Description(%s): %s\n" mode descr
          | None -> ()
        in
        let () =
          let mode = if tags then "query" else "current" in
          match Tagit.tags t with
          | Some tags -> Fmt.pr "Tags(%s): %s\n" mode (String.concat ", " tags)
          | None -> ()
        in
        ()

open Cmdliner

let in_place =
  let doc = "Edit the input file in place." in
  Arg.(value & flag & info [ "i"; "in-place" ] ~doc)

let tags =
  let doc = "Generate SEO-friendly tags for the blog post." in
  Arg.(value & flag & info [ "t"; "tags" ] ~doc)

let description =
  let doc = "Generate SEO-friendly description for the blog post." in
  Arg.(value & flag & info [ "d"; "description" ] ~doc)

let input_file =
  let doc = "Input file containing the blog post content." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

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
let term =
  Term.(const tagit $ setup_log $ in_place $ tags $ description $ input_file)

(* The command definition *)
let cmd =
  let info =
    let doc = "A tool to generate SEO tags and descriptions for blog posts." in
    Cmd.info "tagit" ~doc
  in
  Cmd.v info term

(* Running the command *)
let () = exit @@ Cmd.eval cmd
