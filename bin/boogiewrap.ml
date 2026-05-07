open Containers

let error_re =
  let e = {|(.+\.bpl)\(([0-9]+),([0-9]+)\): Error: (.*)|} in
  Re.Pcre.regexp e

let related_loc =
  let e = {|(.+\.bpl)\(([0-9]+),([0-9]+)\): Related location: (.*)|} in
  Re.Pcre.regexp e

let trace_loc =
  let e = {|Execution trace:\n(    .*\n)+|} in
  Re.Pcre.regexp e

let summary_loc =
  let e = {|Boogie program verifier finished.*|} in
  Re.Pcre.regexp e

type err_info = {
  msg : string;
  line : int;
  col : int;
  filename : string;
  related : err_info option;
  trace : string;
}

let line_of_err m =
  let input = Pp_loc.Input.file m.filename in
  let beginloc = Pp_loc.Position.of_line_col m.line m.col in
  let beginoff = Pp_loc.Position.to_offset input beginloc in
  let e =
    CCIO.with_in m.filename (fun i ->
        In_channel.seek i (Int64.of_int @@ beginoff);
        while
          match In_channel.input_char i with
          | Some '\n' -> false
          | None -> false
          | _ -> true
        do
          ()
        done;
        let p = In_channel.pos i |> Int64.to_int in
        p)
  in
  let eloc = Pp_loc.Position.of_offset e in
  [ (beginloc, Pp_loc.Position.shift eloc (-1)) ]

let format_ploc input =
 fun f ->
  Pp_loc.setup_highlight_tags f
    ~single_line_underline:
      {
        open_tag =
          (fun _ -> Format.ANSI_codes.string_of_style_list [ `Bold; `FG `Red ]);
        close_tag = (fun _ -> Format.ANSI_codes.string_of_style `Reset);
      }
    ();

  Pp_loc.pp ~input ~max_lines:5 f

let show_err_info (m : err_info) =
  let input = Pp_loc.Input.file m.filename in
  let ploc fmt loc =
    Pp_loc.setup_highlight_tags fmt
      ~single_line_underline:
        {
          open_tag =
            (fun _ ->
              Format.ANSI_codes.string_of_style_list [ `Bold; `FG `Red ]);
          close_tag = (fun _ -> Format.ANSI_codes.string_of_style `Reset);
        }
      ();
    Pp_loc.pp ~input fmt loc
  in
  let relatedloc =
    match m.related with
    | Some e ->
        Format.asprintf "\nRelated: %s%a%a" e.msg Format.pp_force_newline ()
          ploc (line_of_err e)
    | None -> Format.asprintf ""
  in
  Format.asprintf "Error: %s%a%a%s%s" m.msg Format.pp_force_newline () ploc
    (line_of_err m) m.trace relatedloc

let get_error msg =
  let mainmsg = Re.exec summary_loc msg |> fun g -> Re.Group.get g 0 in
  let errs =
    let m = Re.all error_re msg in
    m
    |> List.map (fun g ->
        begin
          let filename = Re.Group.get g 1 in
          let line = Re.Group.get g 2 |> Int.of_string_exn in
          let col = Re.Group.get g 3 |> Int.of_string_exn in
          let emsg = Re.Group.get g 4 in
          let _, pos = Re.Group.offset g 0 in
          let trace =
            Re.exec_opt trace_loc ~pos msg
            |> Option.map (fun g -> Re.Group.get g 0)
            |> Option.map (fun g -> "\n" ^ g)
            |> Option.get_or ~default:""
          in
          let rloc = Re.exec_opt related_loc ~pos msg in
          let related =
            Option.bind rloc (function
              | g when fst (Re.Group.offset g 0) - pos < 5 ->
                  let filename = Re.Group.get g 1 in
                  let line = Re.Group.get g 2 |> Int.of_string_exn in
                  let col = Re.Group.get g 3 |> Int.of_string_exn in
                  let msg = Re.Group.get g 4 in
                  Some { msg; line; col; filename; related = None; trace = "" }
              | _ -> None)
          in
          { msg = emsg; line; col; filename; related; trace }
        end)
  in
  (mainmsg, errs)

let print_error msg =
  try
    let msg, ers = get_error msg in
    print_endline msg;
    print_endline "";
    List.iter (fun e -> print_endline @@ show_err_info e) ers
  with Not_found -> print_endline msg

let () =
  let a = Sys.argv |> Array.to_list |> List.tl |> String.concat " " in
  print_endline a;
  let ou, er, i = CCUnix.call ~stdin:(`Str "blah") "boogie %s" a in
  print_error ou
