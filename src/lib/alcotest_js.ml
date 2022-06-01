let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ()

let init ?extra_msg () =
  let open Js_of_ocaml in
  let open Brr in
  let state = Ansi.create () in

  let output =
    let out =
      El.div
        ([ El.h1 [ El.txt' "Alcotest-js" ] ]
        @
        match extra_msg with None -> [] | Some msg -> [ El.p [ El.txt' msg ] ])
    in
    El.set_at (Jstr.v "style")
      (Some
         (Jstr.v
            "font-family: monospace; color: #e8e8e8; background: #131212; \
             padding: 2em;"))
      out;
    El.(set_prop Prop.id (Jstr.v "output") out);
    let style = El.style [ El.txt' Ansi.css ] in
    El.append_children (Document.body G.document) [ style; out ];
    out
  in

  let get_or_make name =
    match Document.find_el_by_id G.document (Jstr.v name) with
    | Some v -> v
    | None ->
        let d = El.div [] in
        El.append_children output [ d ];
        El.(set_prop Prop.id (Jstr.v name) d);
        d
  in
  let append name s =
    let s = Ansi.process state s in
    let p = El.pre [] in
    El.to_jv p |> fun jv ->
    Jv.set jv "innerHTML" (Jv.of_string s);
    El.append_children (get_or_make name) [ p ]
  in
  Sys_js.set_channel_flusher stdout (fun content -> append "stdout" content);
  Sys_js.set_channel_flusher stderr (fun content -> append "stderr" content);
  ()

let run ~name suite =
  Irmin_test.Store.run name ~slow:true ~misc:[]
    ~sleep:Js_of_ocaml_lwt.Lwt_js.sleep
    [ (`Quick, suite) ]
