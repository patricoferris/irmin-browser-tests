open Lwt.Syntax
open Lwt.Infix
open Irmin_client_jsoo
module Codec = Irmin_server_internal.Conn.Codec.Bin
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_jsoo.Make_ext (Codec) (Store)

let test name f client _switch () =
  Logs.debug (fun l -> l "Running: %s" name);
  f client

let suite client all =
  List.map
    (fun (name, speed, f) ->
      Alcotest_lwt.test_case name speed (test name f client))
    all

module Info = Info (Client.Info)

let info = Info.v

module type R = sig
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let client = Client.connect ~uri:R.uri ()
  let config = Irmin_client_jsoo.Store.config R.uri
  let client () = client >>= fun client -> Client.dup client

  let clean () =
    let* client = client () in
    Client.Branch.remove client "main" >|= Error.unwrap "remove"

  let init () =
    let* client = client () in
    Client.Branch.remove client "main" >|= Error.unwrap "remove"

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_jsoo.Store.Make (X)

  let suite =
    Irmin_test.Suite.create ~name:R.kind ~init ~clear_supported:true
      ~store:(module Store)
      ~config ~clean ()
end

let kind, uri = ("Websocket", Uri.of_string "ws://localhost:9090/ws")
let client = Client.connect ~uri ()
let client () = client >>= fun client -> Client.dup client

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ty t =
  Alcotest.testable
    (Fmt.using (Irmin.Type.to_string t) Fmt.string)
    (fun a b -> Irmin.Type.(unstage (equal t)) a b)

let ping () =
  let open Client in
  let* client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let+ r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

let set () =
  let open Client in
  let* client = client () in
  let info = info "test: set" in
  let* r = set ~info client [ "a"; "b"; "c" ] "123" in
  let () = Alcotest.(check (result unit error)) "set" (Ok ()) r in
  let+ r2 = find client [ "a"; "b"; "c" ] in
  Alcotest.(check (result (option string) error)) "get" (Ok (Some "123")) r2

let get_missing () =
  let open Client in
  let* client = client () in
  let+ r = find client [ "missing" ] in
  Alcotest.(check (result (option string) error)) "get_missing" (Ok None) r

let tree () =
  let open Client in
  let* client = client () in
  let tree = Tree.empty client in
  let* local = Tree.to_local tree >|= Error.unwrap "local" in
  Alcotest.(check (ty Tree.Local.t)) "empty tree" (Tree.Local.empty ()) local;
  let* tree = Tree.add tree [ "x" ] "foo" >|= Error.unwrap "x" in
  let* tree = Tree.add tree [ "y" ] "bar" >|= Error.unwrap "y" in
  let* local = Tree.to_local tree >|= Error.unwrap "local x, y" in
  let* local' =
    Tree.Local.(add (empty ()) [ "x" ] "foo" >>= fun x -> add x [ "y" ] "bar")
  in
  Alcotest.(check (ty Tree.Local.t)) "x, y" local' local;
  let* res = set_tree ~info:(info "set_tree") client [ "tree" ] tree in
  Alcotest.(check bool "set_tree") true (Result.is_ok res);
  let* tree = find_tree client Path.empty >|= Error.unwrap "find_tree" in
  let tree = Option.get tree in
  let+ res = set_tree ~info:(info "set_tree") client [ "tree" ] tree in
  Alcotest.(check bool "set_tree") true (Result.is_ok res)

let misc =
  let run f () = f () in
  [
    ("ping", `Quick, run ping);
    ("set", `Quick, run set);
    ("get_missing", `Quick, run get_missing);
    ("tree", `Quick, run tree);
  ]

let misc = [ ("misc", misc) ]

module Websocket = Make (struct
  let kind, uri = (kind, uri)
end)

let () =
  Alcotest_js.init
    ~extra_msg:("Requires irmin-server listening on " ^ Uri.to_string uri)
    ();
  let tests = [ (`Quick, Websocket.suite) ] in
  Lwt.async @@ fun () ->
  Irmin_test.Store.run "irmin-client-jsoo" ~misc
    ~sleep:Js_of_ocaml_lwt.Lwt_js.sleep tests
