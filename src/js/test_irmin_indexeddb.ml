module Store =
  Irmin.Maker (Irmin_indexeddb.Content_store) (Irmin_indexeddb.Branch_store)

let store = Irmin_test.store (module Store) (module Irmin.Metadata.None)
let db_name = "irmin-browser-test"
let config = Irmin_indexeddb.config db_name
let init () = Irmin_indexeddb.Raw.delete_database db_name
let suite = Irmin_test.Suite.create ~name:"INDEXEDDB" ~init ~store ~config ()

let () =
  Alcotest_js.init ~extra_msg:"Seems incredibly slow..." ();
  Lwt.async @@ fun () -> Alcotest_js.run ~name:"irmin-indexeddb" suite
