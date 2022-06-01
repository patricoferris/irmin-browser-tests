let store = Irmin_test.store (module Irmin_mem) (module Irmin.Metadata.None)
let config = Irmin_mem.config ()
let init () = Lwt.return_unit
let suite = Irmin_test.Suite.create ~name:"MEM" ~init ~store ~config ()

let () =
  Alcotest_js.init ();
  Lwt.async @@ fun () -> Alcotest_js.run ~name:"irmin-mem" suite
