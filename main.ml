let parse_only = ref false
let type_only = ref false

let usage = Printf.sprintf
  "Usage : %s program.c [-parse-only] [-type-only]"
  (Filename.basename Sys.argv.(0))

let speclist = [
  "-parse-only",Arg.Unit (fun () -> parse_only := true),
    "Exit after parsing";
  "-type-only",Arg.Unit (fun () -> type_only := true),
    "Exit after typing"
  ]

let args = ref []

let collect arg = args := arg :: !args

let () =
  Arg.parse speclist collect usage;
  match !args with
    | [] ->
      Printf.eprintf "No file to compile specified.\n%s\n" usage;
      exit 2;
    | [h] -> (try
      assert false
      with
        | Error.E _ -> exit 1
        | _ -> Printf.eprintf "Unexpected error.\n";
          exit 2)
    | _ ->
      Printf.eprintf "Too many arguments.\n%s\n" usage;
      exit 2;
