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
    | [file] -> (try
        let h = open_in file in
        let lexbuf = Lexing.from_channel h in
        let ast = Parser.prog Lexer.token lexbuf in
        if !parse_only then exit 0;
        let tast = Typing.type_prog ast in
        exit 0;
      with
        | Error.E (sp,ep,s) -> Error.prerr sp ep s; exit 1;
        | _ -> Printf.eprintf "Unexpected error.\n";
          exit 2)
    | _ ->
      Printf.eprintf "Too many arguments.\n%s\n" usage;
      exit 2;
