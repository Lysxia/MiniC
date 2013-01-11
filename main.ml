(** Mini-C Compiler **)
(* Li-yao Xia *)
open Print_mips

exception Interrupt of int

let interrupt n = raise (Interrupt n)

let fstdout = Format.std_formatter

let parse_only = ref false
let type_only = ref false
let batch = ref false
let output = ref true
let print = ref false
let is = ref false

let usage = Printf.sprintf
  "Usage : %s program.c [-parse-only] [-type-only]"
  (Filename.basename Sys.argv.(0))

let speclist = [
  "-parse-only",Arg.Set parse_only,
    "Exit after parsing";
  "-type-only",Arg.Set type_only,
    "Exit after typing";
  "-no-output",Arg.Clear output,
    "Do not write compiled result";
  "-batch",Arg.Set batch,
    "Compile multiple files (separately)";
  "-print", Arg.Set print,
    "Print compiled tree when -type-only is enabled";
  ]

let args = ref []

let collect arg = args := arg :: !args

let reset () =
  Iselect.reset ()

let compile file =
  let h = open_in file in
  let lexbuf = Lexing.from_channel h in
  try
    reset ();
    let ast = Parser.prog Lexer.token lexbuf in
    close_in h;
    if !parse_only then interrupt 0;
    let tast = Typing.type_prog ast in
    if !type_only
      then begin
        if !print then Ast_printer.print_tfile fstdout tast;
        interrupt 0
      end;
    let f = Filename.basename file in
    let f = if Filename.check_suffix f ".c"
      then (Filename.chop_suffix f ".c")^".s"
      else f^".s" in
    let h = open_out f in
    print_prog h tast;
    0
  with
    | Error.E (sp,ep,s) -> close_in h; Error.prerr file sp ep s; 1
    | Parser.Error -> close_in h; Error.syntax file lexbuf; 1
    | Interrupt n -> n

let () =
  try 
    Arg.parse speclist collect usage;
    let rec main = function
      | [] -> if not !batch
        then begin
          Printf.fprintf stdout "No file to compile specified.\n%s\n%!" usage;
          exit 2;
        end
        else exit 0;
      | file::t when !batch ->
          let exit = compile file in
          Printf.printf "Exit %d from file \"%s\"\n%!"
          exit file;
          main t
      | [file] -> exit (compile file)
      | _ ->
          Printf.fprintf stdout "Too many arguments.\n%s\n%!" usage;
          exit 2;
    in main !args
  with
    | Interrupt 2 -> Printf.fprintf stdout "(╯°^°）╯︵ ┻━┻ Unexpected error.\n%!"; exit 2
