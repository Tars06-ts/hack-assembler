open Assembler
open Parser

open Stdlib

(*let read_file_to_list (file_path : string) : string list =
  try
    (* In_channel.with_open_text handles file closing automatically *)
    In_channel.with_open_text file_path (fun ic ->
      (* Fold over lines: 'line' is the current line, 'acc' is the list built so far.
         We prepend the line to the accumulator (::) to keep the list in order,
         as fold_lines returns lines in file order. *)
      In_channel.fold_lines ic (fun line acc -> line :: acc) []
    )
    (* The list is built in reverse order (since we prepend), so we reverse it once at the end. *)
    |> List.rev
  with
  | Sys_error msg ->
    Printf.eprintf "Error reading file: %s\n" msg;
    exit 1
  | _ ->
    Printf.eprintf "An unknown error occurred while reading the file.\n";
    exit 1

let get_out (input_file: string) : string =
  if Filename.check_suffix input_file ".asm" then
    Filename.chop_suffix input_file ".asm" ^ ".hack"
  else
    failwith "Input file must have a .asm extension"

let main () =
  (* Check if a filename was provided as a command line argument *)
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <input_file.asm>\n" Sys.argv.(0);
    exit 1
  end else
    let input_file = Sys.argv.(1) in
    let output_file = get_out input_file in

    (* 1. Read the input file into a list of lines *)
    let raw_lines = read_file_to_list input_file in

    (* 2. Parse the lines into a raw program AST (List of string Program.stmt) *)
    let parsed_program = Parser.parse_lines raw_lines in

    (* 3. Assemble the program using the two-pass process (handled by final_proj) *)
    let machine_code = Parser.final_proj parsed_program in

    (* 4. Write the binary machine code to the output file *)
    try
      Out_channel.with_open_text output_file (fun oc ->
        Out_channel.output_string oc machine_code
      );
      Printf.printf "Successfully assembled %s into %s\n" input_file output_file
    with
    | Sys_error msg ->
      Printf.eprintf "Error writing file %s: %s\n" output_file msg;
      exit 1
    | _ ->
      Printf.eprintf "An unknown error occurred while writing the file.\n";
      exit 1

(* Execute the main function *)
let () = main () 
*)
let read_file_to_list (file_path : string) : string list =
  try
    (* In_channel.with_open_text handles file closing automatically *)
    In_channel.with_open_text file_path (fun ic ->
      (* Fold over lines: 'line' is the current line, 'acc' is the list built so far.
         We prepend the line to the accumulator (::) to keep the list in order,
         as fold_lines returns lines in file order. *)
      let lines = ref [] in
      try
        while true do
          lines := input_line ic :: !lines
        done;
        []
      with End_of_file -> List.rev !lines
    )
  with
  | Sys_error msg ->
    Printf.eprintf "Error reading file: %s\n" msg;
    exit 1
  | _ ->
    Printf.eprintf "An unknown error occurred while reading the file.\n";
    exit 1

let get_out (input_file: string) : string =
  if Filename.check_suffix input_file ".asm" then
    Filename.chop_suffix input_file ".asm" ^ ".hack"
  else
    failwith "Input file must have a .asm extension"

let main () =
  (* Check if a filename was provided as a command line argument *)
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <input_file.asm>\n" Sys.argv.(0);
    exit 1
  end else
    let input_file = Sys.argv.(1) in
    let output_file = get_out input_file in

    (* 1. Read the input file into a list of lines *)
    let raw_lines = read_file_to_list input_file in

    (* 2. Parse the lines into a raw program AST (List of string Program.stmt) *)
    let parsed_program = parse_lines raw_lines in

    (* 3. Assemble the program using the two-pass process (handled by final_proj) *)
    let machine_code = final_proj parsed_program in

    (* 4. Write the binary machine code to the output file *)
    try
      let oc = open_out output_file in
      output_string oc machine_code;
      close_out oc;
      Printf.printf "Successfully assembled %s into %s\n" input_file output_file
    with
    | Sys_error msg ->
      Printf.eprintf "Error writing file %s: %s\n" output_file msg;
      exit 1
    | _ ->
      Printf.eprintf "An unknown error occurred while writing the file.\n";
      exit 1

(* Execute the main function *)
let () = main ()

