open Hack_assembler
open Ast
open Parser
open Machine 

let read_file_to_list (file_path : string) : string list =
  try
    (* In_channel.with_open_text handles file closing automatically *)
    In_channel.with_open_text file_path (fun ic ->
      (* Fold over lines: 'line' is the current line, 'acc' is the list built so far.
         We prepend the line to the accumulator (::) to keep the list in order,
         as fold_lines returns lines in file order. *)
      In_channel.fold_lines ic ~init:[] ~f:(fun line acc -> line :: acc)
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

