open Ast
open Parser

let filepath = "assemble.asm"
let file_line_list = read_file filepath
let sym_tab = parse_lines file_line_list
