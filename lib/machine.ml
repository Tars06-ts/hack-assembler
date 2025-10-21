(* -- Hack assembly to binary --  *)

(*Jump Module*)
module Jmp = struct
    let jmp (j: Ast.Instr.jinst) : int list =
        match j with 
                | JGT -> [ 0; 0; 1]
                | JEQ -> [ 0; 1; 0]
                | JGE -> [ 0; 1; 1]
                | JLT -> [ 1; 0; 0]
                | JNE -> [ 1; 0; 1]
                | JLE -> [ 1; 1; 0]
                | JMP -> [ 1; 1; 1]
end

(*Destination module*)
module Dest = struct
    let dest (d: Ast.Reg.r) : int list =
        match d with 
                | M -> [0; 0; 1]
                | D -> [0; 1; 0]
                | A -> [1; 0; 0]
end

(*Constant module*)
module Const = struct
    let const (c: Ast.Instr.const) : int list = 
        match c with
                | Zero     -> [0;1;0;1;0;1;0]
                | One      -> [0;1;1;1;1;1;1]
                | MinusOne -> [0;1;1;1;0;1;0] 
end
                             


let jmp = function
        | JGT -> 0b100
        | JEQ -> 0b010
        | JGE -> 0b110
        | JLT -> 0b001
        | JNE -> 0b101
        | JLE -> 0b011
        | JMP -> 0b111 

