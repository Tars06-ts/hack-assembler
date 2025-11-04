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

    let encode (js: Ast.Instr.jinst option) : int list =  
         match js with
                | None -> [0; 0; 0]
                | Some js -> jmp js

end

(*Destination module*)
module Dest = struct
    let dest (d: Ast.Instr.dest) : int list =
        match d with 
                | M -> [0; 0; 1]
                | D -> [0; 1; 0]
                | A -> [1; 0; 0]
                | MD -> [0; 1; 1]
                | AM -> [1; 0; 1]
                | AD -> [1; 1; 0]
                | AMD -> [1; 1; 1]

     let encode (ds: Ast.Instr.dest option) : int list =
         match ds with
                  | None -> [0; 0; 0]
                  | Some ds -> dest ds
end

module Computation = struct
    
    (*Constant def*)
    let const (c: Ast.Instr.const) : int list = 
            match c with
                    | Zero     -> [0;1;0;1;0;1;0]
                    | One      -> [0;1;1;1;1;1;1]
                    | MinusOne -> [0;1;1;1;0;1;0] 

    (*Unary module*)
    module Unary = struct
        let encodeR (r: Ast.Reg.r) : int list = 
            match r with 
                    | D-> [0;0;0;1;1]
                    | A-> [0;1;1;0;0]
                    | M-> [1;1;1;0;0]
                             
        let uEncode (u:Ast.Instr.unary) : int list = 
            match u with
                | ID     -> [0;0]
                | BNeg   -> [0;1]
                | UMinus -> [1;1]
                | Pred   -> [1;0]
                | Succ   -> [1;1]

        let succ (r: Ast.Reg.r) : int list =
              match r with 
                | D -> [0;0;1;1;1;1;1]
                | A -> [0;1;1;0;1;1;1]
                | M -> [1;1;1;0;1;1;1]

        let encode ((o,r) : Ast.Instr.unary * Ast.Reg.r) : int list =
            match o with 
                | Succ -> succ r
                |  _   -> uEncode o @ encodeR r

    end
    (*Binary Module*)
    module Binary = struct 
        let encodeB ((o,r) : Ast.Instr.binary * Ast.Reg.r2) : int list =
            let ambit = match r with 
                | A -> 0
                | _ -> 1
            in
            let opbits = match o with
                | Add      -> [0;0;0;0;1;0]
                | Sub      -> [0;1;0;0;1;1]
                | SubFrom  -> [0;0;0;1;1;1]
                | BAnd     -> [0;0;0;0;0;0]
                | BOr      -> [0;1;0;1;0;1]
            in
            ([ambit] @ opbits)
    end
    let encode (out: Ast.Instr.out): int list =
        match out with 
            | Const c  -> const c
            | Unary (o,r) -> Unary.encode (o,r)
            | Binary (o, r) -> Binary.encodeB (o, r)
end         
   
module Cinst = struct 
        let encode (c:Ast.Instr.cinst) : int list = 
                match c with {dest; out; jump} ->
                        [1;1;1] @ (Computation.encode out) @ (Dest.encode dest) @ (Jmp.encode jump)
end

