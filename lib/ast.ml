module Reg = struct
    type r = A | M | D
    type r2 = A | M
end

module Instr = struct
    type jinst = JGT
               | JEQ
               | JGE
               | JLT
               | JNE
               | JLE
               | JMP
               
    type const = Zero 
               | One 
               | MinusOne

    type unary = BNeg 
               | UMinus
               | Succ
               | Pred 
               | ID

    type binary = Add 
                | Sub 
                | SubFrom 
                | BAnd 
                | BOr

    type out = Const of const
             | Unary of unary*Reg.r
             | Binary of binary*Reg.r2

    type dest = M
    	      | D
	          | MD
	          | A
     	      | AM
    	      | AD      
              | AMD
              

    type cinst = { dest : dest option;
                   out  : out;
                   jump        : jinst option
                 }

    type 'v t = A of 'v
              | C of cinst
    
    let map f = function
              | A x -> A (f x)
              | C i -> C i

    let resolve f = function
                  | C i -> Ok (C i)
                  | A x -> match f x with
                        | Some y -> Ok (A y)
                        | None -> Error x

end

module Program= struct
    type 'v stmt = Label of string
                     | Instruction of 'v Instr.t

    type 'v t = 'v stmt list

    let map f prog = 
            let map_line = function
                         | Label l -> Label l
                         | Instruction i -> Instruction (Instr.map f i)
            in List.map map_line prog

    let address (prog: 'v t) : (string * int) list =
    let rec address_line lines add ls =
            match lines with
                    | (Label s):: tail -> address_line tail add ((s, add) :: ls)
                    | (Instruction _)::tail -> address_line tail (add+1) ls
                    | [] -> List.rev ls
            in address_line prog 0 []
end


