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

    type unary = Bneg 
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
             | Unary of Reg.r*unary
             | Binary of Reg.r2*binary

    type dest = M
    	      | D
	          | MD
	          | A
     	      | AM
    	      | AD      
              | AMD
              

    type cinst = { destination : dest option;
                   output      : out;
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
    type 'v prog = 'v Instr.t list
    let map f ls = List.map (Instr.map f) ls 
end
     
