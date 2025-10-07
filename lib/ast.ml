module Reg = struct
    type r = A | M | D
    type r2 = A | M
end

module  Instr = struct
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

    type 'v inst = A of 'v
                 | C of cinst
    
    let map f = function
              | A x -> A (f x)
              | C i -> C i

end

module Program= struct
    type 'v prog = 'v Inst.t list

end
     
