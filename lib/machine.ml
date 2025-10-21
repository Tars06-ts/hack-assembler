let storage = function
            | M    -> 0b100
            | D    -> 0b010
            | A    -> 0b001 
            | MD   -> 0b110
            | AM   -> 0b101
            | AD   -> 0b011
            | AMD  -> 0b111

let jmp = function
        | JGT -> 0b100
        | JEQ -> 0b010
        | JGE -> 0b110
        | JLT -> 0b001
        | JNE -> 0b101
        | JLE -> 0b011
        | JMP -> 0b111 

