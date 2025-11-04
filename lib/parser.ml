open Ast

let trim_string s = String.trim s

let remove_comment line=
    match String.split_on_char '/' line with 
            |  [] -> ""
            | first :: _ -> first

let clean_line line = line |> remove_comment |> trim_string 

let parse_jump (jump_str : string) : Instr.jinst option =
    match jump_str with 
            | "JGT" -> Some Instr.JGT
            | "JEQ" -> Some Instr.JEQ
            | "JGE" -> Some Instr.JGE
            | "JLT" -> Some Instr.JLT
            | "JNE" -> Some Instr.JNE
            | "JLE" -> Some Instr.JLE
            | "JMP" -> Some Instr.JMP
            | "" -> None
            | _ -> failwith ("Invalid Jmp instruction " ^ jump_str)

let parse_dest (dest_str : string) : Instr.dest option =
    match dest_str with 
            | "M" -> Some Instr.M
            | "D" -> Some Instr.D
            | "MD" -> Some Instr.MD
            | "A" -> Some Instr.A
            | "AM" -> Some Instr.AM
            | "AD" -> Some Instr.AD
            | "AMD" -> Some Instr.AMD
            | "" -> None
            | _ -> failwith ("Invalid Dest instruction" ^ dest_str)

let parse_const (const_str : string) : Instr.const option= 
    match const_str with 
            | "0" -> Some Instr.Zero
            | "1" -> Some Instr.One
            | "-1" -> Some Instr.MinusOne
            | _ -> None

let reg_token s = 
    match s with 
        | "D" -> Reg.D
        | "A" -> Reg.A
        | "M" -> Reg.M
        | _ -> failwith ("Invalid register " ^ s)

let parse_unary (unary_str : string) : Instr.out option =
    let len = String.length unary_str in 
    match unary_str with 
            | _ when len >=2 && String.sub unary_str (len-2) 2 = "+1" ->
                    let r_str = String.sub unary_str 0 (len-2) in 
                    Some (Instr.Unary (Instr.Succ,reg_token r_str))
            | _ when len >=2 && String.sub unary_str (len-2) 2 = "-1" ->
                    let r_str = String.sub unary_str 0 (len-2) in 
                    Some (Instr.Unary (Instr.Pred, reg_token r_str))
            | _ when len >=2 && unary_str.[0] = '!' -> 
                    let r_str = String.sub unary_str 1 (len-1) in 
                    Some (Instr.Unary(Instr.BNeg, reg_token r_str))
            | _ when len >=2 && unary_str.[0] = '-' -> 
                    let r_str = String.sub unary_str 1 (len-1) in 
                   Some (Instr.Unary (Instr.UMinus, reg_token r_str))
            | "D" -> Some (Instr.Unary (Instr.ID, Reg.D))
            | "A" ->  Some (Instr.Unary (Instr.ID, Reg.A))
            | "M" ->  Some (Instr.Unary (Instr.ID, Reg.M))
            | _ -> None

let parse_binary (binary_str : string) : Instr.out = 
    match binary_str with 
            | "D+A" | "A+D" -> Instr.Binary(Instr.Add, Reg.A)
            | "D+M" | "M+D" -> Instr.Binary(Instr.Add, Reg.M)
            | "D-A" -> Instr.Binary(Instr.Sub, Reg.A)
            | "D-M" -> Instr.Binary(Instr.Sub, Reg.M)
            | "A-D" -> Instr.Binary(Instr.SubFrom, Reg.A)
            | "M-D" -> Instr.Binary(Instr.SubFrom, Reg.M)
            | "A&D" | "D&A" -> Instr.Binary(Instr.BAnd, Reg.A)
            | "M&D" | "D&M" -> Instr.Binary(Instr.BAnd, Reg.M)
            | "A|D" | "D|A" -> Instr.Binary(Instr.BOr, Reg.A)
            | "M|D" | "D|M" -> Instr.Binary(Instr.BOr, Reg.M)
            | _ -> failwith("Invalid binary instruction: " ^ binary_str)

let parse_out (out_str : string) : Instr.out =
    match out_str with
    | _ -> 
            match parse_const out_str with
            | Some c -> Instr.Const c
            | None -> 
                    match parse_unary out_str with
                    | Some d -> d
                    | None -> parse_binary out_str



                        
                           





