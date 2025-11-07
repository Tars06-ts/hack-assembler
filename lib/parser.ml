open Ast
open Machine

let trim_string s = String.trim s

let remove_comment line=
    match String.split_on_char '/' line with 
            |  [] -> ""
            | first :: _ -> first

let clean_line line = line |> remove_comment |> trim_string 

(*---HANDLING THE C INSTRUCTIONS---*)

let parse_jump (jump_str : string) : Instr.jinst option =
    match jump_str with 
            | "JGT" -> Some Instr.JGT
            | "JEQ" -> Some Instr.JEQ
            | "JGE" -> Some Instr.JGE
            | "JLT" -> Some Instr.JLT
            | "JNE" -> Some Instr.JNE
            | "JLE" -> Some Instr.JLE
            | "JMP" -> Some Instr.JMP
            | ""    -> None
            | _     -> failwith ("Invalid Jmp instruction " ^ jump_str)

let parse_dest (dest_str : string) : Instr.dest option =
    match dest_str with 
            | "M"   -> Some Instr.M
            | "D"   -> Some Instr.D
            | "MD"  -> Some Instr.MD
            | "A"   -> Some Instr.A
            | "AM"  -> Some Instr.AM
            | "AD"  -> Some Instr.AD
            | "AMD" -> Some Instr.AMD
            | ""    -> None
            | _     -> failwith ("Invalid Dest instruction" ^ dest_str)

let parse_const (const_str : string) : Instr.const option= 
    match const_str with 
            | "0"  -> Some Instr.Zero
            | "1"  -> Some Instr.One
            | "-1" -> Some Instr.MinusOne
            | _    -> None

(*let reg_token s = 
    match s with 
        | "D" -> Reg.D
        | "A" -> Reg.A
        | "M" -> Reg.M
        | _   -> failwith ("Invalid register " ^ s)*)

let parse_unary (unary_str : string) : Instr.out option =
    match unary_str with 
           (* | _ when len >=2 && String.sub unary_str (len-2) 2 = "+1" ->
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
                   Some (Instr.Unary (Instr.UMinus, reg_token r_str))*)
            | "D" -> Some (Instr.Unary (Instr.ID, Reg.D))
            | "A" ->  Some (Instr.Unary (Instr.ID, Reg.A))
            | "M" ->  Some (Instr.Unary (Instr.ID, Reg.M))

            | "!D"-> Some (Instr.Unary (Instr.BNeg, Reg.D))
            | "!A" -> Some (Instr.Unary (Instr.BNeg, Reg.A))
            | "!M" -> Some (Instr.Unary (Instr.BNeg, Reg.M))
            
            | "-D" -> Some (Instr.Unary (Instr.UMinus, Reg.D))
            | "-A" -> Some (Instr.Unary (Instr.UMinus, Reg.A))
            | "-M" -> Some (Instr.Unary (Instr.UMinus, Reg.M))

            | "D+1" -> Some (Instr.Unary (Instr.Succ, Reg.D))
            | "A+1" -> Some (Instr.Unary (Instr.Succ, Reg.A))
            | "M+1" -> Some (Instr.Unary (Instr.Succ, Reg.M))

            | "D-1" -> Some (Instr.Unary (Instr.Pred, Reg.D))
            | "A-1" -> Some (Instr.Unary (Instr.Pred, Reg.A))
            | "M-1" -> Some (Instr.Unary (Instr.Pred, Reg.M))

            | _   -> None

let parse_binary (binary_str : string) : Instr.out = 
    match binary_str with 
            | "D+A" | "A+D"  -> Instr.Binary(Instr.Add, Reg.A)
            | "D+M" | "M+D"  -> Instr.Binary(Instr.Add, Reg.M)
            | "D-A"          -> Instr.Binary(Instr.Sub, Reg.A)
            | "D-M"          -> Instr.Binary(Instr.Sub, Reg.M)
            | "A-D"          -> Instr.Binary(Instr.SubFrom, Reg.A)
            | "M-D"          -> Instr.Binary(Instr.SubFrom, Reg.M)
            | "A&D" | "D&A"  -> Instr.Binary(Instr.BAnd, Reg.A)
            | "M&D" | "D&M"  -> Instr.Binary(Instr.BAnd, Reg.M)
            | "A|D" | "D|A"  -> Instr.Binary(Instr.BOr, Reg.A)
            | "M|D" | "D|M"  -> Instr.Binary(Instr.BOr, Reg.M)
            | _              -> failwith("Invalid binary instruction: " ^ binary_str)

let parse_out (out_str : string) : Instr.out =
    match out_str with 
    | _ -> 
            match parse_const out_str with
            | Some c -> Instr.Const c
            | None   -> 
                       match parse_unary out_str with
                       | Some d -> d
                       | None   -> parse_binary out_str

let parse_c_instr (line : string) : Instr.cinst = 
    let jump_str, rest = 
        match String.split_on_char ';' line with
        | [rest] -> ("", rest)
        | [rest; jump_str] -> (jump_str, rest)
        | _ -> ("", "")
    in 
    let dest_str, out_str = 
        match String.split_on_char '=' rest with 
        | [out_str] -> ("", out_str)
        | [dest_str; out_str] -> (dest_str, out_str)
        | _ -> ("", "")
    in
    let dest = parse_dest dest_str in
    let out = parse_out out_str in
    let jump = parse_jump jump_str 
    in { dest ; out ; jump} 


(*---MANHANDLING THE A INSTRUCTIONS---*)

let parse_a_instr (a_instr : string) : string Instr.t= 
    A (String.sub a_instr 1 (String.length a_instr -1))
    
let parse_label (label_line : string) : string = 
    String.sub label_line 1 (String.length label_line -2)

(*let parse_line (line : string) : string Program.stmt = 
   let cleaned_line =  clean_line line in 
    match cleaned_line.[0] with 
    | '@' -> Instruction (parse_a_instr cleaned_line) 
    | '('  -> Label (parse_label cleaned_line)
    | _   -> Instruction (Instr.C (parse_c_instr cleaned_line))         
*)
let parse_line (line : string) : string Program.stmt option =
   let cleaned_line =  clean_line line in
   if String.length cleaned_line = 0 then None
    (* or handle empty lines appropriately *)
   else
     match cleaned_line.[0] with
     | '@' -> Some (Instruction (parse_a_instr cleaned_line))
     | '('  -> Some (Label (parse_label cleaned_line))
     | _   -> Some (Instruction (Instr.C (parse_c_instr cleaned_line)))


let parse_lines (lines: string list) : string Program.t = 
   List.filter_map (fun x-> x) (List.map parse_line lines)

(*SYMBOL TABLE*)
module Symbol_table = Map.Make(String)
    let init_sym_tab () : (int Symbol_table.t)=
        let pre_def_sym = 
            [("R0", 0); ("R1", 1);("R2", 2);("R3", 3);("R4", 4);("R5", 5);
            ("R6", 6);("R7", 7);("R8", 8);("R9", 9);("R10", 10);("R11", 11);
            ("R12", 12);("R13", 13);("R14", 14);("R15", 15);("SP", 0); ("LCL", 1);
            ("ARG", 2); ("THIS", 3); ("THAT", 4); ("SCREEN", 16384); ("KBD", 24576)
            ]
        in 
        List.fold_left (fun acc (key, value) -> Symbol_table.add key value acc) Symbol_table.empty pre_def_sym

(*-----PASS-1-----*)

let label_add (prog : string Program.t) (table : int Symbol_table.t) : int Symbol_table.t = 
    let labels = Program.address prog in
        List.fold_left (fun acc (key, value) -> Symbol_table.add key value acc) table labels
        
(*-----PASS-2-----*)

let instr_add (table : int Symbol_table.t) =
    let table_ptr = ref table in
    let next_add = ref 16 in 
    
    let resolve_sym str= 
        try 
        (int_of_string str)
        with Failure _ -> 
            match Symbol_table.find_opt str !table_ptr with
                | Some add -> add
                | None -> 
                        let new_add = !next_add in
                        table_ptr := Symbol_table.add str new_add !table_ptr;
                        next_add := !next_add + 1;
                        new_add
    in resolve_sym 

let gen_code (prog: int Program.t) : string list = 
    let rec loop (lines_left : int Program.t) (ls : string list) : string list = 
        match lines_left with 
        | [] -> List.rev ls
        | instr :: tail -> 
                match instr with 
                | Label _ -> loop tail ls
                | Instruction i -> loop tail ((Inst.encode i) :: ls)
    in loop prog []

let final_proj (prog : string Program.t) : string =
    let table = init_sym_tab () in 
    let table_with_labels= label_add prog table in
    let table_complete = instr_add table_with_labels in
    let resolved_program = Program.map table_complete prog 
in String.concat "\n" (gen_code resolved_program)

