open Jasmin
open Prog
open Syntax
open Annotations
open Wsize
open Printf
let indent = "    "

type state = {
  mutable counter:int;
  mutable node_buffer:Buffer.t;
  mutable link_buffer:Buffer.t;
  mutable last_parent:int;
}

let next state = let num=state.counter in
  state.counter <- num +1;
  num

let create_node state_number label = 
  Format.asprintf "%sN_%d[label=%S;];@." indent state_number label

let create_link from_number to_number = 
  Format.asprintf "%sN_%d -- N_%d;@." indent from_number to_number



let wsize_to_string ws = 
    match ws with 
    | U8 -> "U8"
    | U16 ->"U16"
    | U32 ->"U32"
    | U64 ->"U64"
    | U128 ->"U128"
    | U256 -> "U256"

let add_node_with_parent state node label = 
  let node_string = create_node node label in 
  let link_string = create_link state.last_parent node in 
  Buffer.add_string state.node_buffer node_string;
  Buffer.add_string state.link_buffer link_string

let set_as_parent state node_id:unit = 
  state.last_parent <- node_id


let rec parse_pexpr state (px: pexpr_r) = 
  match px with
    | PEParens pe ->()
    | PEVar pi ->()
    | PEGet (align, arr_access, ws, pid,exp,cexp) ->()
    | PEFetch memacc ->()
    | PEpack (svs,exprs) ->()
    | PEBool bv -> ()
    | PEInt z -> ()
    | PECall (id,params) ->()
    | PECombF (id,exprs) -> ()
    | PEPrim (id,params) ->() 
    | PEOp1 (unop,expr) -> () 
    | PEOp2 (binop,exp) ->()
    | PEIf (cond,thenexp,elseexp) ->()
    


let parse_t_array state (ws,expr)= 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Wsize: %s" (wsize_to_string ws));
    parse_pexpr state (L.unloc expr)

let parse_ptype state (pt:ptype_r) = 
    let node = (next state) in
    match pt with
    | TBool -> add_node_with_parent state node "Type:Bool"
    | TInt -> add_node_with_parent state node "Type:Int"
    | TWord ws -> add_node_with_parent state node (Format.asprintf "Type:Word=%s" (wsize_to_string ws))
    | TArray (ws,e) -> 
        add_node_with_parent state node "Type:Array";
        parse_t_array state (ws,e)


let parse_ptr state (ptr:ptr) = 
    let node = (next state) in
    match ptr with 
    | `Pointer wopt -> (
        match wopt with 
        | None -> 
            add_node_with_parent state node "Pointer"
        | Some w -> 
            match w with 
            | `Constant -> add_node_with_parent state node "Pointer:Constant"
            | `Writable -> add_node_with_parent state node "Pointer:Writable"
        )
    | `Direct -> 
        add_node_with_parent state node "Direct"


let parse_storage state (storage:pstorage) = 
    let node = (next state) in
    match storage with 
    | `Reg ptr -> 
        add_node_with_parent state node "Storage:Reg";
        let parent = state.last_parent in 
        set_as_parent state node;
            parse_ptr state ptr;
        set_as_parent state parent;
    | `Stack ptr ->         
        add_node_with_parent state node "Storage:Stack";
        let parent = state.last_parent in 
        set_as_parent state node;
            parse_ptr state ptr;
        set_as_parent state parent;
    | `Inline -> 
        add_node_with_parent state node "Storage:Inline"
    | `Global -> 
        add_node_with_parent state node "Storage:Global"
        

let parse_pstotype state (pst:pstotype) = 
    let store,typevar = pst in 
    let node = (next state) in 
    add_node_with_parent state node "PstoType";
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_storage state store;
        parse_ptype state (L.unloc typevar);
    set_as_parent state parent 

    
let parse_vardecl state  (vd:pident) :unit = 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Varname : <b>%s</b>" (L.unloc vd))

let parse_vardecls state  (vardcls:vardecls) = 
    let ty, vds = vardcls in 
    let node = (next state) in 
    add_node_with_parent state node "VarDecl List";
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_pstotype state ty;
        List.iter (parse_vardecl state) vds;
    set_as_parent state parent



let parse_attribute_wsize state (ws:wsize) = 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Attribute <b>Wsize</b> : %s" (wsize_to_string ws))

let parse_attribute_string state str = 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Attribute <b>String</b> : %s" str)

let parse_attribute_id state id = 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Attribute <b>Id</b> : %s" id)

let parse_attribute_int state z = 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Attribute <b>Int</b> : %s" (Z.to_string z))

let rec parse_attribute state (attr:attribute option) = 
    match attr with
    | None -> ()
    | Some att ->  
        let att = L.unloc att in 
        match att with 
        | Aint z -> parse_attribute_int state z
        | Aid pid -> parse_attribute_id state pid
        | Astring str -> parse_attribute_string state str
        | Aws ws -> parse_attribute_wsize state ws
        | Astruct annots -> parse_annotations state annots

and parse_annotation state (annot:annotation) = 
    let id, att = annot in 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Annotation : <b>%s</b>" (L.unloc id));
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_attribute state att;
    set_as_parent state parent
    

and parse_annotations (state:state) (annot:annotations) =
    let node = (next state) in 
    add_node_with_parent state node "Annotation List";
    let parent = state.last_parent in 
    set_as_parent state node;
        List.iter (parse_annotation state) annot;
    set_as_parent state parent

let parse_arg (state:state) ((annot,vardelcs): (annotations * vardecls)) = 
    let node = (next state) in 
    add_node_with_parent state node "Argument";
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_annotations state annot;
        parse_vardecls state vardelcs;
    set_as_parent state parent

let parse_arg_list (state:state) (arglist:( annotations * vardecls) list) = 
    let node = (next state) in 
    add_node_with_parent state node "Argument List";
    let parent = state.last_parent in 
    set_as_parent state node;
        List.iter (parse_arg state) arglist;
    set_as_parent state parent



let parse_func_returntype state (returntype:(annotations * pstotype)) = 
    let annot,pty = returntype in
    let node  = (next state) in 
    add_node_with_parent state node "FuncReturnType";
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_annotations state annot;
        parse_pstotype state pty;
    set_as_parent state parent

    
let parse_func_returntype_list state (returntype:(annotations * pstotype) list option) = 
    match returntype with 
    | Some returntype -> 
        let node = (next state) in 
        add_node_with_parent state node "FuncReturnTypeList";
        let parent = state.last_parent in
        set_as_parent state node; 
            List.iter (parse_func_returntype state) returntype;
        set_as_parent state parent
    | None  -> ()


let parse_decl state vardecls = ()
let parse_while state (block,exp,cblock) = ()
let parse_for state (id,forcond,block) = ()
let parse_ifthenelse state (cond,thenblock,elseblock) = ()

let parse_assign_cast state  = ()
let parse_assign_op state (pl,op,exp:(plvals * peqop * pexpr)) =
    match op with 
    | `Raw -> ()
    | `Add  castop -> () 
    | `Sub  castop -> ()
    | `Mul  castop -> ()
    | `Div  castop -> ()
    | `Mod  castop -> ()
    | `ShR  castop -> ()
    | `ROR  castop -> ()
    | `ROL  castop -> ()
    | `ShL  castop -> ()
    | `BAnd castop -> ()
    | `BXOr castop -> ()
    | `BOr  castop -> ()
    


let parse_assign_conditionnal state (pl,op,exp,cexp:(plvals * peqop *  pexpr * pexpr option)) = 
    match cexp with
    | None -> parse_assign_op state (pl,op,exp)
    | Some e -> 
        let node = (next state) in 
        add_node_with_parent state node "CondAssign";
        let parent = state.last_parent in 
        set_as_parent state node;
            parse_assign_op state (pl,op,exp);
            parse_pexpr state (L.unloc e);
        set_as_parent state parent

let parse_array_init state id = 
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "ArrayInit : %s" (L.unloc id))
let parse_instr_r state (instr:pinstr_r) = 
    match instr with 
    | PIArrayInit pi -> parse_array_init state pi
    | PIAssign (pl,op,exp,cexp) -> parse_assign_conditionnal state (pl,op,exp,cexp)
    | PIIf (cond,thenblock,elseblock) -> parse_ifthenelse state (cond,thenblock,elseblock)
    | PIFor (id,forcond,blk) -> parse_for state (id,forcond,blk)
    | PIWhile (blk,exp,cblk) -> parse_while state (blk,exp,cblk)
    | PIdecl vds -> parse_decl state vds
    

let parse_instruction state (instr:pinstr) = 
    let annot, ins = instr in
    let node = (next state) in 
    add_node_with_parent state node "Instruction";
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_annotations state annot;
        parse_instr_r state (L.unloc ins);
    set_as_parent state parent




let parse_ident state (ident:pident) = 
    let node = (next state) in 
    add_node_with_parent state node (L.unloc ident)


let parse_return state (return:pident list option) = 
    match return with 
    | None -> ()
    | Some l -> 
        let node = (next state) in
        add_node_with_parent state node "Return";
        let parent = state.last_parent in 
        set_as_parent state node;
            List.iter (parse_ident state) l;
        set_as_parent state parent

let rec parse_funcbody state (funbody:pfunbody)= 
    let node = (next state) in 
    add_node_with_parent state node "Body";
    let parent = state.last_parent in 
    set_as_parent state node;
        List.iter (parse_instruction state) funbody.pdb_instr;
        parse_return state funbody.pdb_ret;
    set_as_parent state parent
and
parse_function state fundef = 
    let node = (next state) in 
    let funame = L.unloc fundef.pdf_name in
    let namestring = match fundef.pdf_cc with
        | None  -> (Format.asprintf "FuncDef : <b>%s</b>" funame)
        | Some v -> match v with 
            | `Inline -> (Format.asprintf "FuncDef : Inline <b>%s</b>" funame)
            | `Export -> (Format.asprintf "FuncDef : Inline <b>%s</b>" funame)
    in
    add_node_with_parent state node namestring;
    let parent = state.last_parent in 
    set_as_parent state node;

        parse_annotations state fundef.pdf_annot;
        parse_arg_list state fundef.pdf_args;
        parse_func_returntype_list state fundef.pdf_rty;
        parse_funcbody state fundef.pdf_body;

    set_as_parent state parent


let parse_global_param state (pparam:pparam) =
    let node = (next state) in 
    add_node_with_parent state node "Global Param";
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_ptype state (L.unloc pparam.ppa_ty);
        parse_ident state pparam.ppa_name;
        parse_pexpr state (L.unloc pparam.ppa_init);
    set_as_parent state parent

let parse_gpexpr state gexp = 
    match gexp with 
    | GEword expr -> parse_pexpr state (L.unloc expr)
    | GEarray expr_list -> 
        let node = (next state) in 
        add_node_with_parent state node "Expression List";
        let parent = state.last_parent in 
        set_as_parent state node;
            List.iter (parse_pexpr state) (List.map (L.unloc) expr_list);
        set_as_parent state parent
    | GEstring gs -> 
        let node = (next state) in
        add_node_with_parent state node (Format.asprintf "String: %s" (L.unloc gs))

let parse_global state (pg:pglobal) = 
    let node = (next state) in 
    add_node_with_parent state node "Global Variable";
    let parent = state.last_parent in
    set_as_parent state node;
        parse_ptype state (L.unloc pg.pgd_type);
        parse_ident state pg.pgd_name;
        parse_gpexpr state pg.pgd_val;
    set_as_parent state parent


let parse_pex_mem state zl = 
    List.iter (fun (x:Z.t*Z.t) -> let node = (next state) in let x1,x2 = x in add_node_with_parent state node (Format.asprintf "%s,%s" (Z.to_string x1) (Z.to_string x2))) zl

    let parse_exec state (exec:pexec) = 
    let node = (next state) in 
    add_node_with_parent state node "Exec";
    let parent = state.last_parent in 
    set_as_parent state node;
        parse_ident state exec.pex_name;
        parse_pex_mem state exec.pex_mem;
    set_as_parent state parent

let parse_require state (require:pident option * prequire list) = 
    let oid, prl = require in
    let node = (next state) in 
    add_node_with_parent state node "Require";
    let parent = state.last_parent in 
    set_as_parent state node;
        match oid with 
        | None -> ()
        | Some id -> parse_ident state id
        ;
        List.iter (fun x -> let node = (next state) in add_node_with_parent state node (L.unloc x)) prl;
    set_as_parent state parent


let rec parse_namespace state (id,items:(pident * pitem L.located list)) =
    let node = (next state) in 
    add_node_with_parent state node (Format.asprintf "Namespace: %s" (L.unloc id));
    let parent = state.last_parent in
    set_as_parent state node;
        List.iter (parse_item state) items;
    set_as_parent state parent;

and parse_item state item =
    match L.unloc item with 
    | PFundef pfundef -> parse_function state pfundef
    | PParam pparam -> parse_global_param state pparam
    | PGlobal pglobal -> parse_global state pglobal
    | Pexec pexec -> parse_exec state pexec
    | Prequire req -> parse_require state req
    | PNamespace (pident,pitemlist)  -> parse_namespace state (pident,pitemlist)
    
let pp_prog fmt ast = 
    let state = {counter=0;node_buffer=Buffer.create 1000;link_buffer=Buffer.create 1000; last_parent=0} in
    let node = (next state) in 
    set_as_parent state node;
    List.iter (parse_item state) ast;
    Format.fprintf fmt "digraph{@.%s@.%s }@." (Buffer.contents state.node_buffer) (Buffer.contents state.link_buffer)
