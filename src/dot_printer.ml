open Jasmin
open Prog
open Syntax



let indent = "    "

type state = {
  mutable counter:int;
  mutable node_buffer:Buffer.t;
  mutable link_buffer:Buffer.t
}

let next state = let num=state.counter in
  state.counter <- num +1;
  num

let create_node state_number label = 
  Format.asprintf "%sN_%d[label=%S;];@." indent state_number label

let create_link from_number to_number = 
  Format.asprintf "%sN_%d -- N_%d;@." indent from_number to_number

let parse_function state pfundef = ()
  

let rec parse_item state item =
    match L.unloc item with 
    | PFundef pfundef -> parse_function state pfundef
    | PParam pparam -> ()
    | PGlobal pglobal -> ()
    | Pexec pexec -> ()
    | Prequire prequire -> ()
    | PNamespace (pident,pitemlist)  -> ()
    


let pp_prog fmt ast = 
  let state = {counter=0;node_buffer=Buffer.create 1000;link_buffer=Buffer.create 1000} in
  List.iter (parse_item state) ast;
  Format.fprintf fmt "digraph{@.%s@.%s }@." (Buffer.contents state.node_buffer) (Buffer.contents state.link_buffer)
