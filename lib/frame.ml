open Types

type 'a frame_env = {
  table: (string, 'a) Hashtbl.t;
  prev: 'a frame_env option;
  types: Types.typ list ref;
  n_fields: int ref;
  id: int;
  depth: int ref;
}

let counter_frames = ref 0

let gen_number_frame () =
  let fresh_number = !counter_frames in
  counter_frames := fresh_number + 1;
  fresh_number

let type_to_string t=
  match t with
  | IntType -> "I"
  | FloatType -> "F"
  | BoolType -> "Z"
  | UnitType -> "V"
  | RefType _ -> "L"^ Ref.string_of_type t ^ ";"
  | NoneType -> "none"
  | StringType -> "Ljava/lang/String;"
  | FunType (args,ret) -> "Lclosure_interface_" ^ String.concat "_" (List.map Ref.string_of_type args) ^ "_" ^ Ref.string_of_type ret ^ ";"
  (* | _ -> failwith "not supported" *)

  let set_depth frame depth =
    frame.depth := depth

let create_frame_from_binds bindings old_frame =
  let rec get_types_list bindings =
    match bindings with
    | [] -> []
    | (_,_,t)::rest ->
      t :: get_types_list rest
  in
  let types = ref (get_types_list bindings) in
  let n_fields = ref (List.length bindings) in
  let id = gen_number_frame () in
  let table = Hashtbl.create 10 in
  let depth = ref 0 in
  {table; prev = old_frame; types; n_fields; id; depth}

let create_frame old_frame =
  let types = ref([]) in
  let n_fields = ref 0 in
  let id = gen_number_frame () in
  let table = Hashtbl.create 10 in
  let depth = ref 0 in
  {table; prev = old_frame; types; n_fields; id; depth}

let create_frame_file frame =
  let preamble = [
    ".class public frame_" ^ string_of_int frame.id;
    ".super java/lang/Object";
    if frame.prev = None
    then ".field public SL Ljava/lang/Object;"
    else
      ".field public SL Lframe_" ^ string_of_int (Option.get(frame.prev)).id ^ ";"
  ] in
  let constructor = [
    ".method public <init>()V";
    "aload 0";
    "invokenonvirtual java/lang/Object/<init>()V";
    "return";
    ".end method"
  ] in
  let types = ref(List.map type_to_string !(frame.types)) in
  let fields = List.mapi (fun i t -> ".field public loc_" ^ string_of_int i ^ " " ^ t) !types in
  let f = preamble @ fields @ constructor in
  let oc = open_out ("frame_"^ string_of_int frame.id ^".j") in
  List.iter (fun x -> Printf.fprintf oc "%s\n" x) f; close_out oc


let bind (env : 'a frame_env option) (id : string) (value : 'a) (t : Types.typ) =
  match env with
  | None -> failwith "cannot bind value to null hashtable"
  | Some v -> Hashtbl.add v.table id value; 
              v.n_fields := !(v.n_fields) + 1;
              v.types := !(v.types) @ [t]
              

let rec find (env : 'a frame_env option) id =
  match env with
  | None -> raise Not_found(*failwith ("There is no envirionment or " ^ id ^ " doesn't exist")*) (* talves mudar para dar return de none? *)
  | Some ev -> match Hashtbl.find_opt ev.table id with
    | Some v ->  v
    | None -> find ev.prev id

let rec findJumpLocation (env : 'a frame_env option) id jmps=
  match env with
  | None -> raise Not_found(*failwith ("There is no envirionment or " ^ id ^ " doesn't exist")*) (* talves mudar para dar return de none? *)
  | Some ev -> match Hashtbl.find_opt ev.table id with
    | Some v ->  (jmps, v)
    | None -> findJumpLocation ev.prev id (jmps+1)

let rec findFrame (env : 'a frame_env option) id=
  match env with
  | None -> raise Not_found(*failwith ("There is no envirionment or " ^ id ^ " doesn't exist")*) (* talves mudar para dar return de none? *)
  | Some ev -> match Hashtbl.find_opt ev.table id with
    | Some _ ->  ev.id
    | None -> findFrame ev.prev id

let begin_scope (prev : 'a frame_env option) : ('a frame_env option) =  Some(create_frame prev)
  
let end_scope (env : 'a frame_env option) =
  match env with
  | None -> failwith "environment does not have previous environment"  
  | Some v -> match v.prev with
    | None -> None
    | Some p -> Some p
