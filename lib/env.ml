type 'a environment = {
    table : (string, 'a) Hashtbl.t;
    prev : 'a environment option;
}


let create_environment (prev : 'a environment option) = { table = Hashtbl.create 10; prev }

let bind (env : 'a environment option) (id : string) (value : 'a) =
  match env with
  | None -> failwith "cannot bind value to null hashtable"
  | Some v -> Hashtbl.add v.table id value



let rec find (env : 'a environment option) id =
  match env with
  | None -> raise Not_found(*failwith ("There is no envirionment or " ^ id ^ " doesn't exist")*) (* talves mudar para dar return de none? *)
  | Some ev -> match Hashtbl.find_opt ev.table id with
               | Some v ->  v
               | None -> find ev.prev id

let begin_scope (prev : 'a environment option) : ('a environment option) = Some (create_environment prev)

let end_scope (env : 'a environment option) =
  match env with
  | None -> failwith "environment does not have previous environment"
  | Some v -> match v.prev with
              | Some prev_environment -> Some (prev_environment)
              | None -> None
