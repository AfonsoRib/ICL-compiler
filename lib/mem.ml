type 'a memory_cell = {
  mutable value: 'a;
  mutable is_free: bool;
}

type memory = {
  mutable cell_array : int memory_cell array;
  mutable free_list : int list;
  mutable size: int;
}

let create_memory size = {
  cell_array = Array.init size (fun _ -> { value = 0; is_free = true });
  free_list = List.init size (fun i -> i);
  size = size;
}

let allocate_memory memory =
  match memory.free_list with
  | [] -> None
  | i :: rest ->
    memory.free_list <- rest;
    memory.cell_array.(i).is_free <- false;
    Some i

let free_memory memory i =
  memory.cell_array.(i).is_free <- true;
  memory.free_list <- i :: memory.free_list

let read_memory memory i =
  memory.cell_array.(i).value

(*Returns the memory location (index) *)
let write_memory memory value =
  let cell = allocate_memory memory in
  match cell with
  | None -> failwith "Out of memory"
  | Some i ->
    memory.cell_array.(i).value <- value;
    Some i
