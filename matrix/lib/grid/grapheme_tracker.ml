type entry = { sample : Packed_cell.t; mutable count : int }

type t = {
  (* Map from Grapheme payload (ID + generation) -> local reference count *)
  counts : (int, entry) Hashtbl.t;
  store : Grapheme_store.t;
  mutable unique : int;
}

let[@inline] payload_key id = Packed_cell.store_key id
let create store = { counts = Hashtbl.create 128; store; unique = 0 }

let add t id =
  match payload_key id with
  | None -> ()
  | Some key -> (
      match Hashtbl.find_opt t.counts key with
      | Some entry -> entry.count <- entry.count + 1
      | None ->
          (* First sighting of this grapheme in the grid: grab a store ref
             once *)
          Packed_cell.incref t.store id;
          Hashtbl.add t.counts key { sample = id; count = 1 };
          t.unique <- t.unique + 1)

let remove t id =
  match payload_key id with
  | None -> ()
  | Some key -> (
      match Hashtbl.find_opt t.counts key with
      | Some entry when entry.count = 1 ->
          Packed_cell.decref t.store entry.sample;
          Hashtbl.remove t.counts key;
          t.unique <- t.unique - 1
      | Some entry -> entry.count <- entry.count - 1
      | None -> ())

let replace t ~old_id ~new_id =
  if old_id <> new_id then (
    add t new_id;
    remove t old_id)

let clear t =
  Hashtbl.iter (fun _ entry -> Packed_cell.decref t.store entry.sample) t.counts;
  Hashtbl.clear t.counts;
  t.unique <- 0

let unique_count t = t.unique
