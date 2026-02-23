type entry = { sample : Glyph.t; mutable count : int }

type t = {
  (* Map from Grapheme payload (ID + generation) -> local reference count *)
  counts : (int, entry) Hashtbl.t;
  pool : Glyph.Pool.t;
  mutable unique : int;
}

let[@inline] payload_key id = Glyph.pool_key id
let create pool = { counts = Hashtbl.create 128; pool; unique = 0 }

let add t id =
  match payload_key id with
  | None -> ()
  | Some key -> (
      match Hashtbl.find_opt t.counts key with
      | Some entry -> entry.count <- entry.count + 1
      | None ->
          (* First sighting of this grapheme in the grid: grab a pool ref
             once *)
          Glyph.Pool.incref t.pool id;
          Hashtbl.add t.counts key { sample = id; count = 1 };
          t.unique <- t.unique + 1)

let remove t id =
  match payload_key id with
  | None -> ()
  | Some key -> (
      match Hashtbl.find_opt t.counts key with
      | Some entry when entry.count = 1 ->
          Glyph.Pool.decref t.pool entry.sample;
          Hashtbl.remove t.counts key;
          t.unique <- t.unique - 1
      | Some entry -> entry.count <- entry.count - 1
      | None -> ())

let replace t ~old_id ~new_id =
  if old_id <> new_id then (
    add t new_id;
    remove t old_id)

let clear t =
  Hashtbl.iter (fun _ entry -> Glyph.Pool.decref t.pool entry.sample) t.counts;
  Hashtbl.clear t.counts;
  t.unique <- 0

let unique_count t = t.unique
