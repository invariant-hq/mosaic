open Mosaic_ui

type ('props, 'event, 'w) t = {
  id : ('props * 'event * 'w) Type.Id.t;
  name : string;
  create : parent:Renderable.t -> layout:Toffee.Style.t -> 'props -> 'w;
  node : 'w -> Renderable.t;
  layout : 'w -> Toffee.Style.t -> unit;
  update : 'w -> 'props -> unit;
  equal : 'props -> 'props -> bool;
  focusable : bool;
  events : ('w -> ('event -> unit) -> unit) option;
  handler : ('w -> ('event -> unit) option -> unit) option;
}

let make ~name ~create ~node ?layout ~update ~equal ?(focusable = false) ?events
    ?handler () =
  let layout =
    match layout with
    | Some layout -> layout
    | None -> fun w style -> Renderable.set_style (node w) style
  in
  {
    id = Type.Id.make ();
    name;
    create;
    node;
    layout;
    update;
    equal;
    focusable;
    events;
    handler;
  }

let name k = k.name

external physically_equal : 'a -> 'b -> bool = "%eq"

let same a b = physically_equal a.id b.id
let no_handler _ = None
