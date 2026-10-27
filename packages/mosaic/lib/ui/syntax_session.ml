(* Highlighters *)

module Highlighter = struct
  type edit = { start : int; removed : int; inserted : int }
  type 'state previous = { content : string; state : 'state; edit : edit }

  type 'state request = {
    content : string;
    language : string;
    range : int * int;
    previous : 'state previous option;
  }

  type 'state result = (Syntax_highlight.t * 'state, exn) Stdlib.result

  type 'state job = {
    poll : unit -> 'state result option;
    cancel : unit -> unit;
  }

  (* The state's type is the highlighter's own: a view never looks at it.
     [off_loop] runs work off the rendering thread, on the renderer's
     executor. A highlighter that keeps nothing between results computes all
     of them at each request, so it is asked for the whole content
     ([whole]). *)
  type t =
    | Highlighter : {
        start :
          'state request ->
          notify:(unit -> unit) ->
          off_loop:((unit -> unit) -> unit) ->
          'state job;
        whole : bool;
      }
        -> t

  let job ~poll ~cancel = { poll; cancel }

  let completed outcome =
    let pending = ref (Some outcome) in
    job
      ~poll:(fun () ->
        let result = !pending in
        pending := None;
        result)
      ~cancel:(fun () -> pending := None)

  let synchronous ~whole f =
    Highlighter
      {
        start =
          (fun request ~notify:_ ~off_loop:_ ->
            completed (try Ok (f request) with exn -> Error exn));
        whole;
      }

  let sync_incremental f = synchronous ~whole:false f

  (* A highlighter that keeps nothing has [()] as its state. *)
  let stateless f { language; content; range = _; previous = _ } =
    (f ~language ~content, ())

  let sync f = synchronous ~whole:true (stateless f)

  let async f =
    Highlighter
      {
        start =
          (fun request ~notify ~off_loop:_ ->
            try f request ~notify with exn -> completed (Error exn));
        whole = false;
      }

  type 'state work = {
    request : 'state request;
    notify : unit -> unit;
    mutable cancelled : bool;
    mutable outcome : 'state result option;
  }

  let worker ~whole f =
    let mutex = Mutex.create () in
    let pending = Queue.create () in
    let running = ref false in
    let rec work () =
      match
        Mutex.protect mutex (fun () ->
            match Queue.take_opt pending with
            | None ->
                running := false;
                None
            | Some task -> Some task)
      with
      | None -> ()
      | Some task ->
          let outcome = try Ok (f task.request) with exn -> Error exn in
          Mutex.protect mutex (fun () ->
              if not task.cancelled then begin
                task.outcome <- Some outcome;
                (* Notification only schedules a render; it never polls the job. *)
                try task.notify () with _ -> ()
              end);
          work ()
    in
    (* The worker runs on the renderer's executor, started outside the lock,
       which an executor that runs it at once takes. *)
    Highlighter
      {
        whole;
        start =
          (fun request ~notify ~off_loop ->
            let task = { request; notify; cancelled = false; outcome = None } in
            let start =
              Mutex.protect mutex (fun () ->
                  Queue.add task pending;
                  let start = not !running in
                  running := true;
                  start)
            in
            match if start then off_loop work with
            | exception exn ->
                Mutex.protect mutex (fun () ->
                    running := false;
                    Queue.clear pending);
                completed (Error exn)
            | () ->
                job
                  ~poll:(fun () ->
                    Mutex.protect mutex (fun () ->
                        let result = task.outcome in
                        task.outcome <- None;
                        result))
                  ~cancel:(fun () ->
                    Mutex.protect mutex (fun () ->
                        task.cancelled <- true;
                        task.outcome <- None;
                        (* Remove cancelled input immediately, preserving the live FIFO. *)
                        for _ = 1 to Queue.length pending do
                          let queued = Queue.take pending in
                          if queued != task then Queue.add queued pending
                        done)));
      }

  let threaded_incremental f = worker ~whole:false f
  let threaded f = worker ~whole:true (stateless f)
end

(* The bytes [a] and [b] share from [i], up to [n], and from their ends, past
   [k], up to [n], compared eight at a time. *)
let rec prefix a b i n =
  if i + 8 <= n && String.get_int64_ne a i = String.get_int64_ne b i then
    prefix a b (i + 8) n
  else if i < n && String.unsafe_get a i = String.unsafe_get b i then
    prefix a b (i + 1) n
  else i

let rec suffix a b k n =
  let la = String.length a and lb = String.length b in
  if
    k + 8 <= n
    && String.get_int64_ne a (la - k - 8) = String.get_int64_ne b (lb - k - 8)
  then suffix a b (k + 8) n
  else if
    k < n && String.unsafe_get a (la - 1 - k) = String.unsafe_get b (lb - 1 - k)
  then suffix a b (k + 1) n
  else k

(* [edit ~source content] is the one span in which [content] differs from
   [source]: the longest common prefix, then the longest common suffix that
   does not overlap it. *)
let edit ~source content : Highlighter.edit =
  let source_length = String.length source in
  let length = String.length content in
  let prefix = prefix source content 0 (min source_length length) in
  let suffix = suffix source content 0 (min source_length length - prefix) in
  {
    start = prefix;
    removed = source_length - prefix - suffix;
    inserted = length - prefix - suffix;
  }

(* A result accepted by a view, with the request that builds on it. The
   request holds the highlighter's state, so the view need not know its type. *)
type latest = {
  captures : Syntax_highlight.t;
  next : content:string -> range:int * int -> notify:(unit -> unit) -> job;
}

and job = {
  poll : unit -> (latest, exn) Stdlib.result option;
  cancel : unit -> unit;
}

let rec request : type state.
    (state Highlighter.request ->
    notify:(unit -> unit) ->
    state Highlighter.job) ->
    language:string ->
    state Highlighter.previous option ->
    content:string ->
    range:int * int ->
    notify:(unit -> unit) ->
    job =
 fun start ~language previous ~content ~range ~notify ->
  let job = start { content; language; range; previous } ~notify in
  let accept (captures, state) =
    let next ~content ~range ~notify =
      let source = Syntax_highlight.content captures in
      let previous =
        { Highlighter.content = source; state; edit = edit ~source content }
      in
      request start ~language (Some previous) ~content ~range ~notify
    in
    { captures; next }
  in
  {
    poll = (fun () -> Option.map (Result.map accept) (job.poll ()));
    cancel = job.cancel;
  }

let first_request (Highlighter.Highlighter { start; _ }) ~off_loop ~language
    ~content ~range ~notify =
  request
    (fun request ~notify -> start request ~notify ~off_loop)
    ~language None ~content ~range ~notify

(* Syntax settings *)

type source = Ranges of Syntax_highlight.t | Highlighter of Highlighter.t

type syntax = {
  language : string option;
  style : Syntax_style.t option;
  source : source;
}

let syntax ?language ?style captures =
  { language; style; source = Ranges captures }

let with_highlighter ~language ?style highlighter =
  { language = Some language; style; source = Highlighter highlighter }

let language syntax = syntax.language
let style syntax = syntax.style

let source_equal a b =
  match (a, b) with
  | Ranges a, Ranges b ->
      a == b
      || String.equal (Syntax_highlight.content a) (Syntax_highlight.content b)
         && Syntax_highlight.ranges a = Syntax_highlight.ranges b
  | Highlighter a, Highlighter b -> a == b
  | _ -> false

(* Two settings with the same producer yield the same captures for the same
   content; they may differ only in presentation. *)
let same_producer a b =
  Option.equal String.equal a.language b.language
  && source_equal a.source b.source

let equal a b =
  same_producer a b && Option.equal Syntax_style.equal a.style b.style

(* Sessions *)

type running = {
  generation : int;
  content : string;
  syntax : syntax;
  range : int * int;
  job : job;
}

type t = {
  notify : unit -> unit;
  off_loop : (unit -> unit) -> unit;
  mutable content : string;
  mutable syntax : syntax option;
  mutable generation : int;
      (* Changes of content or producer, so a result knows if it is current. *)
  mutable running : running option;
  mutable waiting : (string * syntax * Highlighter.t * (int * int)) option;
  mutable latest : latest option;
      (* The latest result of the current producer, with its state. *)
  mutable captures : Syntax_highlight.t option;
      (* The captures the view shows, bound to the content of the latest
         result: that result's over its range, and the earlier results'
         elsewhere, carried across the edits since. *)
  mutable exact : int * int;
      (* The bytes of [captures]'s content that results for that content
         cover. *)
  mutable failed : bool; (* The latest request for the content shown failed. *)
}

let create ~notify ~off_loop =
  {
    notify;
    off_loop;
    content = "";
    syntax = None;
    generation = 0;
    running = None;
    waiting = None;
    latest = None;
    captures = None;
    exact = (0, 0);
    failed = false;
  }

let is_highlighting t =
  Option.is_some t.waiting
  ||
  match t.running with
  | Some running -> running.generation = t.generation
  | None -> false

let bound captures content =
  String.equal (Syntax_highlight.content captures) content

let within (first, last) (first', last') = first' <= first && last <= last'

(* The smallest range that holds both, when they meet or touch. *)
let join (first, last) (first', last') =
  if first <= last' && first' <= last then
    Some (min first first', max last last')
  else None

(* [carry ~conceal captures content] moves [captures] onto [content], which
   differs from their source in one span. A range keeps its scope over its
   bytes outside that span, which is plain, so captures never style bytes they
   were not computed for. *)
let carry ~conceal captures content =
  let source = Syntax_highlight.content captures in
  if String.equal source content then captures
  else
    let { Highlighter.start; removed; inserted } = edit ~source content in
    let removed_end = start + removed and inserted_end = start + inserted in
    let shift = inserted - removed in
    (* Ranges accumulate in reverse declaration order. *)
    let part range ~start_byte ~end_byte moved =
      if start_byte < end_byte then
        Syntax_highlight.range
          ~meta:(Syntax_highlight.meta range)
          ~start_byte ~end_byte
          ~scope:(Syntax_highlight.scope range)
          ()
        :: moved
      else moved
    in
    let move moved range =
      let start_byte = Syntax_highlight.start_byte range in
      let end_byte = Syntax_highlight.end_byte range in
      if end_byte <= start then range :: moved
      else if start_byte >= removed_end then
        part range ~start_byte:(start_byte + shift) ~end_byte:(end_byte + shift)
          moved
      else if conceal && Syntax_highlight.conceals range then moved
      else
        let moved =
          part range ~start_byte ~end_byte:(min end_byte start) moved
        in
        part range ~start_byte:inserted_end ~end_byte:(end_byte + shift) moved
    in
    Syntax_highlight.make ~content
      (List.rev (List.fold_left move [] (Syntax_highlight.ranges captures)))

(* [merge t captures range] takes a result's [captures] over [range] into
   what [t] shows. A result holds every capture that meets its range, whole,
   so it replaces the earlier captures that meet the range, and those that a
   capture reaching out of the range overlaps; the earlier captures elsewhere
   stay, carried onto the result's content. *)
let merge t captures ((first, last) as range) =
  let content = Syntax_highlight.content captures in
  match t.captures with
  | Some earlier when first > 0 || last < String.length content ->
      let same = bound earlier content in
      let earlier = carry ~conceal:true earlier content in
      let fresh = Syntax_highlight.ranges captures in
      let meets r first last =
        Syntax_highlight.start_byte r < last
        && Syntax_highlight.end_byte r > first
      in
      let reaching =
        List.filter
          (fun r ->
            Syntax_highlight.start_byte r < first
            || Syntax_highlight.end_byte r > last)
          fresh
      in
      let kept =
        List.filter
          (fun r ->
            (not (meets r first last))
            && not
                 (List.exists
                    (fun c ->
                      meets r
                        (Syntax_highlight.start_byte c)
                        (Syntax_highlight.end_byte c))
                    reaching))
          (Syntax_highlight.ranges earlier)
      in
      t.captures <- Some (Syntax_highlight.make ~content (kept @ fresh));
      t.exact <-
        (if same then Option.value ~default:range (join t.exact range)
         else range)
  | Some _ | None ->
      t.captures <- Some captures;
      t.exact <- range

(* A highlighter that keeps nothing, and every highlighter on a view's first
   request, is asked for the whole content. *)
let launch t content syntax (Highlighter.Highlighter { whole; _ } as h) range =
  let language = Option.value syntax.language ~default:"" in
  let length = String.length content in
  let range =
    if whole || Option.is_none t.latest then (0, length)
    else
      let first = max 0 (min (fst range) length) in
      (first, max first (min (snd range) length))
  in
  let job =
    match t.latest with
    | Some latest -> latest.next ~content ~range ~notify:t.notify
    | None ->
        first_request h ~off_loop:t.off_loop ~language ~content ~range
          ~notify:t.notify
  in
  t.running <- Some { generation = t.generation; content; syntax; range; job }

(* [finish t running outcome] is [true] iff it changes what [t] shows. A
   result for content that has since changed still describes the bytes the
   current content shares with it, so it becomes the latest. *)
let finish t (running : running) outcome =
  let accepted =
    match outcome with
    | Ok (latest : latest) when bound latest.captures running.content ->
        Some latest
    | Ok _ | Error _ -> None
  in
  let current = running.generation = t.generation in
  let producer =
    match t.syntax with
    | Some syntax -> same_producer running.syntax syntax
    | None -> false
  in
  if current then t.failed <- Option.is_none accepted;
  match accepted with
  | Some (latest : latest) when current || (producer && is_highlighting t) ->
      t.latest <- Some latest;
      merge t latest.captures running.range;
      true
  | Some _ | None -> current

let rec poll t =
  match t.running with
  | None -> false
  | Some running -> (
      match try running.job.poll () with exn -> Some (Error exn) with
      | None -> false
      | Some outcome ->
          t.running <- None;
          let changed = finish t running outcome in
          (match t.waiting with
          | None -> ()
          | Some (content, syntax, highlighter, range) ->
              t.waiting <- None;
              launch t content syntax highlighter range);
          poll t || changed)

(* Requests captures of [t.content] over [range], now or once the request in
   flight completes. *)
let ask t syntax highlighter range =
  match t.running with
  | None -> launch t t.content syntax highlighter range
  | Some _ -> t.waiting <- Some (t.content, syntax, highlighter, range)

let set t ~content ~range syntax =
  let same_producer =
    match (t.syntax, syntax) with
    | Some old, Some syntax -> same_producer old syntax
    | _ -> false
  in
  let same_content = String.equal t.content content in
  t.content <- content;
  t.syntax <- syntax;
  if not same_producer then begin
    t.latest <- None;
    t.captures <- None
  end;
  if not (same_producer && same_content) then begin
    t.generation <- t.generation + 1;
    t.waiting <- None;
    t.failed <- false;
    match syntax with
    | Some ({ source = Highlighter highlighter; _ } as syntax)
      when not (String.equal content "") ->
        ask t syntax highlighter range;
        ignore (poll t : bool)
    | Some _ | None -> ()
  end

let needs_range t =
  match t.syntax with
  | Some { source = Highlighter (Highlighter.Highlighter { whole; _ }); _ } ->
      (not whole) && Option.is_some t.latest
  | Some _ | None -> false

let view t ~range =
  match t.syntax with
  | Some
      ({
         source =
           Highlighter (Highlighter.Highlighter { whole = false; _ } as h);
         _;
       } as syntax)
    when Option.is_some t.latest
         && (not (String.equal t.content ""))
         && (not (is_highlighting t))
         && not t.failed ->
      let shown =
        match t.captures with
        | Some captures -> bound captures t.content && within range t.exact
        | None -> false
      in
      if shown then false
      else begin
        ask t syntax h range;
        poll t
      end
  | Some _ | None -> false

let cancel t =
  t.waiting <- None;
  match t.running with
  | None -> ()
  | Some running -> (
      t.running <- None;
      try running.job.cancel () with _ -> ())

let shown t ~conceal =
  match t.syntax with
  | None -> None
  | Some { source = Ranges captures; _ } ->
      if bound captures t.content then Some captures else None
  | Some { source = Highlighter _; _ } -> (
      match t.captures with
      | None -> None
      | Some _ when t.failed && not (is_highlighting t) -> None
      | Some captures ->
          if bound captures t.content then Some captures
          else if is_highlighting t then
            Some (carry ~conceal captures t.content)
          else None)
