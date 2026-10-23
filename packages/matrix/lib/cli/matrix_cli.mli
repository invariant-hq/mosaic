(** Output for programs that write to a terminal they do not own.

    A command-line program prints its output once, at the width of the terminal
    or of the destination it writes to, and leaves it in the scrollback. It
    shares the terminal with the shell and the programs around it. This library
    is the layer for such programs, between the grid of cells ({!Matrix_grid})
    that a program draws into and the bytes it writes. Its first helpers lay out
    tables ({!Table}) and trees ({!Tree}). Each is a function of plain values,
    so a program prints the result with {!Matrix_grid.to_ansi} or
    {!Matrix_grid.to_text}, and a full-screen program can draw the same layout
    into its own frame. *)

(** Tables, how their columns share a width and where their rows and cells go.

    A {e column} has three widths, in cells. Its {e max-content} width is the
    narrowest at which none of its cells breaks a line, its {e min-content}
    width the narrowest at which none breaks a word, and its {e floor} the
    narrowest it goes, at which words break. A table's {e room} is the width its
    columns share, which {!Table.widths} divides as HTML user agents lay tables
    out (RFC 1942, "Autolayout Algorithm"; CSS 2.1 §17.5.2.2; CSS Tables 3
    §3.9.3). When the room holds every column at its max-content width, each
    column takes that width and the cells left go to the columns by their
    {e grow} weights. A table whose columns do not grow is then as wide as its
    content, as a CSS table of automatic width is, and a table given a width
    fills it by growing its columns. When the room is narrower, each column
    takes its min-content width and a share of the rest in proportion to how
    much wider its max-content width is, "the linear combination (with weights
    adding to 1) of the two consecutive sizing-guesses whose width sums bound
    the available width" (CSS Tables 3 §3.9.3). A terminal cannot scroll a table
    sideways, so below the min-content widths the columns go down to their
    floors in the same way, and their words break as CSS Text 3's
    [overflow-wrap: break-word] breaks a word that does not fit. Below the sum
    of the floors the table is wider than its room, and the program cuts its
    lines. Shares are whole cells, given by largest remainders with ties to the
    earlier column, so the widths sum to the room whenever the columns can take
    it.

    A table's {e frame} ({!Table.type-frame}) is its lines, the {e edge} around
    it and the lines between its columns, and the {e padding} between a cell and
    a line or another cell. A table without an edge has no padding at its sides.
    A table is a list of {e rows} ({!Table.type-row}) from top to bottom, each a
    {e rule} across the table, a row of one cell per column, or a row whose
    cells span columns. {!Table.place} says where each row and cell goes, and
    {!Table.draw} draws the frame's lines, where every crossing takes the glyph
    of the lines that meet there ({!Matrix_grid.Lines}). The program draws each
    cell's content in the region {!Table.place} gives it. A table whose cells
    hold text is laid out and drawn whole by {!Table.grid}.

    This prints a table as wide as its content, with a light frame, whose rows
    [None] are rules:
    {[
    let open Matrix_cli in
    let rows = [ Some [| "name"; "size" |]; None; Some [| "a.ml"; "12" |] ] in
    let measure = Matrix_text.measure ~width_method:`Unicode ~tab_width:2 in
    let column i =
      let need = function Some r -> measure r.(i) | None -> 0 in
      let w = List.fold_left (fun a r -> max a (need r)) 0 rows in
      { Table.floor = 1; min = w; max = w; grow = 0 }
    in
    let ws = Table.widths [| column 0; column 1 |] ~room:40 in
    let frame = { Table.edge = Some `Light; line = Some `Light; pad = 1 } in
    let shape =
      List.map (function Some _ -> Table.Row 1 | None -> Rule `Light) rows
    in
    let g =
      Matrix_grid.create ~width:(Table.width frame ws)
        ~height:(Table.height frame shape)
        ()
    in
    Table.draw g ~x:0 ~y:0 frame ws shape;
    List.iter2
      (fun (p : Table.placed) -> function
        | None -> ()
        | Some r ->
            Array.iteri
              (fun i (c : Matrix_grid.region) ->
                Matrix_grid.draw_text g ~x:c.x ~y:c.y ~text:r.(i))
              p.cells)
      (Table.place ~x:0 ~y:0 frame ws shape)
      rows;
    print_string (Matrix_grid.to_text g)
    ]} *)
module Table : sig
  (** {1:sharing Column widths} *)

  type column = {
    floor : int;  (** The narrowest the column goes. *)
    min : int;  (** Its min-content width. *)
    max : int;  (** Its max-content width. *)
    grow : int;  (** Its weight in the room beyond every max-content width. *)
  }
  (** The type for what a column needs, in cells. A column of a fixed width [n]
      is [{ floor = n; min = n; max = n; grow = 0 }], and bounds on a column's
      width are bounds on its fields. *)

  val widths : column array -> room:int -> int array
  (** [widths cols ~room] is the width of each column of [cols] in a room of
      [room] cells, by the rule of the module's preamble. It sums to less than
      [room] when the room is wider than the max-content widths and no column
      grows, and to more when it is narrower than the floors.

      Raises [Invalid_argument] unless [0 <= floor <= min <= max] and
      [grow >= 0] for every column. *)

  (** {1:placing Placing and drawing}

      A table's region starts at column [x] and row [y], its edge included, and
      every value below measures it with the same frame. *)

  type frame = {
    edge : Matrix_grid.Lines.weight option;
        (** The weight of the edge, [None] for none. *)
    line : Matrix_grid.Lines.weight option;
        (** The weight of the lines between columns, [None] for none. *)
    pad : int;  (** The blank cells between a cell and a line or a cell. *)
  }
  (** The type for frames. An edge or a line is one cell wide, the cell its
      lines pass through, so the glyphs it is drawn with
      ({!Matrix_grid.Lines.glyphs}) must be one cell wide. A wider glyph covers
      the padding or the content beside it. *)

  (** The type for rows. A height below [0] counts as [0]. *)
  type row =
    | Rule of Matrix_grid.Lines.weight
        (** A line of that weight across the table, one row high. *)
    | Row of int  (** A row of that height with a cell for each column. *)
    | Spanned of int * int list
        (** A row of that height whose cells span the given numbers of columns,
            in order. *)

  val chrome : frame -> int -> int
  (** [chrome f n] is the number of cells the frame [f] takes across a table of
      [n] columns, what the table's width leaves its columns. *)

  val width : frame -> int array -> int
  (** [width f ws] is the width of the table whose columns are [ws] wide. *)

  val height : frame -> row list -> int
  (** [height f rows] is the height of the table of [rows]. *)

  type placed = {
    row : Matrix_grid.region;  (** The row across the table. *)
    cells : Matrix_grid.region array;
        (** Its cells, left to right, [[||]] for a rule. *)
  }
  (** The type for a placed row. *)

  val place : x:int -> y:int -> frame -> int array -> row list -> placed list
  (** [place ~x ~y f ws rows] is where each of [rows] goes, in order, in the
      table whose columns are [ws] wide. A cell spanning columns covers them and
      the frame between them.

      Raises [Invalid_argument] if the spans of a [Spanned] row are not all
      positive or do not sum to the number of columns. *)

  val draw :
    ?glyphs:Matrix_grid.Lines.glyphs ->
    ?style:Ansi.Style.t ->
    Matrix_grid.t ->
    x:int ->
    y:int ->
    frame ->
    int array ->
    row list ->
    unit
  (** [draw ~glyphs ~style g ~x ~y f ws rows] draws the lines of the table that
      {!place} places into [g], in [glyphs] ({!Matrix_grid.Lines.Unicode} by
      default) and [style] ({!Ansi.Style.default} by default). The lines between
      two cells of a row reach the rule or edge above and below the row, and a
      row's spans leave out the lines inside them. It writes no other cell.

      Raises as {!place}. *)

  (** {1:text Tables of text}

      A {e table of text} is a table whose cells hold text, which {!grid} lays
      out and draws whole. Each column has an alignment and an {e overflow}. A
      column that wraps breaks its cells' lines at words, as
      {!Matrix_text.Layout} does with [`Word], so its min-content width is its
      widest word. A column that truncates shows a line that does not fit up to
      the last cell, which shows an ellipsis (…), so its min-content width is
      its widest line, as CSS's [white-space: nowrap] makes it. Every column's
      floor is one cell, it does not grow, and its max-content width is its
      widest line, so the table is as wide as its content when the room holds
      it. Below the min-content widths, the wrapped columns narrow to their
      floors first and the truncated columns keep their widths, and only then do
      the truncated columns narrow ({!widths} shares each step). A wrapped
      column that breaks words keeps all its text, and a truncated one loses
      some, so the column that loses text yields last. A row is as tall as its
      tallest cell. TABs are two cells wide. *)

  type align = [ `Left | `Center | `Right ]
  (** The type for the alignment of each line of a cell in its column, by its
      cells up to the last that is not a blank. A centered line has the one cell
      left over, if any, on its right. *)

  type overflow = [ `Wrap | `Truncate ]
  (** The type for overflows. *)

  type text_column = { align : align; overflow : overflow }
  (** The type for the columns of a table of text. *)

  type cell = { text : string; style : Ansi.Style.t }
  (** The type for the cells of a table of text: a text, whose line ends start
      new lines, and its style. *)

  val cell : ?style:Ansi.Style.t -> string -> cell
  (** [cell ~style text] is the cell of [text] in [style], {!Ansi.Style.default}
      by default. *)

  (** The type for the rows of a table of text. *)
  type text_row =
    | Cells of cell array  (** A row of one cell for each column. *)
    | Separator of Matrix_grid.Lines.weight
        (** A rule of that weight across the table. *)

  val grid :
    ?glyphs:Matrix_grid.Lines.glyphs ->
    ?style:Ansi.Style.t ->
    ?width_method:Matrix_text.width_method ->
    frame ->
    room:int ->
    text_column array ->
    text_row list ->
    Matrix_grid.t
  (** [grid ~glyphs ~style ~width_method f ~room cols rows] is a grid with the
      table of text of frame [f], columns [cols] and rows [rows] drawn in it,
      its lines in [glyphs] and [style] as {!draw} draws them. The table's
      columns share [room] cells less the frame's ({!widths}), measured with
      [width_method] ([`Unicode] by default), which the grid keeps. The grid is
      as wide and as tall as the table, and at least one cell each way. It is
      wider than [room] when even the floors do not fit, and the program then
      cuts its lines.

      Raises [Invalid_argument] if a row of [rows] does not have one cell for
      each column. *)
end

(** Trees, the guides that draw them one node per line.

    A tree is drawn one node per line, depth first, as [tree(1)] lists a
    directory. A node below the roots stands behind one {e guide} piece for each
    of its ancestors below the roots, outermost first, and one for itself. An
    ancestor's piece is a {e pipe} when the ancestor has a sibling after it,
    since the line to that sibling passes this node, and a {e space} otherwise.
    The node's own piece is a {e branch} when it has a sibling after it and a
    {e last} when it is the last of its siblings. A root stands behind no piece.
    The node's other lines, such as the rest of a label that wraps, stand behind
    its {e rails}, the pieces of its ancestors followed by a pipe or a space for
    itself, so the lines to its later siblings and to its children go on beside
    them.

    A node is told by its {e lasts}, whether it and each of its ancestors below
    the roots is the last of its siblings, innermost first, [[]] for a root.
    {!Tree.fold} gives every node its lasts, and {!Tree.prefix} and
    {!Tree.rails} turn them into text.

    This prints a tree of strings:
    {[
    type node = N of string * node list

    let print roots =
      let open Matrix_cli in
      Tree.fold
        ~children:(fun (N (_, c)) -> c)
        (fun lasts (N (label, _)) () ->
          print_endline (Tree.prefix Tree.single lasts ^ label))
        roots ()
    ]} *)
module Tree : sig
  type guide = {
    branch : string;  (** Before a node with a sibling after it. *)
    last : string;  (** Before the last of its siblings. *)
    pipe : string;  (** Under an ancestor with a sibling after it. *)
    space : string;  (** Under an ancestor without one. *)
  }
  (** The type for guide pieces. The four pieces must be equally wide, or the
      nodes of one depth do not line up. *)

  val single : guide
  (** [single] is the pieces of light lines ["├── "], ["└── "], ["│   "] and
      ["    "]. *)

  val ascii : guide
  (** [ascii] is the pieces of ASCII characters ["|-- "], ["`-- "], ["|   "] and
      ["    "], as [tree --charset=ascii] draws them. *)

  val fold :
    children:('a -> 'a list) ->
    (bool list -> 'a -> 'acc -> 'acc) ->
    'a list ->
    'acc ->
    'acc
  (** [fold ~children f roots acc] folds [f] over [roots] and their descendants,
      depth first, each node with its lasts. A node's children are
      [children node], so a collapsed node is one whose [children] is [[]]. *)

  val prefix : guide -> bool list -> string
  (** [prefix g lasts] is the text before the first line of the node of [lasts],
      its pieces in [g]. *)

  val rails : guide -> bool list -> string
  (** [rails g lasts] is the text before the other lines of the node of [lasts],
      its rails in [g]. *)
end
