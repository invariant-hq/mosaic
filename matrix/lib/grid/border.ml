let u = Uchar.of_int

type t = {
  top_left : Uchar.t;
  top_right : Uchar.t;
  bottom_left : Uchar.t;
  bottom_right : Uchar.t;
  horizontal : Uchar.t;
  vertical : Uchar.t;
  top_t : Uchar.t;
  bottom_t : Uchar.t;
  left_t : Uchar.t;
  right_t : Uchar.t;
  cross : Uchar.t;
}

type side = [ `Top | `Right | `Bottom | `Left ]

let all = [ `Top; `Right; `Bottom; `Left ]

(* Constants *)
let single =
  {
    top_left = u 0x250C;
    top_right = u 0x2510;
    bottom_left = u 0x2514;
    bottom_right = u 0x2518;
    horizontal = u 0x2500;
    vertical = u 0x2502;
    top_t = u 0x252C;
    bottom_t = u 0x2534;
    left_t = u 0x251C;
    right_t = u 0x2524;
    cross = u 0x253C;
  }

let double =
  {
    top_left = u 0x2554;
    top_right = u 0x2557;
    bottom_left = u 0x255A;
    bottom_right = u 0x255D;
    horizontal = u 0x2550;
    vertical = u 0x2551;
    top_t = u 0x2566;
    bottom_t = u 0x2569;
    left_t = u 0x2560;
    right_t = u 0x2563;
    cross = u 0x256C;
  }

let rounded =
  {
    single with
    top_left = u 0x256D;
    top_right = u 0x256E;
    bottom_left = u 0x2570;
    bottom_right = u 0x256F;
  }

let heavy =
  {
    top_left = u 0x250F;
    top_right = u 0x2513;
    bottom_left = u 0x2517;
    bottom_right = u 0x251B;
    horizontal = u 0x2501;
    vertical = u 0x2503;
    top_t = u 0x2533;
    bottom_t = u 0x253B;
    left_t = u 0x2523;
    right_t = u 0x252B;
    cross = u 0x254B;
  }

let ascii =
  let c v = Uchar.of_char v in
  {
    top_left = c '+';
    top_right = c '+';
    bottom_left = c '+';
    bottom_right = c '+';
    horizontal = c '-';
    vertical = c '|';
    top_t = c '+';
    bottom_t = c '+';
    left_t = c '+';
    right_t = c '+';
    cross = c '+';
  }

let empty =
  let s = Uchar.of_char ' ' in
  {
    top_left = s;
    top_right = s;
    bottom_left = s;
    bottom_right = s;
    horizontal = s;
    vertical = s;
    top_t = s;
    bottom_t = s;
    left_t = s;
    right_t = s;
    cross = s;
  }

let modify ?top_left ?top_right ?bottom_left ?bottom_right ?horizontal ?vertical
    ?top_t ?bottom_t ?left_t ?right_t ?cross t =
  let d default opt = Option.value ~default opt in
  {
    top_left = d t.top_left top_left;
    top_right = d t.top_right top_right;
    bottom_left = d t.bottom_left bottom_left;
    bottom_right = d t.bottom_right bottom_right;
    horizontal = d t.horizontal horizontal;
    vertical = d t.vertical vertical;
    top_t = d t.top_t top_t;
    bottom_t = d t.bottom_t bottom_t;
    left_t = d t.left_t left_t;
    right_t = d t.right_t right_t;
    cross = d t.cross cross;
  }
