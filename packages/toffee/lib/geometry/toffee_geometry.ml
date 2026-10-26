(* geometry.ml *)

(* Re-export axis types *)

module Absolute_axis = Absolute_axis
module Abstract_axis = Abstract_axis

(* Re-export geometric types *)

module Rect = Rect
module Size = Size
module Point = Point
module Sizef = Sizef
module Rectf = Rectf
module Pointf = Pointf
module Line = Line
module Min_max = Min_max
module In_both_abs_axis = In_both_abs_axis

(* Export types at top level for convenience *)

type absolute_axis = Absolute_axis.t
type abstract_axis = Abstract_axis.t
type 'a rect = 'a Rect.t
type 'a size = 'a Size.t
type 'a point = 'a Point.t
type 'a line = 'a Line.t
type ('min, 'max) min_max = ('min, 'max) Min_max.t
type 'a in_both_abs_axis = 'a In_both_abs_axis.t
