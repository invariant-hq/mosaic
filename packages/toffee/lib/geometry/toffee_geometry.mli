(** Geometric primitives useful for layout.

    This module provides foundational types for representing dimensions,
    positions, and constraints used throughout the layout engine. *)

(** {1 Axis types} *)

module Absolute_axis = Absolute_axis
module Abstract_axis = Abstract_axis

(** {1 Geometric types} *)

module Size = Size
module Point = Point
module Line = Line
module Rect = Rect
module Min_max = Min_max
module In_both_abs_axis = In_both_abs_axis

(** {1 Geometric types of floats}

    The concrete sizes, rectangles and points that layout computes, with their
    floats stored flat. *)

module Sizef = Sizef
module Rectf = Rectf
module Pointf = Pointf

(** {1 Type aliases} *)

type absolute_axis = Absolute_axis.t
(** Alias for {!Absolute_axis.t}. *)

type abstract_axis = Abstract_axis.t
(** Alias for {!Abstract_axis.t}. *)

type 'a rect = 'a Rect.t
(** Alias for {!Rect.t}. *)

type 'a size = 'a Size.t
(** Alias for {!Size.t}. *)

type 'a point = 'a Point.t
(** Alias for {!Point.t}. *)

type 'a line = 'a Line.t
(** Alias for {!Line.t}. *)

type ('min, 'max) min_max = ('min, 'max) Min_max.t
(** Alias for {!Min_max.t}. *)

type 'a in_both_abs_axis = 'a In_both_abs_axis.t
(** Alias for {!In_both_abs_axis.t}. *)
