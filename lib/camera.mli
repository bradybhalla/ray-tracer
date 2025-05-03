open Math
open Utils

type t

val create : ?pos:Vec3.t -> ?look_at:Vec3.t -> pixel_height:int -> unit -> t
(** Create a new camera instance pointing to a position. *)

val get_pixel_dim : t -> [ `Col of int ] * [ `Row of int ]
(** Get the pixel dimensions of the camera. *)

val get_ray : t -> [ `Col of int ] -> [ `Row of int ] -> Ray.t
(** Calculate the ray corresponding to a specific pixel of the camera. *)
