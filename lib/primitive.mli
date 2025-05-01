open Utils

type t = { shape : Shape.t; material : Material.t; medium : Medium.spec }

val get_intersection :
  t -> float * shape_intersection -> (float * intersection) option
(** find an intersection with a primitive, returning the intersection time and
    info it it exists *)

val get_first_intersection : t list -> Ray.t -> (float * intersection) option
(** find the first intersection in a list of primitives *)
