open Math
open Utils

let rotate_vec axis angle v =
  let s = sin angle in
  let c = cos angle in
  v
  +@ (Vec3.cross axis v *@ s)
  +@ (Vec3.cross axis (Vec3.cross axis v) *@ (1.0 -. c))

type single =
  | Rotation of Vec3.t * float (* axis must be normalized *)
  | Translation of Vec3.t
  | Scale of Vec3.t

(* items on the left get applied first *)
type t = single list

let identity = []

let single_transform_point p s =
  match s with
  | Rotation (axis, angle) -> rotate_vec axis angle p
  | Translation pos -> p +@ pos
  | Scale scale ->
      { x = p.x *. scale.x; y = p.y *. scale.y; z = p.z *. scale.z }

let single_transform_vec v s =
  match s with
  | Rotation (axis, angle) -> rotate_vec axis angle v
  | Translation _ -> v
  | Scale scale ->
      { x = v.x *. scale.x; y = v.y *. scale.y; z = v.z *. scale.z }

let single_inv_transpose_transform_vec v s =
  match s with
  | Rotation (axis, angle) -> rotate_vec axis angle v
  | Translation _ -> v
  | Scale scale ->
      { x = v.x /. scale.x; y = v.y /. scale.y; z = v.z /. scale.z }

(** [compose fst snd] applies [fst] and then [snd] *)
let compose : t -> t -> t = ( @ )

let inv (tr : t) : t =
  let inv_s = function
    | Rotation (axis, angle) -> Rotation (axis, -.angle)
    | Translation v -> Translation (v *@ -1.0)
    | Scale v -> Scale { x = 1.0 /. v.x; y = 1.0 /. v.y; z = 1.0 /. v.z }
  in
  List.fold_left (fun rest s -> inv_s s :: rest) [] tr

(** Apply a transformation to a point *)
let point (tr : t) (point : Vec3.t) : Vec3.t =
  List.fold_left single_transform_point point tr

(** Apply a transformation to a vector (no translation) *)
let vec (tr : t) (vec : Vec3.t) : Vec3.t =
  List.fold_left single_transform_vec vec tr

(** Apply a transformation to a normal vector by applying the inverse transpose
    to a regular vector. Normalizes the result. *)
let normal (tr : t) (vec : Vec3.t) : Vec3.t =
  List.fold_left single_inv_transpose_transform_vec vec tr |> Vec3.normalize

let ray (transform : t) (ray : Ray.t) : Ray.t =
  let origin = point transform ray.origin in
  let dir = vec transform ray.dir in
  { origin; dir }

let shape_intersection transform (si : shape_intersection) =
  {
    point = point transform si.point;
    outward_normal = normal transform si.outward_normal;
    tex_coord = si.tex_coord;
    medium_transition = si.medium_transition;
  }
