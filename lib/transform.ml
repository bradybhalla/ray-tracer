open Math
open Utils

let rotate_vec axis angle v =
  let s = sin angle in
  let c = cos angle in
  Vec3.copy v
  |> Vec3.mul_add (Vec3.cross' axis v) s
  |> Vec3.mul_add (Vec3.cross' axis (Vec3.cross' axis v)) (1.0 -. c)

type single =
  | Rotation of Vec3.t * float (* axis must be normalized *)
  | Translation of Vec3.t
  | Scale of Vec3.t

(* items on the left get applied first *)
(* TODO: storing as a list is nice for doing calculations easily,
           but a matrix could be more efficient if there are multiple
           transformations
   *)
type t = single list

let identity = []

let single_transform_point p s =
  match s with
  | Rotation (axis, angle) -> rotate_vec axis angle p
  | Translation pos -> Vec3.copy p |> Vec3.add pos
  | Scale scale ->
      Vec3.copy p |> Vec3.mul scale

let single_transform_vec v s =
  match s with
  | Rotation (axis, angle) -> rotate_vec axis angle v
  | Translation _ -> v
  | Scale scale -> Vec3.copy v |> Vec3.mul scale

let single_inv_transpose_transform_vec v s =
  match s with
  | Rotation (axis, angle) -> rotate_vec axis angle v
  | Translation _ -> v
  | Scale scale ->
      Vec3.creater
        {
          x = Vec3.x v /. Vec3.x scale;
          y = Vec3.y v /. Vec3.y scale;
          z = Vec3.z v /. Vec3.z scale;
        }

(** [compose fst snd] applies [fst] and then [snd] *)
let compose : t -> t -> t = ( @ )

let inv (tr : t) : t =
  let inv_s = function
    | Rotation (axis, angle) -> Rotation (axis, -.angle)
    | Translation v -> Translation (Vec3.copy v |> Vec3.cmul (-1.0))
    | Scale v ->
        Scale
          (Vec3.creater
             { x = 1.0 /. Vec3.x v; y = 1.0 /. Vec3.y v; z = 1.0 /. Vec3.z v })
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
    normal = normal transform si.normal;
    tex_coord = si.tex_coord;
    medium_transition_dir = si.medium_transition_dir;
  }
