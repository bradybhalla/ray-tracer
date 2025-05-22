open Utils

module PrimShape = struct
  module type RequiredInterface = sig
    type t
    type params

    val create : params -> t
    val intersect : t -> Ray.t -> (float * shape_intersection) option
    val transform : t -> Transform.t -> t
  end

  (* constrain shapes to the required interface for primitives *)
  module Sphere : RequiredInterface with type params = Shape_sphere.params =
    Shape_sphere

  module Plane : RequiredInterface with type params = Shape_plane.params =
    Shape_plane

  module Triangle : RequiredInterface with type params = Shape_triangle.params =
    Shape_triangle

  type t = Sphere of Sphere.t | Plane of Plane.t | Triangle of Triangle.t

  type params =
    | SphereParams of Sphere.params
    | PlaneParams of Plane.params
    | TriangleParams of Triangle.params

  let create (p : params) =
    match p with
    | SphereParams p -> Sphere (Sphere.create p)
    | PlaneParams p -> Plane (Plane.create p)
    | TriangleParams p -> Triangle (Triangle.create p)

  let intersect (v : t) ray =
    match v with
    | Sphere v -> Sphere.intersect v ray
    | Plane v -> Plane.intersect v ray
    | Triangle v -> Triangle.intersect v ray

  let transform (v : t) (tr : Transform.t) : t =
    match v with
    | Sphere v -> Sphere (Sphere.transform v tr)
    | Plane v -> Plane (Plane.transform v tr)
    | Triangle v -> Triangle (Triangle.transform v tr)
end

type t = { shape : PrimShape.t; material : Material.t; medium : Medium.spec }

let get_intersection (ray : Ray.t) (prim : t) =
  let shape_int = PrimShape.intersect prim.shape ray in
  match shape_int with
  | None -> None
  | Some (t, si) ->
      Some
        ( t,
          {
            si;
            material = prim.material;
            medium_incident =
              Medium.get_incident prim.medium si.medium_transition;
            medium_transmitted =
              Medium.get_transmitted prim.medium si.medium_transition;
          } )
