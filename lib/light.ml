open Math
open Utils

module LightShape = struct
  module type RequiredInterface = sig
    type t
    type params

    val create : params -> t
    val transform : t -> Transform.t -> t
    val sample_point : t -> Vec3.t
    val intersect : t -> Ray.t -> (float * shape_intersection) option
  end

  (* constrain shapes to the required interface for light sources *)
  module Sphere : RequiredInterface with type params = Shape_sphere.params =
    Shape_sphere

  module Point : RequiredInterface with type params = Shape_point.params =
    Shape_point

  type t = Sphere of Sphere.t | Point of Point.t
  type params = SphereParams of Sphere.params | PointParams of Point.params

  let create (p : params) =
    match p with
    | SphereParams p -> Sphere (Sphere.create p)
    | PointParams p -> Point (Point.create p)

  let transform (v : t) (tr : Transform.t) : t =
    match v with
    | Sphere v -> Sphere (Sphere.transform v tr)
    | Point v -> Point (Point.transform v tr)

  let sample_point (v : t) : Vec3.t =
    match v with
    | Sphere v -> Sphere.sample_point v
    | Point v -> Point.sample_point v

  let intersect (v : t) (ray : Ray.t) : (float * shape_intersection) option =
    match v with
    | Sphere v -> Sphere.intersect v ray
    | Point v -> Point.intersect v ray
end

type t =
  | GeometryLight of { shape : LightShape.t; power : float }
  | InfiniteLight of {power: float}

let get_sample (light : t) (pos : Vec3.t) =
  match light with
  | GeometryLight { shape; power } ->
      let light_pos = LightShape.sample_point shape in
      let pos_to_light = light_pos -@ pos in
      Some
        {
          light_pos;
          dir_to_light = Vec3.normalize pos_to_light;
          dist_to_light = Vec3.mag pos_to_light;
          power;
        }
  | InfiniteLight _ -> None

let get_intersection (ray : Ray.t) (light : t) =
  match light with
  | GeometryLight { shape; power } ->
      LightShape.intersect shape ray
      |> Option.fold ~none:None ~some:(fun (t, i) ->
             Some (t, { pos = i.point; power }))
  | InfiniteLight {power} ->
      Some (Float.max_float, { pos = Ray.at ray 999.0; power })
