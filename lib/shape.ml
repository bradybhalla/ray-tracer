open Math
open Shapes

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

let intersect (s : t) ray =
  match s with
  | Sphere v -> Sphere.intersect v ray
  | Plane v -> Plane.intersect v ray
  | Triangle v -> Triangle.intersect v ray

let transform (s : t) (tr : Transform.t) : t =
  match s with
  | Sphere v -> Sphere (Sphere.transform v tr)
  | Plane v -> Plane (Plane.transform v tr)
  | Triangle v -> Triangle (Triangle.transform v tr)

let sample_point (s : t) : Vec3.t =
  match s with
  | Sphere v -> Sphere.sample_point v
  | Triangle v -> Triangle.sample_point v
  | Plane _ -> failwith "not implemented yet"

