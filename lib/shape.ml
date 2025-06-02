open Shapes
open Utils

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

let intersect ~(shape : t) ~ray =
  match shape with
  | Sphere v -> Sphere.intersect v ray
  | Plane v -> Plane.intersect v ray
  | Triangle v -> Triangle.intersect v ray

let transform ~shape ~tr : t =
  match shape with
  | Sphere v -> Sphere (Sphere.transform v tr)
  | Plane v -> Plane (Plane.transform v tr)
  | Triangle v -> Triangle (Triangle.transform v tr)

let sample (shape : t) : shape_sample =
  match shape with
  | Sphere v -> Sphere.sample v
  | Triangle v -> Triangle.sample v
  | Plane _ -> failwith "cannot sample from infinite shape"
