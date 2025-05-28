open Math

type t = Sphere of Shape_sphere.t | Plane of Shape_plane.t | Triangle of Shape_triangle.t

type params =
  | SphereParams of Shape_sphere.params
  | PlaneParams of Shape_plane.params
  | TriangleParams of Shape_triangle.params

let create (p : params) =
  match p with
  | SphereParams p -> Sphere (Shape_sphere.create p)
  | PlaneParams p -> Plane (Shape_plane.create p)
  | TriangleParams p -> Triangle (Shape_triangle.create p)

let intersect (s : t) ray =
  match s with
  | Sphere v -> Shape_sphere.intersect v ray
  | Plane v -> Shape_plane.intersect v ray
  | Triangle v -> Shape_triangle.intersect v ray

let transform (s : t) (tr : Transform.t) : t =
  match s with
  | Sphere v -> Sphere (Shape_sphere.transform v tr)
  | Plane v -> Plane (Shape_plane.transform v tr)
  | Triangle v -> Triangle (Shape_triangle.transform v tr)

let sample_point (s : t) : Vec3.t =
  match s with
  | Sphere v -> Shape_sphere.sample_point v
  | Triangle v -> Shape_triangle.sample_point v
  | Plane _ -> failwith "not implemented yet"

