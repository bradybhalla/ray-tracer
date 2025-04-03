open Ray_tracer.Math

type t = {
  fov : float;
  aspect_ratio : float;
  near : float;
  far : float;
  pixel_rows : int;
}

let create ~fov ~aspect_ratio ~pixel_rows =
  { fov; aspect_ratio; near = 5.0; far = 100.0; pixel_rows }

let get_virtual_dim camera =
  let width =
    camera.aspect_ratio *. 2.0 *. tan (camera.fov /. 2.0)
  in
  let height = 2.0 *. tan (camera.fov /. 2.0) in
  (`Width width, `Height height)

let get_output_dim camera =
  let cols =
    int_of_float (camera.aspect_ratio *. float_of_int camera.pixel_rows)
  in
  let rows = camera.pixel_rows in
  (`Col cols, `Row rows)

let get_ray camera (`Col c) (`Row r) : Ray.t =
  let `Width width, `Height height = get_virtual_dim camera in
  let `Col cols, `Row rows = get_output_dim camera in
  let x = (float_of_int c +. 0.5) /. float_of_int cols *. width -. width /. 2.0 in
  let y = (float_of_int r +. 0.5) /. float_of_int rows *. height -. height /. 2.0 in
  let dir = Vec3.create x y 1.0 |> Vec3.normalize in
  let origin = Vec3.(Random.(create (float 0.1) (float 0.1) (float 0.1) -@ create 0.05 0.05 0.05)) in
  { origin; dir }
