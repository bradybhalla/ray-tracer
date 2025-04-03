open Math

type t = {
  fov : float;
  aspect_ratio : float;
  near : float;
  far : float;
  pixel_height : int;
  antialiasing_factor : float;
}

let create ?(fov = 0.5) ?(aspect_ratio = 16.0 /. 9.0)
    ?(antialiasing_factor = 0.05) pixel_height =
  {
    fov;
    aspect_ratio;
    near = 5.0;
    far = 100.0;
    pixel_height;
    antialiasing_factor;
  }

let get_virtual_dim camera =
  let width = camera.aspect_ratio *. 2.0 *. tan (camera.fov /. 2.0) in
  let height = 2.0 *. tan (camera.fov /. 2.0) in
  (`Width width, `Height height)

let get_output_dim camera =
  let cols =
    int_of_float (camera.aspect_ratio *. float_of_int camera.pixel_height)
  in
  let rows = camera.pixel_height in
  (`Col cols, `Row rows)

let get_ray camera (`Col c) (`Row r) : Ray.t =
  let `Width width, `Height height = get_virtual_dim camera in
  let `Col cols, `Row rows = get_output_dim camera in
  let x =
    ((float_of_int c +. 0.5) /. float_of_int cols *. width) -. (width /. 2.0)
  in
  let y =
    ((float_of_int r +. 0.5) /. float_of_int rows *. height) -. (height /. 2.0)
  in
  let dir = Vec3.create x y 1.0 |> Vec3.normalize in
  let origin = Vec3.(Sample.unit_vec3 () *@ camera.antialiasing_factor) in
  { origin; dir }
