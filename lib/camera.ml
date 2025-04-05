open Math

type t = {
  fov : float;
  aspect_ratio : float;
  pos : Vec3.t;
  ex : Vec3.t;
  ey : Vec3.t;
  ez : Vec3.t;
  pixel_jitter : bool;
  pixel_height : int;
}

let create ?(pos = Vec3.create 0.0 0.0 (-10.0)) ?(look_at = Vec3.zero)
    ~pixel_height () =
  let ez = look_at -@ pos |> Vec3.normalize in
  let ex = Vec3.cross (Vec3.create 0.0 1.0 0.0) ez |> Vec3.normalize in
  let ey = Vec3.cross ez ex |> Vec3.normalize in
  {
    fov = 1.12;
    aspect_ratio = 16.0 /. 9.0;
    pos;
    ex;
    ey;
    ez;
    pixel_jitter = true;
    pixel_height;
  }

let get_virtual_dim camera =
  let width = camera.aspect_ratio *. 2.0 *. tan (camera.fov /. 2.0) in
  let height = 2.0 *. tan (camera.fov /. 2.0) in
  (`Width width, `Height height)

let get_pixel_dim camera =
  let cols =
    int_of_float (camera.aspect_ratio *. float_of_int camera.pixel_height)
  in
  let rows = camera.pixel_height in
  (`Col cols, `Row rows)

let get_ray camera (`Col c) (`Row r) : Ray.t =
  let `Width width, `Height height = get_virtual_dim camera in
  let `Col cols, `Row rows = get_pixel_dim camera in
  let jitter () = if camera.pixel_jitter then Random.float 1.0 else 0.5 in
  let x =
    ((float_of_int c +. jitter ()) /. float_of_int cols *. width)
    -. (width /. 2.0)
  in
  let y =
    ((float_of_int r +. jitter ()) /. float_of_int rows *. height)
    -. (height /. 2.0)
  in
  let dir = (camera.ex *@ x) +@ (camera.ey *@ y) +@ camera.ez in
  let origin = camera.pos in
  { origin; dir }
