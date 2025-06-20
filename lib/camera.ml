open Math
open Utils

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

type params = {
  pos : Vec3.t;
  look_at : Vec3.t;
  pixel_height : int;
  fov : float;
  aspect_ratio : float;
  pixel_jitter : bool;
}

let default_params =
  {
    pos = Vec3.create 0.0 0.0 (-10.0);
    look_at = Vec3.zero ();
    pixel_height = 500;
    fov = 1.12;
    aspect_ratio = 16.0 /. 9.0;
    pixel_jitter = true;
  }

let create { pos; look_at; pixel_height; fov; aspect_ratio; pixel_jitter } =
  let ez = look_at -@ pos |> Vec3.normalize in
  let ex = Vec3.cross (Vec3.create 0.0 1.0 0.0) ez |> Vec3.normalize in
  let ey = Vec3.cross ez ex |> Vec3.normalize in
  { fov; aspect_ratio; pos; ex; ey; ez; pixel_jitter; pixel_height }

(* pixel dimension of the camera *)
let get_pixel_dim (camera : t) =
  let cols =
    int_of_float (camera.aspect_ratio *. float_of_int camera.pixel_height)
  in
  let rows = camera.pixel_height in
  (cols, rows)

let get_ray ~(camera : t) ~col ~row : Ray.t =
  (* dimension of the camera in scene coords if its origin was a distance of 1 away *)
  let virtual_width = camera.aspect_ratio *. 2.0 *. tan (camera.fov /. 2.0) in
  let virtual_height = 2.0 *. tan (camera.fov /. 2.0) in
  (* dimension of the output image in pixels *)
  let cols, rows = get_pixel_dim camera in
  let jitter () = if camera.pixel_jitter then Random.float 1.0 else 0.5 in
  let x =
    ((float_of_int col +. jitter ()) /. float_of_int cols *. virtual_width)
    -. (virtual_width /. 2.0)
  in
  let y =
    ((float_of_int row +. jitter ()) /. float_of_int rows *. virtual_height)
    -. (virtual_height /. 2.0)
  in
  let dir = (camera.ex *@ x) +@ (camera.ey *@ y) +@ camera.ez in
  let origin = camera.pos in
  Ray.create ~origin ~dir |> Ray.normalize
