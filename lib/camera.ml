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
    pixel_jitter = false;
    (* TODO: set back to true *)
    pixel_height;
  }

(* pixel dimension of the camera *)
let get_pixel_dim camera =
  let cols =
    int_of_float (camera.aspect_ratio *. float_of_int camera.pixel_height)
  in
  let rows = camera.pixel_height in
  (`Col cols, `Row rows)

let get_ray camera (`Col c) (`Row r) : Ray.t =
  (* dimension of the camera in scene coords if its origin was a distance of 1 away *)
  let virtual_width = camera.aspect_ratio *. 2.0 *. tan (camera.fov /. 2.0) in
  let virtual_height = 2.0 *. tan (camera.fov /. 2.0) in
  (* dimension of the output image in pixels *)
  let `Col cols, `Row rows = get_pixel_dim camera in
  let jitter () = if camera.pixel_jitter then Random.float 1.0 else 0.5 in
  let x =
    ((float_of_int c +. jitter ()) /. float_of_int cols *. virtual_width)
    -. (virtual_width /. 2.0)
  in
  let y =
    ((float_of_int r +. jitter ()) /. float_of_int rows *. virtual_height)
    -. (virtual_height /. 2.0)
  in
  let dir = (camera.ex *@ x) +@ (camera.ey *@ y) +@ camera.ez in
  let origin = camera.pos in
  Ray.create ~origin ~dir |> Ray.normalize


let _todo_remove_get_ray_float camera (`Col c) (`Row r) : Ray.t =
  (* dimension of the camera in scene coords if its origin was a distance of 1 away *)
  let virtual_width = camera.aspect_ratio *. 2.0 *. tan (camera.fov /. 2.0) in
  let virtual_height = 2.0 *. tan (camera.fov /. 2.0) in
  (* dimension of the output image in pixels *)
  let `Col cols, `Row rows = get_pixel_dim camera in
  let x =
    ((c +. 0.5) /. float_of_int cols *. virtual_width)
    -. (virtual_width /. 2.0)
  in
  let y =
    ((r +. 0.5) /. float_of_int rows *. virtual_height)
    -. (virtual_height /. 2.0)
  in
  let dir = (camera.ex *@ x) +@ (camera.ey *@ y) +@ camera.ez in
  let origin = camera.pos in
  Ray.create ~origin ~dir |> Ray.normalize

(*
TODO: This does an expensive level of detail optimization by recomputing
shape intersections to see change in texture coords. It also may be wrong
around the edges when there is no texture coordinate estimate.

A better way would be to have the shapes return tangent planes with a vector
for the du direction and for the dv direction. Then, pixel changes can be
projected onto this tangent plane to get du/dx, dv/dx, ...

This is about as expensive as a plane intersection and doesn't fail around edges.
It also will make it so the `shape` argument is not needed.
*)
let get_level_of_detail (camera : t) (`Col x) (`Row y) (shape : Shape.t)
    (si : shape_intersection) =
  let get_offset_si (`Col x) (`Row y) =
    let ray = _todo_remove_get_ray_float camera (`Col x) (`Row y) in
    Shape.intersect shape ray
  in
  let x = float_of_int x in
  let y = float_of_int y in
  let eps = 0.0001 in
  let int_dx = get_offset_si (`Col (x +. eps)) (`Row y) in
  let int_dy = get_offset_si (`Col x) (`Row (y +. eps)) in
  match (int_dx, int_dy) with
  | Some (_, si_dx), Some (_, si_dy) ->
      let du_dx = si_dx.tex_coord.u -. si.tex_coord.u in
      let dv_dx = si_dx.tex_coord.v -. si.tex_coord.v in
      let du_dy = si_dy.tex_coord.u -. si.tex_coord.u in
      let dv_dy = si_dy.tex_coord.v -. si.tex_coord.v in
      let dx = sqrt ((du_dx *. du_dx) +. (dv_dx *. dv_dx)) /. eps in
      let dy = sqrt ((du_dy *. du_dy) +. (dv_dy *. dv_dy)) /. eps in
      max dx dy (* how much the tex coord changes when we change a single pixel *)
  | _, _ -> 1.0
