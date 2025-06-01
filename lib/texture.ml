open Math

type tex_coord = { u : float; v : float }

type t =
  | Constant of Vec3.t
  | Checkered of int * int * Vec3.t * Vec3.t
  | Image of int * int * Vec3.t array array

let eval tex tex_coord =
  let u = safe_clamp ~minv:0.0 ~maxv:1.0 tex_coord.u in
  let v = safe_clamp ~minv:0.0 ~maxv:1.0 tex_coord.v in
  match tex with
  | Constant c -> c
  | Checkered (nu, nv, c1, c2) ->
      if
        (int_of_float (floor (float_of_int nu *. u))
        + int_of_float (floor (float_of_int nv *. v)))
        mod 2
        = 0
      then c1
      else c2
  | Image (rows, cols, a) ->
      let r = int_of_float (floor (float_of_int rows *. u)) in
      let c = int_of_float (floor (float_of_int cols *. v)) in
      (* TODO: add interpolation *)
      a.(r).(c)
