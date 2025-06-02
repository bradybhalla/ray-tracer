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
      let r_float = float_of_int rows *. u in
      let c_float = float_of_int cols *. v in
      let r0 = int_of_float (floor r_float) in
      let r1 = (r0 + 1) mod rows in
      let r1_prop = decimal r_float in
      let c0 = int_of_float (floor c_float) in
      let c1 = (c0 + 1) mod cols in
      let c1_prop = decimal c_float in
      (* bilinear interpolation *)
      (a.(r0).(c0) *@ ((1.0 -. r1_prop) *. (1.0 -. c1_prop)))
      +@ (a.(r0).(c1) *@ ((1.0 -. r1_prop) *. c1_prop))
      +@ (a.(r1).(c0) *@ (r1_prop *. (1.0 -. c1_prop)))
      +@ (a.(r1).(c1) *@ (r1_prop *. c1_prop))
