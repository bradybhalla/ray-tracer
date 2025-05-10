open Math

type tex_coord = { u : float; v : float }

type t =
  | Constant of Vec3.t
  | Checkered of int * int * Vec3.t * Vec3.t
  | Image of int * int * Vec3.t array array

let eval tex u v =
  let u = clamp 0.0 1.0 u in
  let v = clamp 0.0 1.0 v in
  match tex with
  | Constant c -> Vec3.copy c
  | Checkered (nu, nv, c1, c2) ->
      if
        (int_of_float (floor (float_of_int nu *. u))
        + int_of_float (floor (float_of_int nv *. v)))
        mod 2
        = 0
      then Vec3.copy c1
      else Vec3.copy c2
  | Image (rows, cols, a) ->
      let r = int_of_float (floor (float_of_int rows *. u)) in
      let c = int_of_float (floor (float_of_int cols *. v)) in
      (* TODO: add interpolation *)
      Vec3.copy a.(r).(c)
