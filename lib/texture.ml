open Math

type image = int * int * Vec3.t array array
type tex_coord = { u : float; v : float }

type t =
  | Constant of Vec3.t
  | Checkered of int * int * Vec3.t * Vec3.t
  | Image of image
  | MipmapImage of (int * t array)
  | MipmapDebug

let create_mipmap ((rows, cols, a) : image) =
  let rec gen_levels (rows, cols, a) acc =
    if rows <= 1 || cols <= 1 then List.rev ((rows, cols, a) :: acc)
    else
      let level_rows = rows lsr 1 in
      let level_cols = cols lsr 1 in
      let level_data =
        Array.make_matrix level_rows level_cols (Vec3.create 0.0 0.0 0.0)
      in
      for r = 0 to level_rows - 1 do
        for c = 0 to level_cols - 1 do
          let r0 = r * 2 in
          let c0 = c * 2 in
          let r1 = r0 + 1 in
          let c1 = c0 + 1 in
          level_data.(r).(c) <-
            (a.(r0).(c0) +@ a.(r0).(c1) +@ a.(r1).(c0) +@ a.(r1).(c1)) /@ 4.0
        done
      done;
      gen_levels (level_rows, level_cols, level_data) ((rows, cols, a) :: acc)
  in
  let levels = gen_levels (rows, cols, a) [] in
  MipmapImage (min rows cols, levels |> List.map (fun i -> Image i) |> Array.of_list)

let rec eval tex u v lod =
  let u = clamp 0.0 1.0 u in
  let v = clamp 0.0 1.0 v in
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
  | MipmapImage (dtexpix_dcoord, mipmap) ->
      let dcoord_dpix = lod in
      (* ratio of texture pixel to screen pixel *)
      let dtexpix_dpix = dcoord_dpix *. float_of_int dtexpix_dcoord in
      let lod = log dtexpix_dpix /. log 2.0 in
      let lod =
        lod |> max 0.0 |> min (float_of_int (Array.length mipmap - 1))
      in
      let lower = int_of_float lod in
      let upper =
        if lower = Array.length mipmap - 1 then lower else lower + 1
      in
      let frac = decimal lod in
      let c1 = eval mipmap.(lower) u v 0.0 in
      let c2 = eval mipmap.(upper) u v 0.0 in
      (c1 *@ (1.0 -. frac)) +@ (c2 *@ frac)
  | MipmapDebug ->
      (* how fast texture coord changes with pixel *)
      let colors =
        [|
          (* red *) Vec3.create 1.0 0.0 0.0;
          (* orange *) Vec3.create 1.0 0.5 0.0;
          (* yellow *) Vec3.create 1.0 1.0 0.0;
          (* green *) Vec3.create 0.0 1.0 0.0;
          (* blue *) Vec3.create 0.0 0.0 1.0;
          (* purple *) Vec3.create 1.0 0.0 1.0;
        |]
      in
      let dcoord_dpix = lod in
      (* best (average?) resolution of texture image *)
      (* TODO: how to handle x/y *)
      let dtexpix_dcoord = 1024.0 in
      (* ratio of texture pixel to screen pixel *)
      let dtexpix_dpix = dcoord_dpix *. dtexpix_dcoord in
      let lod = log dtexpix_dpix /. log 2.0 in
      let lod =
        lod |> max 0.0 |> min (float_of_int (Array.length colors - 1))
      in
      let lower = int_of_float lod in
      let upper =
        if lower = Array.length colors - 1 then lower else lower + 1
      in
      let frac = decimal lod in
      let c1 = colors.(lower) in
      let c2 = colors.(upper) in
      (c1 *@ (1.0 -. frac)) +@ (c2 *@ frac)
