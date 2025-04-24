type rgb = int * int * int

(* data in row major format *)
type t = {
  width : int;
  height : int;
  max_color_value : int;
  data : rgb list list;
}

let default_max_val = 255
let gamma_correction = 1.0 /. 2.2

let to_string (ppm : t) (_mode : [ `P3 ]) =
  let header =
    Printf.sprintf "P3\n%d %d\n%d\n" ppm.width ppm.height ppm.max_color_value
  in
  let pixel_to_str (r, g, b) = Printf.sprintf "%d %d %d" r g b in
  let row_strings =
    List.map (fun row -> String.concat " " (List.map pixel_to_str row)) ppm.data
  in
  header ^ String.concat "\n" row_strings ^ "\n"

let to_array (ppm : t) =
  let _ = ppm in
  failwith "TODO"

let of_render (render : Render.t) =
  let val_of_color color =
    int_of_float
      (Float.pow color gamma_correction *. float_of_int default_max_val)
  in
  let `Col width, `Row height = Render.get_dim render in
  let data =
    List.init height (fun y ->
        List.init width (fun x ->
            let color = Render.get_pixel_color render (`Col x) (`Row y) in
            (val_of_color color.x, val_of_color color.y, val_of_color color.z)))
  in
  { width; height; max_color_value = default_max_val; data }

let of_file (filename : string) (_mode : [ `P6 ]) =
  let _ = filename in
  failwith "TODO"

let print (ppm : t) = to_string ppm `P3 |> print_string

let save (filename : string) (ppm : t) =
  let oc = open_out filename in
  to_string ppm `P3 |> output_string oc;
  close_out oc
