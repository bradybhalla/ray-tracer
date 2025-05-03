open Math

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

let to_string (ppm : t) (mode : [ `P3 ]) =
  let header =
    Printf.sprintf "P3\n%d %d\n%d\n" ppm.width ppm.height ppm.max_color_value
  in
  let pixel_to_str (r, g, b) = Printf.sprintf "%d %d %d" r g b in
  let row_strings =
    List.map (fun row -> String.concat " " (List.map pixel_to_str row)) ppm.data
  in
  header ^ String.concat "\n" row_strings ^ "\n"

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

let to_texture (ppm : t) : Texture.t =
  let calc_value v =
    let normalized = float_of_int v /. float_of_int ppm.max_color_value in
    let corrected =
      Float.pow (clamp 0.0 1.0 normalized) (1.0 /. gamma_correction)
    in
    corrected
  in
  let rows = ppm.height in
  let cols = ppm.width in
  let data = Array.make_matrix rows cols (Vec3.create 0.0 0.0 0.0) in
  List.iteri
    (fun i row ->
      List.iteri
        (fun j (r, g, b) ->
          data.(i).(j) <-
            Vec3.create (calc_value r) (calc_value g) (calc_value b))
        row)
    ppm.data;
  Image (rows, cols, data)

let of_file (filename : string) (mode : [ `P6 ]) : t =
  let chan = open_in_bin filename in
  let read_rgb () =
    let r = input_byte chan in
    let g = input_byte chan in
    let b = input_byte chan in
    (r, g, b)
  in
  let read_to e =
    let buf = Buffer.create 16 in
    let rec aux () =
      let c = input_char chan in
      if c = e then ()
      else (
        Buffer.add_char buf c;
        aux ())
    in
    aux ();
    Buffer.contents buf
  in
  let magic = read_to '\n' in
  if magic <> "P6" then failwith "Only binary PPM (P6) supported";
  let width = int_of_string (read_to ' ') in
  let height = int_of_string (read_to '\n') in
  let max_color_value = int_of_string (read_to '\n') in
  let data =
    List.init height (fun _ -> List.init width (fun _ -> read_rgb ()))
  in
  close_in chan;
  { max_color_value; width; height; data }

let print (ppm : t) = to_string ppm `P3 |> print_string

let save (filename : string) (ppm : t) =
  let oc = open_out filename in
  to_string ppm `P3 |> output_string oc;
  close_out oc
