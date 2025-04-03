open Math

type rgb = int * int * int

module type ImageProvider = sig
  type t
  val get_dim : t -> [`Col of int] * [`Row of int]
  val get_pixel_color : t -> [ `Col of int ] -> [ `Row of int ] -> Vec3.t
end

module Make (Img : ImageProvider) = struct
  (* data in row major format *)
  type t = {
    width : int;
    height : int;
    max_color_value : int;
    data : rgb array array;
  }

  let default_max_val = 255

  let of_render (render : Img.t) =
    let `Col width, `Row height = Img.get_dim render in
    let data = Array.make_matrix height width (0, 0, 0) in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let color = Img.get_pixel_color render (`Col x) (`Row y) in
        let r = int_of_float (color.x *. float_of_int default_max_val) in
        let g = int_of_float (color.y *. float_of_int default_max_val) in
        let b = int_of_float (color.z *. float_of_int default_max_val) in
        data.(y).(x) <- (r, g, b)
      done
    done;
    { width; height; max_color_value = default_max_val; data }

  let print (img : t) =
    Printf.printf "P3\n%d %d\n%d\n" img.width img.height img.max_color_value;
    Array.iter
      (fun row ->
        Array.iter (fun (r, g, b) -> Printf.printf "%d %d %d " r g b) row;
        print_newline ())
      img.data
end
