open Math

type tex_coord = { u : float; v : float }

module ColorTexture = struct
  type t = Constant of Vec3.t | Checkered of Vec3.t * Vec3.t

  let eval tex u v =
    match tex with
    | Constant c -> c
    | Checkered (c1, c2) ->
        if (int_of_float (floor u) + int_of_float (floor v)) mod 2 = 0 then c1
        else c2
end
