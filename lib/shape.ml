open Math

module Shape = struct
  type t =
    | Sphere of { pos : Vec3.t; radius : float }
    | Plane of { normal : Vec3.t; pos : Vec3.t }
end
