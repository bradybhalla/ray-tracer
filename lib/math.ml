module Vec3 = struct
  type t = { x : float; y : float; z : float }

  let create x y z = { x; y; z }
  let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }
end
