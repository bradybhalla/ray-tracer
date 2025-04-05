module Vec3 = struct
  type t = { x : float; y : float; z : float }

  let create x y z = { x; y; z }
  let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)
  let cwise f v = { x = f v.x; y = f v.y; z = f v.z }
  let bin f u v = { x = f u.x v.x; y = f u.y v.y; z = f u.z v.z }
  let add = bin ( +. )
  let sub = bin ( -. )
  let cmul v k = cwise (fun x -> x *. k) v
  let cdiv v k = cwise (fun x -> x /. k) v
  let cmin v m = cwise (min m) v
  let cmax v m = cwise (max m) v

  let normalize v =
    let len = sqrt (dot v v) in
    if len = 0.0 then v else cdiv v len
end

let ( +@ ) = Vec3.add
let ( -@ ) = Vec3.sub
let ( *@ ) = Vec3.cmul
let ( /@ ) = Vec3.cdiv

module Ray = struct
  type t = { origin : Vec3.t; dir : Vec3.t }

  let normalize ray = { ray with dir = Vec3.normalize ray.dir }
  let at ray time = ray.origin +@ (ray.dir *@ time)
end

module Sample = struct
  (* TODO: not correct sampling *)
  let unit_vec3 () =
    let v =
      Vec3.create (Random.float 1.0) (Random.float 1.0) (Random.float 1.0)
    in
    let v = v -@ Vec3.create 0.5 0.5 0.5 in
    Vec3.normalize v
end
