module Vec3 = struct
  type t = { x : float; y : float; z : float }

  let zero = { x = 0.0; y = 0.0; z = 0.0 }
  let create x y z = { x; y; z }
  let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)
  let cwise f v = { x = f v.x; y = f v.y; z = f v.z }
  let bin f u v = { x = f u.x v.x; y = f u.y v.y; z = f u.z v.z }
  let add = bin ( +. )
  let sub = bin ( -. )
  let cmul v k = cwise (fun x -> x *. k) v
  let cdiv v k = cwise (fun x -> x /. k) v
  let min m = cwise (min m)
  let max m = cwise (max m)
  let mag v = sqrt (dot v v)
  let mag_sq v = dot v v

  let normalize v =
    let len = sqrt (dot v v) in
    if len = 0.0 then v else cdiv v len

  let reflect v n =
    let dot_product = dot v n in
    let scaled_normal = cmul n (2.0 *. dot_product) in
    sub v scaled_normal

  let refract v n eta_i_div_eta_t =
    let cos_t = -.dot v n in
    let perp = cmul (add v (cmul n cos_t)) eta_i_div_eta_t in
    let parallel = cmul n (-.sqrt (1.0 -. mag_sq perp)) in
    add perp parallel

  let cross u v =
    {
      x = (u.y *. v.z) -. (u.z *. v.y);
      y = (u.z *. v.x) -. (u.x *. v.z);
      z = (u.x *. v.y) -. (u.y *. v.x);
    }

  let is_close v1 v2 =
    let eps = 0.0001 in
    abs_float (v1.x -. v2.x) < eps
    && abs_float (v1.y -. v2.y) < eps
    && abs_float (v1.z -. v2.z) < eps
end

let ( +@ ) = Vec3.add
let ( -@ ) = Vec3.sub
let ( *@ ) = Vec3.cmul
let ( /@ ) = Vec3.cdiv
