let eps = 0.00001

let solve_quadratic a b c =
  let sgnb = if b < 0.0 then -1.0 else 1.0 in
  let discriminant = (b *. b) -. (4.0 *. a *. c) in
  if discriminant < 0.0 then None
  else
    let term = -.b -. (sgnb *. sqrt discriminant) in
    let t1 = term /. (2.0 *. a) in
    let t2 = 2.0 *. c /. term in
    let t1, t2 = (min t1 t2, max t1 t2) in
    Some (t1, t2)

let clamp ~minv ~maxv v = v |> min maxv |> max minv

let safe_clamp ~minv ~maxv v =
  let res = clamp ~minv ~maxv v in
  if abs_float (v -. res) > eps then
    failwith (Printf.sprintf "%f cannot be safely clamped to %f" v res)
  else res

let decimal v = v -. floor v

module Vec3 = struct
  type t = { x : float; y : float; z : float }

  let zero () = { x = 0.0; y = 0.0; z = 0.0 }
  let create x y z = { x; y; z }
  let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)
  let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z }
  let sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z }
  let mul v1 v2 = { x = v1.x *. v2.x; y = v1.y *. v2.y; z = v1.z *. v2.z }
  let cmul v k = { x = v.x *. k; y = v.y *. k; z = v.z *. k }
  let cdiv v k = { x = v.x /. k; y = v.y /. k; z = v.z /. k }
  let min m v = { x = min m v.x; y = min m v.y; z = min m v.z }
  let max m v = { x = max m v.x; y = max m v.y; z = max m v.z }
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
    if mag_sq perp > 1.0 then reflect v n (* Total Internal Reflection *)
    else
      let parallel =
        cmul n (-.sqrt (1.0 -. mag_sq perp |> safe_clamp ~minv:0.0 ~maxv:1.0))
      in
      add perp parallel

  let cross u v =
    {
      x = (u.y *. v.z) -. (u.z *. v.y);
      y = (u.z *. v.x) -. (u.x *. v.z);
      z = (u.x *. v.y) -. (u.y *. v.x);
    }

  let is_close v1 v2 =
    abs_float (v1.x -. v2.x) < eps
    && abs_float (v1.y -. v2.y) < eps
    && abs_float (v1.z -. v2.z) < eps
end

let ( +@ ) = Vec3.add
let ( -@ ) = Vec3.sub
let ( *@@ ) = Vec3.mul
let ( *@ ) = Vec3.cmul
let ( /@ ) = Vec3.cdiv

module Sample = struct
  (* TODO: not correct sampling *)
  let unit_vec3 () =
    let v =
      Vec3.create (Random.float 1.0) (Random.float 1.0) (Random.float 1.0)
    in
    let v = v -@ Vec3.create 0.5 0.5 0.5 in
    Vec3.normalize v

  let rec hemisphere_unit_vec3 normal =
    let v = unit_vec3 () in
    if Vec3.dot v normal >= 0.0 then v else hemisphere_unit_vec3 normal

  let float () = Random.float 1.0
end
