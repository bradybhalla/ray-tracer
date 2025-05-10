let vec3_creates = ref 0

module Vec3 = struct
  type xyz = { x : float; y : float; z : float }
  type t = { mutable x : float; mutable y : float; mutable z : float }

  let create x y z =
    (* vec3_creates := !vec3_creates + 1; *)
    { x; y; z }

  let copy v = create v.x v.y v.z
  let creater ({ x; y; z } : xyz) : t = create x y z
  let x (v : t) : float = v.x
  let y (v : t) : float = v.y
  let z (v : t) : float = v.z
  let zero () = create 0.0 0.0 0.0
  let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)
  let add' v1 v2 = create (v1.x +. v2.x) (v1.y +. v2.y) (v1.z +. v2.z)
  let sub' v1 v2 = create (v1.x -. v2.x) (v1.y -. v2.y) (v1.z -. v2.z)
  let cmul' v k = create (v.x *. k) (v.y *. k) (v.z *. k)
  let cdiv' v k = create (v.x /. k) (v.y /. k) (v.z /. k)
  let min' m v = create (min m v.x) (min m v.y) (min m v.z)
  let max' m v = create (max m v.x) (max m v.y) (max m v.z)

  let mul v1 v2 =
    v2.x <- v2.x *. v1.x;
    v2.y <- v2.y *. v1.y;
    v2.z <- v2.z *. v1.z;
    v2

  let add v1 v2 =
    v2.x <- v2.x +. v1.x;
    v2.y <- v2.y +. v1.y;
    v2.z <- v2.z +. v1.z;
    v2

  let cadd k v2 =
    v2.x <- v2.x +. k;
    v2.y <- v2.y +. k;
    v2.z <- v2.z +. k;
    v2

  let mul_add v1 k v2 =
    v2.x <- v2.x +. (v1.x *. k);
    v2.y <- v2.y +. (v1.y *. k);
    v2.z <- v2.z +. (v1.z *. k);
    v2

  let sub v1 v2 =
    v2.x <- v2.x -. v1.x;
    v2.y <- v2.y -. v1.y;
    v2.z <- v2.z -. v1.z;
    v2

  let cmul k v =
    v.x <- v.x *. k;
    v.y <- v.y *. k;
    v.z <- v.z *. k;
    v

  let cdiv k v =
    v.x <- v.x /. k;
    v.y <- v.y /. k;
    v.z <- v.z /. k;
    v

  let min m v =
    v.x <- min m v.x;
    v.y <- min m v.y;
    v.z <- min m v.z;
    v

  let max m v =
    v.x <- max m v.x;
    v.y <- max m v.y;
    v.z <- max m v.z;
    v

  let mag v = sqrt (dot v v)
  let mag_sq v = dot v v

  let normalize' v =
    let len = sqrt (dot v v) in
    if len = 0.0 then v else cdiv' v len

  let normalize v =
    let len = sqrt (dot v v) in
    if len = 0.0 then v else cdiv len v

  let reflect' v n =
    let dot_product = dot v n in
    let scaled_normal = cmul' n (2.0 *. dot_product) in
    sub' v scaled_normal

  let refract' v n eta_i_div_eta_t =
    let cos_t = -.dot v n in
    let perp = cmul' (add' v (cmul' n cos_t)) eta_i_div_eta_t in
    let parallel = cmul' n (-.sqrt (1.0 -. mag_sq perp)) in
    add perp parallel

  let cross' u v =
    create
      ((u.y *. v.z) -. (u.z *. v.y))
      ((u.z *. v.x) -. (u.x *. v.z))
      ((u.x *. v.y) -. (u.y *. v.x))

  let cross u v =
    create
      ((v.y *. u.z) -. (v.z *. u.y))
      ((v.z *. u.x) -. (v.x *. u.z))
      ((v.x *. u.y) -. (v.y *. u.x))

  let is_close v1 v2 =
    let eps = 0.0001 in
    abs_float (v1.x -. v2.x) < eps
    && abs_float (v1.y -. v2.y) < eps
    && abs_float (v1.z -. v2.z) < eps
end

let ( +@ ) = Vec3.add'
let ( -@ ) = Vec3.sub'
let ( *@ ) = Vec3.cmul'
let ( /@ ) = Vec3.cdiv'

module Sample = struct
  (* TODO: not correct sampling *)
  let unit_vec3 () =
    Vec3.create (Random.float 1.0) (Random.float 1.0) (Random.float 1.0)
    |> Vec3.cadd (-0.5) |> Vec3.normalize

  let float () = Random.float 1.0
end

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

let clamp minv maxv v = v |> min maxv |> max minv
let decimal v = v -. floor v
