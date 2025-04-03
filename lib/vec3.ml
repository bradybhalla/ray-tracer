type t = { x : float; y : float; z : float }

let create x y z = { x; y; z }

let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)

let cwise f v = {x = f v.x; y = f v.y; z = f v.z}
let bin f u v = {x = f u.x v.x; y = f u.y v.y; z = f u.z v.z}

let ( +@ ) = bin ( +. )
let ( -@ ) = bin ( -. )
let ( *@ ) v k = cwise (fun x-> x *. k) v
let ( /@ ) v k = cwise (fun x -> x /. k) v

let normalize v =
  let len = sqrt (dot v v) in
  if len = 0.0 then v else (v /@ len)
