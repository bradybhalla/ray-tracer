type t = { origin: Vec3.t; dir: Vec3.t }

let normalize {origin; dir} = {origin; dir=Vec3.normalize dir}
let at time ray = Vec3.(ray.origin +@ (ray.dir *@ time))
