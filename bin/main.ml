open Ray_tracer

let () =
  let v1 = Math.Vec3.create 1.0 2.0 3.0 in
  let v2 = Math.Vec3.create 4.0 5.0 6.0 in
  let result = Math.Vec3.add v1 v2 in
  Printf.printf "Result: x=%f, y=%f, z=%f\n" result.x result.y result.z;
  Greet.greet ()
