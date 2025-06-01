open Utils
open Primitive
open Light
open Math

type t = {
  camera : Camera.t;
  primitives : Primitive.t list;
  finite_lights : Light.t list;
  infinite_lights : Light.infinite_light list;
  all_lights : Light.t list;
}

let create ~(camera : Camera.t) ~(primitives : Primitive.t list)
    ~(external_lights : Light.t list) =
  let light_of_prim p =
    match p.light with None -> None | Some l -> Some (Geometric l)
  in
  let finite_of_light l =
    match l with Geometric _ | Point _ -> Some l | Infinite _ -> None
  in
  let infinite_of_light l = match l with Infinite l -> Some l | _ -> None in
  let all_lights = List.filter_map light_of_prim primitives @ external_lights in
  {
    camera;
    primitives;
    all_lights;
    finite_lights = List.filter_map finite_of_light all_lights;
    infinite_lights = List.filter_map infinite_of_light all_lights;
  }

let first_primitive_intersection ~(scene : t) ~(ray : Ray.t) :
    Primitive.intersection option =
  let cmp (i : Primitive.intersection option)
      (i' : Primitive.intersection option) =
    match (i, i') with
    | None, i' -> i'
    | i, None -> i
    | Some pi, Some pi' -> if pi.time < pi'.time then Some pi else Some pi'
  in
  let prim_ints =
    scene.primitives
    |> List.map (fun p -> Primitive.get_intersection ~ray ~primitive:p)
  in
  prim_ints |> List.fold_left cmp None

let is_shadowed ~(scene : t) ~(int : Primitive.intersection)
    ~(sample : light_sample) =
  let shadow_ray =
    Ray.create ~origin:(int.si.point +@ (sample.wi *@ eps)) ~dir:sample.wi
  in
  let shadow_int = first_primitive_intersection ~scene ~ray:shadow_ray in
  match shadow_int with
  | None -> false
  | Some { si; _ } ->
      let dist = Vec3.mag (si.point -@ int.si.point) in
      dist +. (2.0 *. eps)
      < sample.light_dist (* this epsilon should be larger than the one above *)
