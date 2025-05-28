open Utils
open Primitive
open Light

type t = {
  camera : Camera.t;
  primitives : Primitive.t list;
  finite_emitters : Light.t list;
  infinite_lights : Light.infinite_light list;
}

let create ~(camera : Camera.t) ~(primitives : Primitive.t list)
    ~(external_lights : Light.t list) =
  let light_of_prim p =
    match p.light with None -> None | Some l -> Some (Geometric l)
  in
  let finite_of_light l =
    match l with
    | Geometric _ -> Some l
    | Point _ -> Some l
    | Infinite _ -> None
  in
  let infinite_of_light l = match l with Infinite l -> Some l | _ -> None in
  {
    camera;
    primitives;
    finite_emitters = List.filter_map light_of_prim primitives @ List.filter_map finite_of_light external_lights;
    infinite_lights = List.filter_map infinite_of_light external_lights;
  }

let first_primitive_intersection (scene : t) (ray : Ray.t) :
    Primitive.intersection option =
  let cmp (i : Primitive.intersection option)
      (i' : Primitive.intersection option) =
    match (i, i') with
    | None, i' -> i'
    | i, None -> i
    | Some pi, Some pi' -> if pi.time < pi'.time then Some pi else Some pi'
  in
  let prim_ints =
    scene.primitives |> List.map (Primitive.get_intersection ray)
  in
  prim_ints |> List.fold_left cmp None
