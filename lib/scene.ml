open Utils

type t = {
  camera : Camera.t;
  primitives : Primitive.t list;
  lights : Light.t list;
}

type int_with_time = Prim of float * primitive_intersection | Light of float * light_intersection | NoInt

let prim_to_shared : (float * primitive_intersection) option -> int_with_time = function
  | None -> NoInt
  | Some (t, i) -> Prim (t, i)

let light_to_shared = function
  | None -> NoInt
  | Some (t, i) -> Light (t, i)

let get_time = function
  | Prim (t, _) | Light (t, _) -> t
  | NoInt ->
      raise
        (Invalid_argument "should not call this function with argument None")

let first_intersection (scene : t) (ray : Ray.t) : intersection =
  let cmp v v' =
    match (v, v') with
    | NoInt, v' -> v'
    | v, NoInt -> v
    | i, i' -> if get_time i < get_time i' then i else i'
  in
  let prim_ints =
    scene.primitives
    |> List.map (Primitive.get_intersection ray)
    |> List.map prim_to_shared
  in
  let light_ints =
    scene.lights
    |> List.map (Light.get_intersection ray)
    |> List.map light_to_shared
  in
  let res = List.fold_left cmp NoInt (prim_ints @ light_ints)
  in
  match res with
  | NoInt -> NoInt
  | Prim (_, v) -> PrimitiveInt v
  | Light (_, v) -> LightInt v

let first_primitive_intersection (scene : t) (ray : Ray.t) :
    primitive_intersection option =
  let cmp v v' =
    match (v, v') with
    | None, v' -> v'
    | v, None -> v
    | Some (t, i), Some (t', i') -> if t < t' then Some (t, i) else Some (t', i')
  in
  let prim_ints =
    scene.primitives |> List.map (Primitive.get_intersection ray)
  in
  prim_ints |> List.fold_left cmp None |> Option.map snd
