open Utils

type t = {
  shape : Shape.t;
  material : Material.t;
  medium_transition : Medium.transition;
}

let get_intersection (prim : t) ((t, si) : float * shape_intersection) :
    (float * intersection) option =
  Some
    ( t,
      {
        si;
        material = prim.material;
        medium_incident =
          Medium.get_incident prim.medium_transition si.medium_transition_dir;
        medium_transmitted =
          Medium.get_transmitted prim.medium_transition si.medium_transition_dir;
      } )

let cmp v v' =
  match (v, v') with
  | None, v' -> v'
  | v, None -> v
  | Some (t, i), Some (t', i') -> if t < t' then Some (t, i) else Some (t', i')

(* TODO: this should be in an aggregate class *)
let get_first_intersection (prims : t list) (ray : Ray.t) =
  prims
  |> List.map (fun p -> Shape.intersect p.shape ray >>= get_intersection p)
  |> List.fold_left cmp None
  |> ( =<< ) (lift snd)
