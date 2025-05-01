type t =
  | Diffuse of { tex : Texture.t; reflect_prob : float }
  | Refractive of { reflect_prob : float }
(** TODO: we are exposing the type (for now) because some are created elsewhere *)

val create : [ `Red 
             | `Green 
             | `Blue 
             | `White 
             | `Black 
             | `Checkerboard of int * int 
             | `Gradient of int * int 
             | `Mirror 
             | `Glass ] -> t
(** Create a few common materials *)
