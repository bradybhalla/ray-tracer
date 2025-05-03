type t =
  | Diffuse of { tex : Texture.t; reflect_prob : float }
  | Refractive of { reflect_prob : float }
