open Math

type bsdf = Vec3.t -> Vec3.t -> Vec3.t

type t = Diffuse of { tex : Texture.t } | Glass | Mirror | BSDF of bsdf
