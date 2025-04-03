open Math

module type ImageProvider = sig
  type t
  val get_dim : t -> [`Col of int] * [`Row of int]
  val get_pixel_color : t -> [ `Col of int ] -> [ `Row of int ] -> Vec3.t
end

(*
  Make a module which can convert a render to a PPM image.
  The input module must be able to provide the dimensions
  of the image and the color of each pixel.
*)
module Make (Img : ImageProvider) : sig
  type t
  val of_render : Img.t -> t
  val print : t -> unit
end
