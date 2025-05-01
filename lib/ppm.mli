type t

val to_string : t -> [ `P3 ] -> string
val to_texture : t -> Texture.t
val of_render : Render.t -> t
val of_file : string -> [ `P6 ] -> t
val print : t -> unit
val save : string -> t -> unit
