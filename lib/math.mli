val vec3_creates : int ref

module Vec3 : sig
  type t
  type xyz = { x : float; y : float; z : float }

  val copy : t -> t
  val creater : xyz -> t
  val x : t -> float
  val y : t -> float
  val z : t -> float
  val zero : unit -> t
  val create : float -> float -> float -> t
  val dot : t -> t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
  val cadd : float -> t -> t
  val mul_add : t -> float -> t -> t
  val sub : t -> t -> t
  val cmul : float -> t -> t
  val cdiv : float -> t -> t
  val min : float -> t -> t
  val max : float -> t -> t
  val add' : t -> t -> t
  val sub' : t -> t -> t
  val cmul' : t -> float -> t
  val cdiv' : t -> float -> t
  val min' : float -> t -> t
  val max' : float -> t -> t
  val mag : t -> float
  val mag_sq : t -> float
  val normalize : t -> t
  val normalize' : t -> t
  val reflect' : t -> t -> t
  val refract' : t -> t -> float -> t
  val cross' : t -> t -> t
  val cross : t -> t -> t
  val is_close : t -> t -> bool
end

val ( +@ ) : Vec3.t -> Vec3.t -> Vec3.t
val ( -@ ) : Vec3.t -> Vec3.t -> Vec3.t
val ( *@ ) : Vec3.t -> float -> Vec3.t
val ( /@ ) : Vec3.t -> float -> Vec3.t

module Sample : sig
  val unit_vec3 : unit -> Vec3.t
  val float : unit -> float
end

val solve_quadratic : float -> float -> float -> (float * float) option
val clamp : float -> float -> float -> float
val decimal : float -> float
