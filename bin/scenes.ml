open Ray_tracer
open Ray_tracer.Math
open Ray_tracer.Render

let make_sphere x z mat r : Primitive.t =
  {
    shape = Sphere { pos = Vec3.create x (1.0 -. r) z; radius = r };
    material = Material.create mat;
  }

let spheres_scene pixel_height =
  {
    camera = Camera.create ~pos:(Vec3.create 0.0 (-2.0) (-8.0)) ~pixel_height ();
    primitives =
      List.init 6 (fun i ->
          let mat = if i mod 2 = 0 then `Red else `Blue in
          make_sphere (float_of_int (5 - (2 * i))) 0.0 mat 1.0)
      @ List.init 6 (fun i ->
            make_sphere (float_of_int (5 - (2 * i))) (-2.0) (`Glass 2.5) 0.75)
      @ [
          Primitive.
            {
              shape =
                Sphere { pos = Vec3.create 0.0 (-4.0) 15.0; radius = 12.0 };
              material = Material.create `Mirror;
            };
          {
            shape =
              Plane
                {
                  normal = Vec3.create 0.0 (-1.0) 0.0;
                  pos = Vec3.create 0.0 1.0 0.0;
                };
            material = Material.create `White;
          };
        ];
    lights =
      [ Point { pos = Vec3.create (-10.0) (-30.0) (-30.0); power = 2000.0 } ];
  }

let make_wall (pos : Vec3.t) mat : Primitive.t =
  {
    shape = Plane { normal = Vec3.normalize (pos *@ -1.0); pos };
    material = Material.create mat;
  }

let room_scene pixel_height t =
  let v = 1.0 +. (2.0 *. exp (-6.0 *. t)) in
  {
    camera =
      Camera.create
        ~pos:(Vec3.create 0.0 1.0 (-3.0))
        ~look_at:(Vec3.create 0.0 1.0 4.0) ~pixel_height ();
    primitives =
      [
        make_wall (Vec3.create 3.0 0.0 0.0) `Mirror;
        make_wall (Vec3.create (-3.0) 0.0 0.0) `Mirror;
        make_wall (Vec3.create 0.0 2.0 0.0) `White;
        make_wall (Vec3.create 0.0 (-2.0) 0.0) `White;
        make_wall (Vec3.create 0.0 0.0 4.0) `White;
        make_wall (Vec3.create 0.0 0.0 (-6.0)) `White;
        {
          shape = Sphere { pos = Vec3.create (-1.3) 1.5 1.0; radius = 0.5 };
          material = Material.create `Red;
        };
        {
          shape = Sphere { pos = Vec3.create 1.0 1.5 1.0; radius = 0.5 };
          material = Material.create `Blue;
        };
        {
          shape = Sphere { pos = Vec3.create (-0.5) 1.0 2.0; radius = 1.0 };
          material = Material.create `Mirror;
        };
        {
          shape = Sphere { pos = Vec3.create 0.5 1.5 (-0.5); radius = 0.5 };
          material = Material.create (`Glass v);
        };
      ];
    lights = [ Point { pos = Vec3.create 0.0 (-1.8) 0.0; power = 20.0 } ];
  }
