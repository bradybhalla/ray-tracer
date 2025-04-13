open Math
open Render

let two_spheres_scene pixel_height =
  {
    camera = Camera.create ~pos:(Vec3.create 0.0 (-1.0) (-5.0)) ~pixel_height ();
    primitives =
      [
        {
          shape = Sphere { pos = Vec3.create 2.0 0.0 0.0; radius = 1.0 };
          material = Material.create `Red;
        };
        {
          shape = Sphere { pos = Vec3.create 0.0 0.0 0.0; radius = 1.0 };
          material = Material.create `Mirror;
        };
        {
          shape = Sphere { pos = Vec3.create (-2.0) 0.0 0.0; radius = 1.0 };
          material = Material.create `Blue;
        };
        {
          shape =
            Plane
              {
                normal = Vec3.create (-1.0) 0.0 (-1.0) |> Vec3.normalize;
                pos = Vec3.create 10.0 0.0 0.0;
              };
          material = Material.create `Mirror;
        };
        {
          shape =
            Plane
              {
                normal = Vec3.create 0.0 (-1.0) 0.0;
                pos = Vec3.create 0.0 1.5 0.0;
              };
          material = Material.create `Green;
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

let box_room_scene pixel_height t =
  {
    camera = Camera.create ~pos:(Vec3.create 0.0 1.0 (-5.0)) ~look_at:(Vec3.create 0.0 1.0 4.0) ~pixel_height ();
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
          shape = Sphere { pos = Vec3.create (0.5) 1.5 ((-2.0) *. sin t *. sin t); radius = 0.5 };
          material = Material.create `Glass;
        };
      ];
    lights = [ Point { pos = Vec3.create 0.0 (-1.8) 0.0; power = 20.0 } ];
  }
