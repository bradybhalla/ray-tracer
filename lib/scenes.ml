open Object
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
                pos = Vec3.create 0.0 0.5 0.0;
              };
          material = Material.create `Green;
        };
      ];
    lights = [ Point { pos = Vec3.create 0.0 (-50.0) 0.0; power = 2000.0 } ];
  }
