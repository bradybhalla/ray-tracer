open Object
open Math
open Render

let two_spheres_scene pixel_height =
  {
    camera = Camera.create ~pixel_height ();
    primitives =
      [
        {
          shape = Shape.Sphere { pos = Vec3.create 0.0 0.0 10.0; radius = 1.0 };
          material = { color = Vec3.create 1.0 0.0 0.0 };
        };
        {
          shape =
            Shape.Sphere { pos = Vec3.create (-2.0) 0.0 10.0; radius = 1.0 };
          material = { color = Vec3.create 1.0 0.0 0.0 };
        };
      ];
  }
