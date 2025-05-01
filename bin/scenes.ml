open Ray_tracer
open Ray_tracer.Math
open Ray_tracer.Render

let image = Ppm.of_file "textures/earth.ppm" `P6 |> Ppm.to_texture
let image_material = Material.Diffuse { tex = image; reflect_prob = 0.0 }

let make_sphere x z mat r : Primitive.t =
  {
    shape = Sphere { pos = Vec3.create x (1.0 -. r) z; radius = r };
    material = mat;
    medium =
      (match mat with
      | Refractive _ -> { inside = 1.5; outside = 1.0 }
      | _ -> Medium.default_spec);
  }

let mc = Material.create

let spheres_scene pixel_height t =
  {
    camera = Camera.create ~pos:(Vec3.create 0.0 (-1.0) (-4.0)) ~pixel_height ();
    primitives =
      [
        make_sphere (-2.0) 0.0 (mc `Red) 1.0;
        make_sphere 2.0 0.0 (mc `Glass) 1.0;
        make_sphere 0.0 0.0 (mc (`Gradient (9, 22))) 1.0;
        {
          shape =
            Plane
              {
                normal = Vec3.create 0.0 (-1.0) 0.0;
                pos = Vec3.create 0.0 1.0 0.0;
                xdir =
                  Vec3.create
                    (cos (1.0 *. Float.pi *. t))
                    0.0
                    (sin (1.0 *. Float.pi *. t));
              };
          material = Material.create (`Checkerboard (2, 2));
          medium = Medium.default_spec;
        };
      ];
    lights =
      [ Point { pos = Vec3.create (-10.0) (-30.0) (-30.0); power = 2000.0 } ];
  }

let make_wall (pos : Vec3.t) mat : Primitive.t =
  {
    shape =
      Plane
        {
          normal = Vec3.normalize (pos *@ -1.0);
          pos;
          xdir = Vec3.create 1.0 0.0 0.0;
        };
    material = Material.create mat;
    medium = Medium.default_spec;
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
          medium = Medium.default_spec;
        };
        {
          shape = Sphere { pos = Vec3.create 1.0 1.5 1.0; radius = 0.5 };
          material = Material.create `Blue;
          medium = Medium.default_spec;
        };
        {
          shape = Sphere { pos = Vec3.create (-0.5) 1.0 2.0; radius = 1.0 };
          material = Material.create `Mirror;
          medium = Medium.default_spec;
        };
        {
          shape = Sphere { pos = Vec3.create 0.5 1.5 (-0.5); radius = 0.5 };
          material = Material.create `Glass;
          medium = { inside = v; outside = 1.0 };
        };
      ];
    lights = [ Point { pos = Vec3.create 0.0 (-1.8) 0.0; power = 20.0 } ];
  }

let space_scene pixel_height t =
  let t = t *. 2.0 *. Float.pi in
  {
    camera =
      Camera.create
        ~pos:(Vec3.create (-10.0 *. sin t) 0.0 (-10.0 *. cos t))
        ~pixel_height ();
    primitives =
      [
        {
          shape = Sphere { pos = Vec3.create 3.0 0.0 0.0; radius = 3.0 };
          material = image_material;
          medium = Medium.default_spec;
        };
        {
          shape = Sphere { pos = Vec3.create (-3.0) 0.0 1.0; radius = 2.0 };
          material = Material.create `Mirror;
          medium = Medium.default_spec;
        };
        {
          shape =
            Plane
              {
                normal = Vec3.create 0.0 (-1.0) 0.0;
                pos = Vec3.create 0.0 4.0 0.0;
                xdir = Vec3.create 1.0 0.0 0.0;
              };
          material = Material.create `Blue;
          medium = Medium.default_spec;
        };
      ];
    lights =
      [
        Point { pos = Vec3.create (-30.0) (-20.0) 0.0; power = 1000.0 };
        Point { pos = Vec3.create 30.0 (-20.0) 0.0; power = 1000.0 };
      ];
  }
