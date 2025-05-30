open Ray_tracer
open Ray_tracer.Math
open Ray_tracer.Render
open Ray_tracer.Transform
open Scene_utils

let spheres_scene pixel_height t =
  let t = 2.0 *. Float.pi *. t in
  let tr1 =
    [
      Scale (Vec3.create (1.0 /. (1.0 +. (2.0 *. t))) 1.0 1.0);
      Translation (Vec3.create (-2.0) 0.0 0.0);
    ]
  in
  let tr2 = [ Rotation (Vec3.create 0.0 (-1.0) 0.0, t) ] in
  {
    camera = Camera.create ~pos:(Vec3.create 1.0 (-1.0) (-4.0)) ~pixel_height ();
    primitives =
      [
        sphere_on_y1 0.0 0.0 `Mirror 1.0 % tr1;
        sphere_on_y1 2.0 0.0 `Glass 1.0;
        sphere_on_y1 0.0 0.0 (`Gradient (9, 22)) 1.0 % tr2;
        ground (`Checkerboard (2, 2)) 1.0;
      ];
    lights = [ light_at (Vec3.create (-10.0) (-30.0) (-30.0)) 2000.0 ];
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
        wall_facing_origin (Vec3.create 3.0 0.0 0.0) (`Checkerboard (2, 2));
        wall_facing_origin (Vec3.create (-3.0) 0.0 0.0) (`Checkerboard (2, 2));
        wall_facing_origin (Vec3.create 0.0 2.0 0.0) (`Checkerboard (2, 2));
        wall_facing_origin (Vec3.create 0.0 (-2.0) 0.0) (`Checkerboard (2, 2));
        wall_facing_origin (Vec3.create 0.0 0.0 4.0) (`Checkerboard (2, 2));
        wall_facing_origin (Vec3.create 0.0 0.0 (-6.0)) `White;
        sphere_at (Vec3.create (-1.3) 1.5 1.0) 0.5 `Red;
        sphere_at (Vec3.create 1.0 1.5 1.0) 0.5 `Blue;
        sphere_at (Vec3.create (-0.5) 1.0 2.0) 1.0 `Mirror;
        sphere_at (Vec3.create 0.5 1.5 (-0.5)) 0.5 `Glass;
      ];
    lights = [ light_at (Vec3.create 0.0 (-1.8) 0.0) 20.0 ];
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
        sphere_at (Vec3.create 3.0 0.0 0.0) 3.0 `Earth;
        {
          shape =
            Shape.create
              (TriangleParams
                 {
                   p0 = Vec3.create (-5.0) (-5.0) (-5.0);
                   p1 = Vec3.create (-5.0) 5.0 (-5.0);
                   p2 = Vec3.create (-5.0) (-5.0) 5.0;
                 });
          material = mc `Mirror;
          medium = Medium.default_spec;
        };
        ground `Blue 4.0;
      ];
    lights =
      [
        light_at (Vec3.create (-30.0) (-20.0) 0.0) 1000.0;
        light_at (Vec3.create 30.0 (-20.0) 0.0) 1000.0;
      ];
  }

let obj_scene pixel_height =
  let triangles = Mesh.of_file "objects/test_part.obj" |> Mesh.to_shapes in
  fun t ->
    let t = t *. 2.0 *. Float.pi in
    let rot = Transform.Rotation (Vec3.create 0.0 (-1.0) 0.0, t) in
    {
      camera =
        Camera.create ~pos:(Vec3.create 0.0 (-2.0) (-10.0)) ~pixel_height ();
      primitives =
        List.map
          (fun triangle ->
            Primitive.
              {
                shape = triangle;
                material = mc `Blue;
                medium = Medium.default_spec;
              }
            % [ rot ])
          triangles
        @ [
            ground (`Checkerboard (2, 2)) 0.0
            % [
                Scale (Vec3.create 0.1 0.1 0.1);
                Translation (Vec3.create 0.0 7.0 0.0);
              ];
          ];
      lights = [ light_at (Vec3.create (-18.0) (-10.0) (-20.0)) 500.0 ];
    }
