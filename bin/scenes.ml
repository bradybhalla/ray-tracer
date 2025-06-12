open Ray_tracer
open Ray_tracer.Math
open Ray_tracer.Transform
open Scene_utils

let sph_phi (w : Vec3.t) =
  let p = Float.atan2 w.y w.z in
  if p < 0.0 then p +. (2.0 *. Float.pi) else p

let sph_theta (w : Vec3.t) = acos w.z
let ax = 0.2
let ay = 0.2

let lambda (w : Vec3.t) =
  let theta = sph_theta w in
  let phi = sph_phi w in
  let tan2theta = Float.pow (Float.tan theta) 2.0 in
  let alpha2 = Float.pow (cos phi *. ax) 2.0 +. Float.pow (sin phi *. ay) 2.0 in
  (sqrt (1.0 +. (alpha2 *. tan2theta)) -. 1.0) /. 2.0

let d_single (wm : Vec3.t) =
  let tm = sph_theta wm in
  let pm = sph_phi wm in
  let denom =
    Float.pi *. ax *. ay
    *. Float.pow (cos tm) 4.0
    *. Float.pow
         (1.0
         +. tan tm *. tan tm
            *. ((cos pm *. cos pm /. ax /. ax) +. (sin pm *. sin pm /. ay /. ay))
         )
         2.0
  in
  1.0 /. denom

let materials pixel_height t =
  let t = t *. 2.0 *. Float.pi in
  let open Vec3 in
  let make_bsdf color : Material.bsdf =
   fun ~wo ~wi ->
    if wo.z < 0.0 || wi.z < 0.0 then Vec3.zero ()
    else
      let g wo wi = 1.0 /. (1.0 +. lambda wo +. lambda wi) in
      let wm = wi +@ wo |> Vec3.normalize in
      let fres =
        1.0
        (*fr_complex (Vec3.dot wo wm |> abs_float) eta k*)
      in
      let f = d_single wm *. fres *. g wo wi /. (4.0 *. wi.z *. wo.z) in
      color *@ f
  in
  Scene.create
    ~camera:
      (Camera.create
         {
           Camera.default_params with
           pos = Vec3.create (-10.0 *. sin t) (-1.0) (-10.0 *. cos t);
           look_at = Vec3.create 0.0 (-1.0) 0.0;
           fov = 0.7;
           pixel_height;
         })
    ~primitives:
      [
        (* sphere_on_y1 (-5.0) 0.0 (`Checkerboard (9, 22)) 2.0; *)
        sphere_on_y1 0.0 0.0 (`BSDF (make_bsdf (Vec3.create 1.0 1.0 1.0))) 2.0;
        sphere_on_y1 (-4.0) (-2.0)
          (`BSDF (make_bsdf (Vec3.create 0.7 0.3 0.3)))
          2.0;
        ground `White 1.0;
        triangle_light_at (Vec3.zero ()) 1.5 3.0
        % [ Transform.Rotation (Vec3.create 1.0 0.0 0.0, Float.pi /. 2.0);
            Transform.Rotation (Vec3.create 0.0 (-1.0) (-1.0), Float.pi /. 4.0);
            Transform.Translation (Vec3.create 4.5 (-2.0) 0.0); ];
        triangle_light_at (Vec3.create (-4.0) (-5.0) (-3.0)) 1.5 3.0;
      ]
    ~external_lights:[ Light.Infinite (Environment (Vec3.create 0.5 0.5 0.5)) ]

let lens pixel_height t =
  let t = 2.0 *. Float.pi *. t in
  let tr = [ Translation (Vec3.create 0.0 (-0.25 +. sin t) 0.0) ] in
  let sc = [ Scale (Vec3.create 2.0 0.3 2.0) ] in
  Scene.create
    ~camera:
      (Camera.create
         {
           Camera.default_params with
           pos = Vec3.create 0.0 (-2.0) (-6.0);
           pixel_height;
         })
    ~primitives:
      [
        sphere_on_y1 0.0 0.0 `Glass 1.0 % sc % tr;
        ground (`Checkerboard (2, 2)) 1.0;
        triangle_light_at (Vec3.create 1.5 (-2.0) 0.0) 3.0 20.0 % tr;
      ]
    ~external_lights:[ Infinite (Environment (Vec3.create 0.2 0.2 0.2)) ]

let three_spheres pixel_height t =
  let t = 2.0 *. Float.pi *. t in
  let tr1 =
    [
      Scale (Vec3.create (1.0 /. (1.0 +. (2.0 *. t))) 1.0 1.0);
      Translation (Vec3.create (-2.0) 0.0 0.0);
    ]
  in
  let tr2 = [ Rotation (Vec3.create 0.0 (-1.0) 0.0, t) ] in
  Scene.create
    ~camera:
      (Camera.create
         {
           Camera.default_params with
           pos = Vec3.create (-1.0) (-1.0) (-4.0);
           pixel_height;
         })
    ~primitives:
      [
        sphere_on_y1 0.0 0.0 `Mirror 1.0 % tr1;
        sphere_on_y1 2.0 0.0 `Glass 1.0;
        sphere_on_y1 0.0 0.0 (`Gradient (9, 22)) 1.0 % tr2;
        ground (`Checkerboard (2, 2)) 1.0;
        triangle_light_at (Vec3.create 0.0 (-10.0) 0.0) 6.0 7.0;
        triangle_light_at (Vec3.create 2.0 (-1.5) 0.0) 1.0 5.0;
      ]
    ~external_lights:[]

let room pixel_height _ =
  Scene.create
    ~camera:
      (Camera.create
         {
           Camera.default_params with
           pos = Vec3.create 0.0 1.0 (-4.0);
           pixel_height;
         })
    ~primitives:
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
        triangle_light_at (Vec3.create 0.0 (-1.0) 0.0) 1.0 10.0;
      ]
    ~external_lights:[]

let globe pixel_height t =
  let t = t *. 2.0 *. Float.pi in
  Scene.create
    ~camera:
      (Camera.create
         {
           Camera.default_params with
           pos = Vec3.create (-10.0 *. sin t) 0.0 (-10.0 *. cos t);
           pixel_height;
         })
    ~primitives:
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
          light = None;
          material = mc `Mirror;
          medium = Medium.default_spec;
        };
        ground `Blue 4.0;
      ]
    ~external_lights:
      [
        point_light_at (Vec3.create (-30.0) (-20.0) 0.0) 1000.0;
        point_light_at (Vec3.create 30.0 (-20.0) 0.0) 1000.0;
      ]

let onshape pixel_height =
  let triangles =
    Mesh.of_file "objects/test_part.obj" |> Mesh.to_shapes ~scale:100.0
  in
  fun t ->
    let t = t *. 2.0 *. Float.pi in
    let rot = Transform.Rotation (Vec3.create 0.0 (-1.0) 0.0, t) in
    Scene.create
      ~camera:
        (Camera.create
           {
             Camera.default_params with
             pos = Vec3.create 0.0 (-2.0) (-10.0);
             pixel_height;
           })
      ~primitives:
        (List.map
           (fun triangle ->
             Primitive.
               {
                 shape = triangle;
                 material = mc `Blue;
                 light = None;
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
          ])
      ~external_lights:
        [ point_light_at (Vec3.create (-18.0) (-10.0) (-20.0)) 500.0 ]
