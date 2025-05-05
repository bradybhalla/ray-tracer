open Math

type face_vertex = { vi : int; ti : int option; ni : int option }
type v = Vec3.t
type vt = Texture.tex_coord
type vn = Vec3.t
type f = face_vertex * face_vertex * face_vertex

type t = {
  vertices : v array;
  tex_coords : vt array;
  vertex_normals : vn array;
  faces : f list;
}

let ios = int_of_string
let fos = float_of_string

let of_file (filename : string) =
  let get_face_vert (s : string) : face_vertex =
    match String.split_on_char '/' s with
    | [ vi ] -> { vi = ios vi; ti = None; ni = None }
    | [ vi; ti ] -> { vi = ios vi; ti = Some (ios ti); ni = None }
    | [ vi; ""; ni ] -> { vi = ios vi; ti = None; ni = Some (ios ni) }
    | [ vi; ti; ni ] -> { vi = ios vi; ti = Some (ios ti); ni = Some (ios ni) }
    | ss -> failwith ("unexcepted face vertex: " ^ String.concat "/" ss)
  in
  let process_line (vs, ts, ns, fs) (s : string) =
    let s = String.trim s in
    match String.split_on_char ' ' s with
    | [ "v"; x; y; z ] ->
        let vertex = Vec3.create (fos x) (fos y) (fos z) in
        (vertex :: vs, ts, ns, fs)
    | [ "vt"; u; v ] ->
        let tex = Texture.{ u = fos u; v = fos v } in
        (vs, tex :: ts, ns, fs)
    | [ "vn"; x; y; z ] ->
        let normal = Vec3.create (fos x) (fos y) (fos z) in
        (vs, ts, normal :: ns, fs)
    | [ "f"; p1; p2; p3 ] ->
        let face = (get_face_vert p1, get_face_vert p2, get_face_vert p3) in
        (vs, ts, ns, face :: fs)
    | s :: _ when String.get s 0 = '#' -> (vs, ts, ns, fs)
    | ss ->
        Printf.eprintf "warning: unknown line in obj file \"%s\"\n"
          (String.concat " " ss);
        (vs, ts, ns, fs)
  in
  let chan = open_in filename in
  let lines = In_channel.input_lines chan in
  let vs, ts, ns, fs = List.fold_left process_line ([], [], [], []) lines in
  {
    vertices = Array.of_list (List.rev vs);
    tex_coords = Array.of_list (List.rev ts);
    vertex_normals = Array.of_list (List.rev ns);
    faces = fs;
  }

let to_shapes (mesh : t) : Shape.t list =
  let face_to_triangle ((f0, f1, f2) : f) =
    Shape.create (TriangleParams
      {
        p0 = mesh.vertices.(f0.vi - 1) *@ 100.0; (* TODO: my onshape files are in meters *)
        p1 = mesh.vertices.(f1.vi - 1) *@ 100.0;
        p2 = mesh.vertices.(f2.vi - 1) *@ 100.0;
      })
  in
  List.map face_to_triangle mesh.faces
