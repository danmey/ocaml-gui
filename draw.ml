(* open LablGL *)
open GL

module Pos = struct
  type t = (int*int)
  let fold2 f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)
  let sub = fold2 (-)
  let add = fold2 (+)
  let abs (x,y) = abs x, abs y
  let x = fst
  let y = snd
  let set_x p ax = (ax, y p)
  let set_y p ay = (x p, ay)
  let add_x p p' = (x p + x p', y p)
  let add_y p p' = (x p,y p + y p')
  let to_string (x,y) = Printf.sprintf "(%d, %d)" x y
end
module Rect = struct
  type t = {mutable x:int; mutable y:int; mutable w:int; mutable h:int}
  let rect (x,y) (w,h) = {x=x; y=y; w=w; h=h; }
  let size r = (r.w,r.h)
  let abs_dim r = (r.x, r.y),(r.x+r.w,r.y+r.h)
  let pos r = (r.x, r.y)
  let subr r1 r2 = 
    let p1 = pos r1 in 
    let p2 = pos r2 in 
    rect (Pos.sub p2 p1) (size r1)
  let set_pos r (x,y) = r.x <- x; r.y <- y
  let is_in r (x,y) = x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h
  let together r1 r2 =
    let x, y = max r1.x r2.x, max r1.y r2.y in
    let x',y' = min (r1.x + r1.w) (r2.x + r2.w), min (r1.y + r1.h) (r2.y + r2.h) in
    rect (x,y) (x'-x, y'-y)
  let pos_in r (x,y) = (x-r.x,y-r.y)
  let fold2 f r (x,y) = {r with x=f r.x x; y=f r.y y}
  let sub = fold2 (-)
  let by = fold2 (+)
  let by_h r (x,_) = { r with x=r.x+x}
  let by_v r (_,y) = { r with y=r.y+y}
  let o = rect (0,0) (0,0)
  let place_in r1 r2 = let p = pos r2 in by r1 p
  let border n r = rect (r.x + n / 2, r.y + n / 2) (r.w - n, r.h - n)
  let string_of_rect r = Printf.sprintf "Rect(%d,%d,%d,%d)" r.x r.y r.w r.h
  let lift4 f { x; y; w; h; } = f x, f y, f w, f h
  let lift22 f g  { x; y; w; h; } = f x, f y, g w,g h
  let coords { x; y; w; h } = x,y,w,h
  let coordsf { x; y; w; h } = float x, float y, float w, float h
end
module Draw = struct
  type t =
    | Line of (int * int * int * int) * (float * float * float * float)
    | Rect of Rect.t * (float * float * float * float)
    | Custom of (unit -> unit)

  let string_of_draw = function
    | Line ((x1,y1,x2,y2), _) -> Printf.sprintf "Line(%d,%d,%d,%d)" x1 y1 x2 y2
    | Rect (r,_) -> Rect.string_of_rect r
    | Custom _ -> ""

(* let triangulate lst = *)
(*   let els q = let lines,quads = ref 0,ref 0 in *)
(*     List.iter (function | Line _ -> lines := !lines + 1 | Rect _ -> quads := !quads+1 | _ -> ()) q; *)
(*     !lines,!quads in *)
(*   let vc,cc = let l,q = els lst in l*2*4 + q*4*2*4,((q*4)+l)*4*4 in *)
(*   let vertices = Bigarray.Array1.create_static float32 vc in *)
(*   let colors = Bigarray.Array1.create_static float32 cc in *)
(*   let of_dec x = float_of_int(x) +. 0.5 in *)
(*   let store_vtx i (x,y) (r,g,b,a) = *)
(*     Bigarray.Array1.set vertices ~pos:(i+0) x; *)
(*     Bigarray.Array1.set vertices ~pos:(i+1) y; *)
(*     Bigarray.Array1.set colors   ~pos:(2*i+0) r; *)
(*     Bigarray.Array1.set colors   ~pos:(2*i+1) g; *)
(*     Bigarray.Array1.set colors   ~pos:(2*i+2) b; *)
(*     Bigarray.Array1.set colors   ~pos:(2*i+3) a; *)
(*     i+2 in *)
(*   let store_line i (x1,y1) (x2,y2) color = *)
(*     let dx = float (x2-x1) in *)
(*     let dy = float (y2-y1) in *)
(*     let len dx dy = sqrt ((dx*.dx)+.(dy*.dy)) in *)
(*     let l = len dx dy in *)
(*     let vx = -0.5*.dx /. l in *)
(*     let vy =  0.5*.dy /. l in *)
(*     let c1 = of_dec(x1)-.vy, of_dec(y1)-.vx in *)
(*     let c2 = of_dec(x2)-.vy, of_dec(y2)-.vx in *)
(*     let c3 = of_dec(x2)+.vy, of_dec(y2)+.vx in *)
(*     let c4 = of_dec(x1)+.vy, of_dec(y1)+.vx in *)
(*     let i = store_vtx i c1 color in *)
(*     let i = store_vtx i c2 color in *)
(*     let i = store_vtx i c3 color in *)
(*     let i = store_vtx i c4 color in *)
(*       i in *)
(*   let store_rect i ((x1,y1),(x2,y2)) color = *)
(*     let i = store_line i (x1,y1) (x2,y1) color in *)
(*     let i = store_line i (x2,y1) (x2,y2) color in *)
(*     let i = store_line i (x2,y2) (x1,y2) color in *)
(*     let i = store_line i (x1,y2) (x1,y1) color in *)
(*       i *)
(*   in *)
(*   let i = ref 0 in *)
(*     List.iter (function *)
(* 		  | Rect (r,c) ->             i:= store_rect !i (Rect.abs_dim r) c *)
(* 		  | Line ((x1,y1,x2,y2),c) -> i:= store_line !i (x1,y1) (x2,y2) c *)
(* 		  | Custom _ -> () *)
(* 	       ) *)

(*       lst; *)

(*     vertices,colors *)

(* let draw el = *)
(*   let vertices,color = triangulate [el] in *)
(*     match el with Custom code -> code () | _ -> (); *)
(*     glDisableClientState GL_NORMAL_ARRAY; *)
(*     glDisableClientState Attrib.GL_TEXTURE_BIT_coord; *)
(*     glEnableClientState GL_VERTEX_ARRAY; *)
(*     glEnableClientState GL_COLOR_BUFFER_BIT; *)
(*     glVertexPointer `two vertices; *)
(*     glColorPointer `four color; *)
(*     glDrawArrays GL_QUADS ~first:0 ~count:((Raw.length vertices)/2); *)
(* end *)
end
open BatInt

let render_bitmap_string x y font string =
  glMatrixMode GL_PROJECTION;
  glPushMatrix ();
  glLoadIdentity ();
  let w,h = Display.display_size () in
  let right, top = float w, float h in
  Glu.gluOrtho2D ~left:0. ~right:right ~top:top ~bottom:0.;
  glMatrixMode GL_MODELVIEW;
  glPushMatrix ();
  glLoadIdentity ();
  glDisable GL_TEXTURE_2D;
  glRasterPos3 ~x ~y:(top-.y-.10.) ~z:0.;
  for i = 0 to String.length string - 1 do
    Glut.glutBitmapCharacter ~font ~c:(string.[i])
  done;
  glPopMatrix ();
  glMatrixMode GL_PROJECTION;
  glPopMatrix ();
  glMatrixMode GL_MODELVIEW
  
let draw_text x y text =
  render_bitmap_string (float x) (float y) Glut.GLUT_BITMAP_8_BY_13 text

let text_width str = 
  if not !Display.inited then
    (ignore( Glut.glutInit Sys.argv );
     Glut.glutInitDisplayMode [Glut.GLUT_RGBA; Glut.GLUT_DEPTH; Glut.GLUT_DOUBLE];
     ignore (Glut.glutCreateWindow ~title:"OpenGL Demo");
     Display.inited := true);

  Glut.glutBitmapLength ~font:Glut.GLUT_BITMAP_8_BY_13 ~str
