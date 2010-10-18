
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
end
module Rect = struct
  type t = {mutable x:int; mutable y:int; mutable w:int; mutable h:int}
  let rect (x,y) (w,h) = {x=x; y=y; w=w; h=h; }
  let size r = (r.w,r.h)
  let abs_dim r = (r.x, r.y),(r.x+r.w,r.y+r.h)
  let pos r = (r.x, r.y)
  let is_in r (x,y) = x >= r.x && x < r.x + r.w && y >= r.y && y < r.y + r.h
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

let triangulate lst =
  let els q = let lines,quads = ref 0,ref 0 in
    List.iter (function | Line _ -> lines := !lines + 1 | Rect _ -> quads := !quads+1 | _ -> ()) q;
    !lines,!quads in
  let vc,cc = let l,q = els lst in l*2*4 + q*4*2*4,((q*4)+l)*4*4 in
  let vertices = Raw.create_static `float vc in
  let colors = Raw.create_static `float cc in
  let of_dec x = float_of_int(x) +. 0.5 in
  let store_vtx i (x,y) (r,g,b,a) =
    Raw.set_float vertices ~pos:(i+0) x;
    Raw.set_float vertices ~pos:(i+1) y;
    Raw.set_float colors   ~pos:(2*i+0) r;
    Raw.set_float colors   ~pos:(2*i+1) g;
    Raw.set_float colors   ~pos:(2*i+2) b;
    Raw.set_float colors   ~pos:(2*i+3) a;
    i+2 in
  let store_line i (x1,y1) (x2,y2) color =
    let dx = float (x2-x1) in
    let dy = float (y2-y1) in
    let len dx dy = sqrt ((dx*.dx)+.(dy*.dy)) in
    let l = len dx dy in
    let vx = -0.5*.dx /. l in
    let vy =  0.5*.dy /. l in
    let c1 = of_dec(x1)-.vy, of_dec(y1)-.vx in
    let c2 = of_dec(x2)-.vy, of_dec(y2)-.vx in
    let c3 = of_dec(x2)+.vy, of_dec(y2)+.vx in
    let c4 = of_dec(x1)+.vy, of_dec(y1)+.vx in
    let i = store_vtx i c1 color in
    let i = store_vtx i c2 color in
    let i = store_vtx i c3 color in
    let i = store_vtx i c4 color in
      i in
  let store_rect i ((x1,y1),(x2,y2)) color =
    let i = store_line i (x1,y1) (x2,y1) color in
    let i = store_line i (x2,y1) (x2,y2) color in
    let i = store_line i (x2,y2) (x1,y2) color in
    let i = store_line i (x1,y2) (x1,y1) color in
      i
  in
  let i = ref 0 in
    List.iter (function
		  | Rect (r,c) ->             i:= store_rect !i (Rect.abs_dim r) c
		  | Line ((x1,y1,x2,y2),c) -> i:= store_line !i (x1,y1) (x2,y2) c
		  | Custom _ -> ()
	       )

      lst;

    vertices,colors

let draw el =
  let vertices,color = triangulate [el] in
    match el with Custom code -> code () | _ -> ();
    GlArray.disable `normal;
    GlArray.disable `texture_coord;
    GlArray.enable `vertex;
    GlArray.enable `color;
    GlArray.vertex `two vertices;
    GlArray.color `four color;
    GlArray.draw_arrays `quads ~first:0 ~count:((Raw.length vertices)/2);
end
;;
