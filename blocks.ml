open Widget
open Draw
open Window

type 'a block_tree = Block of 'a * 'a list
type stickiness = TopBottom | LeftRight

let expand_rect_left dx rect constr =
  let real_dx = ( dx / constr) * constr in
  { rect with 
    Rect.x = rect.Rect.x + real_dx; 
    Rect.w = rect.Rect.w - real_dx }

let block_cmp l r =
  let y = r.Rect.y - l.Rect.y in
  if y <> 0 then y
  else
    r.Rect.x - l.Rect.x

(* class block_canvas = object ( self : 'self ) *)
(*   inherit [ block ] composite as composite *)
(*   inherit graphical as super *)
(*   method add block =  *)
(*     composite#add block; *)
(*     block#set_canvas (self :> block_canvas) *)

    
(* end and block name = object ( self : 'self ) *)
class block name = object ( self : 'self ) 
  inherit canvas as canvas
  inherit draggable as super
  val left_border = new draggable_constrained (HorizontalWith 10)
  val right_border = new draggable_constrained (HorizontalWith 10)
  val name = name
  initializer
    canvas#add left_border;
    canvas#add right_border
    
  method drag _ (x,y) (dx,dy) =
    let grid x = (x + 5) / 10 * 10 in
    window.pos.Rect.x <- grid x;
    window.pos.Rect.y <- grid y

  method value = ""
  method name = name
  method paint state =
    let caption = Printf.sprintf "%s: %s" name self#value in
    caption_painter caption 0 state

  method invalidate rect =
    super#invalidate rect;
    left_border#invalidate (Rect.rect (0,0) (10, rect.Rect.h));
    right_border#invalidate (Rect.rect (rect.Rect.w-10,0) (10, rect.Rect.h))

  method dragged widget (dx, dy) =
    let grid x = (x + 5) / 10 * 10 in
    let rect = self#window.pos in
    if widget == left_border then
      self # invalidate (expand_rect_left dx rect 10)
    else if widget == right_border then
      self # invalidate
        (Rect.rect
           (rect.Rect.x, rect.Rect.y)
           (grid (rect.Rect.w + dx), rect.Rect.h))
end

class block_canvas = object ( self : 'self)
  inherit canvas
  inherit fixed as super
  val mutable last_mouse_pos = (0,0)
  method mouse_down button pos =
    match button with
      | Event.Right ->
        last_mouse_pos <- pos;
        let m = new menu pos  ["perlin"; "add"; "clamp";] in
        self # add (m :> draggable);
        true
      | Event.Middle ->
        self # layout; true
      | _ -> super # mouse_down button pos

  method layout =
    let rects = List.map (fun w -> w # window.pos) widgets in
    let widget_rects = List.combine rects widgets in
    let sorted = BatList.sort ~cmp:block_cmp rects in
    let rec stack_loop acc cur_y = function
      | x :: xs when x.Rect.y = cur_y ->
        (match acc with
          | [] -> stack_loop ([x] :: acc) cur_y xs
          | a :: b -> stack_loop ((a @ [x]) :: b) cur_y xs)
      | x :: xs -> stack_loop ([x] :: acc) x.Rect.y xs
      | [] -> acc in
    
    let stack = stack_loop [[]] ((List.hd sorted).Rect.y) sorted in
    
    let rec loop = function
      | x :: xs -> String.concat "\t" (List.map (fun rect -> (List.assoc rect widget_rects)#name) x) :: (loop xs)
      | [] -> []
    in
    print_endline (String.concat "\n" (loop stack));
    print_endline ""

    method event wind = 
      function
      | Event.Custom ("menu_item", _, what) -> 
          let b = (new block what) in
          self#add (b :> draggable);
          b#invalidate (Rect.rect last_mouse_pos (80, 20));
        true
      | ev -> super # event wind ev
end

open BatFloat
class texture_preview trigger = object ( self : 'self )
  inherit graphics as super
  val mutable texid = None
  initializer
    window.Window.painter <- self#draw;
    texid <- Some (Texture.Tga.gl_maketex (Perlin.Op.array_of_texture (Perlin.Op.normalize 256 (Perlin.Op.clouds 3 0.4))))

    method draw rect =
      BatOption.may (fun texid ->
        GlTex.bind_texture ~target:`texture_2d texid;
        GlTex.parameter ~target:`texture_2d (`mag_filter `nearest);
        GlTex.parameter ~target:`texture_2d (`min_filter `nearest); 
        Gl.enable `texture_2d;
        let x, y, w, h = Window.center_rect rect in
        GlDraw.begins `quads;
        GlTex.coord2 (0.0, 0.0);
        GlDraw.vertex ~x ~y ();
        GlTex.coord2 (1.0, 0.0);
        GlDraw.vertex ~x:(x + w) ~y ();
        GlTex.coord2 (1.0, 1.0);
        GlDraw.vertex ~x:(x + w) ~y:(y + h) ();
        GlTex.coord2 (0.0, 1.0);
        GlDraw.vertex ~x ~y:(y + h) ();
        GlDraw.ends ();
        Gl.disable `texture_2d;
      ) texid; ()

    method event wind ev = 
      print_endline "event:";
      match ev with
      | Event.Parameters ["octaves",Event.Float oct] when wind == trigger#window ->
        texid <- Some (Texture.Tga.gl_maketex (Perlin.Op.array_of_texture (Perlin.Op.normalize 256 (Perlin.Op.clouds 3 oct)))); true
      | ev -> super # event wind ev
    
end
