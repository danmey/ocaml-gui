open Window
open Draw

open BatFloat

type state = Normal | Pressed | Dragged

let button_painter state rect =
  let c r g b = GlDraw.color (float r/.255., float g/.255., float b/.255.) in
  let x, y, w, h = center_rect rect in
  (* Should use skinning instead *)
  match state with
    | Normal ->
      GlDraw.begins `lines;
      c 132 132 132;
      GlDraw.vertex ~x:(x + w-2.) ~y:(y + 1.) ();
      GlDraw.vertex ~x:(x + w-2.) ~y:(y + h-2.) ();

      GlDraw.vertex ~x:(x+1.) ~y:(y + h - 2.) ();
      GlDraw.vertex ~x:(x + w-2.) ~y:(y + h-2.) ();

      c 66 66 66;
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + 0.) ();
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + h-1.) ();

      GlDraw.vertex ~x:x ~y:(y + h-1.) ();
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + h-1.) ();

      c 255 255 255;
      GlDraw.vertex ~x:x ~y:y ();
      GlDraw.vertex ~x:(x + w-2.) ~y:y ();

      GlDraw.vertex ~x:x ~y:y ();
      GlDraw.vertex ~x:x ~y:(y+h-2.) ();

      GlDraw.ends ()

    | Pressed ->
      GlDraw.begins `lines;
      
      c 255 255 255;
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + 0.) ();
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + h-1.) ();
      
      GlDraw.vertex ~x:x ~y:(y + h-1.) ();
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + h-1.) ();
      
      c 132 132 132;
      GlDraw.vertex ~x:x ~y:y ();
      GlDraw.vertex ~x:(x + w-2.) ~y:y ();
      
      GlDraw.vertex ~x:x ~y:y ();
      GlDraw.vertex ~x:x ~y:(y+h-2.) ();
      GlDraw.ends ()
    | Dragged ->
      GlDraw.begins `lines;
      
      c 255 0 0;
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + 0.) ();
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + h-1.) ();
      
      GlDraw.vertex ~x:x ~y:(y + h-1.) ();
      GlDraw.vertex ~x:(x + w-1.) ~y:(y + h-1.) ();
      
      c 255 132 132;
      GlDraw.vertex ~x:x ~y:y ();
      GlDraw.vertex ~x:(x + w-2.) ~y:y ();
      
      GlDraw.vertex ~x:x ~y:y ();
      GlDraw.vertex ~x:x ~y:(y+h-2.) ();
      GlDraw.ends ()

open BatInt

class widget = object
  val window : window = (empty_window ())
end

class graphical = object ( self : 'self )
  inherit widget
  val mutable state = Normal
  initializer window.painter <- (fun rect -> self#paint state rect)
  method invalidate rect = window.pos <- rect
  method window = window
  method paint = button_painter
end

open BatFloat
let quad_painter rect =
   let x, y, w, h = center_rect rect in
   GlDraw.begins `quads;
   GlDraw.vertex ~x ~y ();
   GlDraw.vertex ~x:(x + w) ~y ();
   GlDraw.vertex ~x:(x + w) ~y:(y + h) ();
   GlDraw.vertex ~x ~y:(y + h) ();
   GlDraw.ends ();
   ()

class interactive = object ( self : 'self )
  inherit graphical as super
  method event (window : Window.window) (ev : Event.event) = false
  initializer
    Event.register window
      (fun window ev -> self#event window ev)

  method mouse_down (point : Pos.t) = false
  method mouse_up (point : Pos.t) = false
  method mouse_motion (point : Pos.t) = false

  method event wind = function
    | Event.MouseDown point -> self#mouse_down point
    | Event.MouseUp point -> self#mouse_up point 
    | Event.MouseMotion point -> self#mouse_motion point
    | _ -> false
end

class draggable = object ( self : 'self )
  inherit interactive as super
  val mutable dragged_pos = (0,0)

  method mouse_down point = 
    state <- Dragged; 
    dragged_pos <- point; 
    true
      
  method mouse_up _ = 
    state <- Normal; 
    self#drag_end; 
    true

  method mouse_motion point =
      let window_pos = Rect.pos window.pos in
      let new_window_pos = 
        Pos.sub (Pos.add window_pos point) dragged_pos in
      let dpos = Pos.sub new_window_pos window_pos in
      self#drag new_window_pos dpos;
      let w,h = Rect.size window.pos in
      Event.run_events window (Event.Drag dpos);
      true

  method drag pos dpos = Rect.set_pos window.pos pos

  method drag_end = ()
end

class composite = object ( self : 'self )
  inherit interactive as super
  val mutable widgets = []
  method add (widget : graphical) = 
    Window.add window (widget#window); 
    widgets <- widgets @ [widget]
end

class desktop =  object ( self : 'self )
  inherit composite as super
  initializer 
    ignore(Window.add Window.desktop window); ()
end

type constr = Horizontal | Vertical

class draggable_constrained constr = object ( self : 'self )
  inherit draggable
  val mutable constr = constr
  method drag pos dpos =
    match constr with 
      | Horizontal -> window.pos.Rect.x <- fst pos
      | Vertical -> window.pos.Rect.y <- snd pos
end

open BatInt
class splitter first second constr1 = object ( self : 'self )
  inherit composite as super
  val mutable constr = constr1
  val split_widget = new draggable_constrained constr1
  val first = first
  val second = second
  val mutable formed = false
  initializer
    self#add (split_widget :> graphical);
    self#add (first :> graphical);
    self#add (second :> graphical);
    ()

  (* VERY CRUFTY CODE, needs to tide up this! *)
  method event (window : Window.window) (ev : Event.event) = 
    match ev with
      | Event.Drag (dx, dy) when split_widget#window == window ->
        (match constr with
          | Horizontal ->
            let w1,h = Rect.size first#window.pos in
            let w2,_ = Rect.size second#window.pos in
            let x2,_ = Rect.pos second#window.pos in
            first#invalidate (Rect.rect (0,0) (w1+dx,h));
            second#invalidate (Rect.rect (x2+dx,0) (w2-dx,h));
            true
          | Vertical ->
            let w,h1 = Rect.size first#window.pos in
            let _,h2 = Rect.size second#window.pos in
            let _,y2 = Rect.pos second#window.pos in
            first#invalidate (Rect.rect (0,0) (w, h1+dy));
            second#invalidate (Rect.rect (0, y2+dy) (w,h2-dy));
            true)
      | _ -> false
        
  method invalidate_after_init rect =
        (match constr with
          | Horizontal ->
            super#invalidate rect;
            let x,y = Rect.pos window.pos in
            let w,h = Rect.size window.pos in
            let half = split_widget#window.pos.Rect.x + 10 in
            let xs = half - 10 in
            split_widget#invalidate (Rect.rect (xs,0) (20,h));
            first#invalidate (Rect.rect (0,0) (xs,h));
            second#invalidate (Rect.rect (xs,0) (w,h-xs))
          | Vertical ->
            super#invalidate rect;
            let x,y = Rect.pos window.pos in
            let w,h = Rect.size window.pos in
            let half = split_widget#window.pos.Rect.y + 10 in
            let ofs = half - 10 in
            split_widget#invalidate (Rect.rect (0,ofs) (w, 20));
            first#invalidate (Rect.rect (0,0) (w, ofs));
            second#invalidate (Rect.rect (0, ofs+20) (w,h-ofs-20)))

  method invalidate_before_init rect = 
        (match constr with
          | Horizontal ->
            super#invalidate rect;
            let x,y = Rect.pos window.pos in
            let w,h = Rect.size window.pos in
            let half = x+w/2 in
            let xs = half - 10 in
            split_widget#invalidate (Rect.rect (xs,0) (20,h));
            first#invalidate (Rect.rect (0,0) (xs,h));
            second#invalidate (Rect.rect (xs,0) (w,h))
          | Vertical ->
            super#invalidate rect;
            let x,y = Rect.pos window.pos in
            let w,h = Rect.size window.pos in
            let half = x+h/2 in
            let ofs = half - 10 in
            split_widget#invalidate (Rect.rect (0,ofs) (w, 20));
            first#invalidate (Rect.rect (0,0) (w, ofs));
            second#invalidate (Rect.rect (0, ofs+20) (w,h-ofs-20)))

  method invalidate rect =
    (if not formed then self#invalidate_before_init else self#invalidate_after_init) rect;
      formed <- true;
      
end

class block = object ( self : 'self )
  inherit composite as super
  val bar_widget = new draggable
    
  initializer
    self#add (bar_widget :> graphical);
    ()

  method event (wind : Window.window) (ev : Event.event) = 
    super#event wind ev;
    match ev with
      | Event.Drag dpos when bar_widget#window == wind ->
        print_endline "drag2";
        self#invalidate (Rect.by window.pos dpos); true
      | _ -> false

  method invalidate rect = 
    super#invalidate rect;
    let x,y = Rect.pos rect in
    let w,h = Rect.size rect in
    bar_widget#invalidate (Rect.rect (0,0) (w,20))
end

open BatFloat
let caption_painter text _ state rect =
   let x, y, w, h = center_rect rect in
   button_painter state rect;
   let len = float (text_width text) in
   let ofs_x = (w - len) / 2. + x in
   let ofs_y = (h - 10.) / 2. + y in
   draw_text (int_of_float ofs_x) (int_of_float ofs_y) text;
   ()

class button = 
  let normal_caption = "Push me!" in
  let pushed_caption = "Pushed" in
object ( self : 'self )
  inherit interactive as super
  val mutable caption = normal_caption
  method paint state = caption_painter caption 0 state
  method mouse_down _ = caption <- pushed_caption; true
  method mouse_up _ = caption <- normal_caption; true
end

open BatInt
class slider = object ( self : 'self )
  inherit draggable_constrained Horizontal as super
  val mutable value = 0.
  val mutable step = 0.01
  val mutable drag_value = 0.0
  method paint rect state = 
    caption_painter (Printf.sprintf "%2.2f" (drag_value +. value)) 0 rect state

  method drag _ (dx,_) =
    drag_value <- step *. (float dx);
    
  method drag_end =
    value <- value +. drag_value;
    drag_value <- 0.
  method value = value
end

type layout = Rect.t -> Rect.t -> int -> int -> Rect.t
let horizontal_layout spacing parent_rect _ c i =
  let (w,h) = Rect.size parent_rect in
  let s = w / c in
    Rect.rect ((i * s)+spacing, 0) (s-2*spacing,h)

let vertical_layout spacing parent_rect _ c i =
  let (w,h) = Rect.size parent_rect in
  let s = h / c in
    Rect.rect (0, (i * s)+spacing) (w, s-2*spacing)

let fixed_vertical_layout spacing height parent_rect d c i =
  let (w,h) = Rect.size parent_rect in
  if (height + 2 * spacing) * c > h then
    vertical_layout spacing parent_rect d c i
  else
    let ofs = (i * (height + 2 * spacing)) + spacing in
    Rect.rect (spacing, ofs) (w-spacing*2,height)

let fixed_horizontal_layout spacing width parent_rect d c i =
  let (w,h) = Rect.size parent_rect in
  if (width + 2 * spacing) * c > w then
    horizontal_layout spacing parent_rect d c i
  else
    let ofs = (i * (width + 2 * spacing)) + spacing in
    Rect.rect (ofs, spacing) (width, h-spacing*2)

class frame layout = object ( self : 'self )
  inherit composite as super
    
  method invalidate rect =
    super#invalidate rect;
    let count = List.length window.children in
    BatList.iteri (fun i w ->
      let local_rect = layout (window.pos) (w#window.pos) count i in
      w # invalidate local_rect) widgets
end

open BatFloat
class graphics = object ( self : 'self )
  inherit graphical
  initializer
    window.painter <- self#draw
    method draw rect =
      let x, y, w, h = center_rect rect in
      GlDraw.begins `quads;
      GlDraw.color (0.,0.,0.);
      GlDraw.vertex ~x ~y ();
      GlDraw.vertex ~x:(x + w) ~y ();
      GlDraw.vertex ~x:(x + w) ~y:(y + h) ();
      GlDraw.vertex ~x ~y:(y + h) ();
      GlDraw.ends ();
      ()
end
    
