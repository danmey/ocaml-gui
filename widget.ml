open Draw

open BatFloat

type state = Normal | Pressed | Dragged

let c r g b = GlDraw.color (float r/.255., float g/.255., float b/.255.)
let button_painter state rect =
  let x, y, w, h = Window.center_rect rect in
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

open BatFloat
(* let quad_painter rect = *)
(*    let x, y, w, h = Window.center_rect rect in *)
(*    GlDraw.begins `quads; *)
(*    GlDraw.vertex ~x ~y (); *)
(*    GlDraw.vertex ~x:(x + w) ~y (); *)
(*    GlDraw.vertex ~x:(x + w) ~y:(y + h) (); *)
(*    GlDraw.vertex ~x ~y:(y + h) (); *)
(*    GlDraw.ends (); *)
(*    () *)

open BatInt

class virtual widget = object ( self : 'self )
  val virtual window : Window.window
  method window = window
end

class graphical = object ( self : 'self )
  inherit widget
  val window = (Window.empty_window ())
  val mutable state = Normal
  initializer window.Window.painter <- (fun rect -> self#paint state rect)
  method invalidate rect = window.Window.pos <- rect
  method revalidate = self # invalidate window.Window.pos

  method paint = button_painter
end

class interactive = object ( self : 'self )
  inherit graphical as super
  method event (window : Window.window) (ev : Event.event) = false

  initializer
    Event.register window
      (fun window ev -> self#event window ev)

  method mouse_down button (point : Pos.t) = false
  method mouse_up button (point : Pos.t) = false
  method mouse_motion button (point : Pos.t) = false

  method event wind = function
    | Event.MouseDown (b,point) -> self#mouse_down b point
    | Event.MouseUp (b, point) -> self#mouse_up b point 
    | Event.MouseMotion (b, point) -> self#mouse_motion b point
    | _ -> false
end

class virtual [ 'a ] composite = object ( self : 'self )
  inherit widget as super
  val mutable widgets : (Window.window * 'a) list = []

  method add (widget : 'a) = 
    Window.add self#window (widget#window); 
    widgets <- widgets @ [widget#window, widget]

  method remove widget =
    Window.remove self # window (widget#window);
    widgets <- BatList.remove_if (fun w -> widget == snd w) widgets

  method remove_all =
    self # iter (fun w -> self # remove w)

  method iter f = List.iter (fun (_, w) -> f w) widgets
  method find h = List.assq h widgets
end

class [ 'a ] generic_canvas = object ( self : 'self )
  inherit [ 'a ] composite as composite
  inherit graphical as super
  method add block = 
    composite#add block;
    block#set_parent (self :> 'a generic_canvas)
    
  method first =
    match widgets with
      | (_,f)::_ -> f

  method second =
    match widgets with
      | _::(_,s)::_ -> s

  method dragged widget dpos = ()
  method clicked widget button pos = ()

end and canvas = object ( self : 'self )
  inherit [ draggable ] generic_canvas
end and draggable = object ( self : 'self )
  inherit interactive as super
  val mutable dragged_pos = (0,0)
  val mutable parent : canvas option = None
  method value = ""

  method set_parent c = parent <- Some c
  method name = ""

  method mouse_down button point = 
    state <- Dragged; 
    dragged_pos <- point;
    BatOption.may (fun parent -> parent # clicked (self :> draggable) button point) parent;
    true
      
  method mouse_up _ _ = 
    (if self#drag_end then
      state <- Normal);
    true

  method mouse_motion _ point =
      let window_pos = Rect.pos window.Window.pos in
      let new_window_pos = Pos.sub (Pos.add window_pos point) dragged_pos in
      let dpos = Pos.sub new_window_pos window_pos in
      Event.run_events window (Event.Drag dpos);
      self#drag point new_window_pos dpos;
      true

  method follow_drag dpos =       
    dragged_pos <- Pos.add dpos dragged_pos;

  method drag point pos dpos = 
    print_endline "drag";
    Rect.set_pos window.Window.pos pos;
    BatOption.may (fun parent -> parent#dragged (self :> draggable) dpos) parent
  
  method drag_end = true
end

class fixed = object ( self : 'self )
  inherit draggable

  method follow_drag dpos = () 
  method drag point pos dpos = ()
  method drag_end = false
end

class desktop =  object ( self : 'self )
  inherit [ graphical ] composite as super
  inherit fixed
  initializer 
    ignore(Window.add Window.desktop window)
end

type constr = Horizontal | Vertical | HorizontalWith of int

open BatInt
class draggable_constrained constr = object ( self : 'self )
  inherit draggable
  val mutable constr = constr
  method drag a pos dpos =
    (match constr with 
      | Horizontal -> window.Window.pos.Rect.x <- fst pos
      | Vertical -> window.Window.pos.Rect.y <- snd pos
      | HorizontalWith amount -> window.Window.pos.Rect.x <- ( fst pos + amount / 2 ) / amount * amount);
    BatOption.may (fun parent -> parent#dragged (self :> draggable) dpos) parent

end

class splitter first second constr1 = object ( self : 'self )
  inherit draggable
  inherit canvas as super
    
  val mutable formed = false
  val mutable constr = constr1
  val split_widget = new draggable_constrained constr1
  val first = first
  val second = second
  initializer
    self#add (split_widget);
    self#add (first);
    self#add (second);
    ()

  (* VERY CRUFTY CODE, needs to tide up this! *)
  method dragged widget (dx, dy) =
      if split_widget == widget then
        let first_rect = first#window.Window.pos in
        let second_rect = second#window.Window.pos in
        match constr with
          | Horizontal ->
            first # invalidate { first_rect with Rect.w = first_rect.Rect.w + dx };
            second # invalidate { second_rect with 
              Rect.w = second_rect.Rect.w - dx; 
              Rect.x = second_rect.Rect.x + dx }
          | Vertical ->
            first # invalidate { first_rect with 
              Rect.h = first_rect.Rect.h + dy };
            second # invalidate { second_rect with 
              Rect.h = second_rect.Rect.h - dy; 
              Rect.y = second_rect.Rect.y + dy }
          | _ -> failwith "splitter.dragged: Not supported"
        
  method invalidate rect =
    (match constr with
      | Horizontal ->
        super#invalidate rect;
        let p,_ = Rect.pos window.Window.pos in
        let w,h = Rect.size window.Window.pos in
        let o = p + w / 2 - 10 in
        split_widget#invalidate (Rect.rect (o,0) (20,h));
        first#invalidate (Rect.rect (0,0) (o,h));
        second#invalidate (Rect.rect (o+20,0) (w,h))
      | Vertical ->
        super#invalidate rect;
        let _,p = Rect.pos window.Window.pos in
        let w,h = Rect.size window.Window.pos in
        let o = p + h / 2 - 10 in
        split_widget#invalidate (Rect.rect (0,o) (w, 20));
        first#invalidate (Rect.rect (0,0) (w, o));
        second#invalidate (Rect.rect (0, o+20) (w,h-o-20)))
          
end


type 'a element_tree = Node of string * 'a * 'a element_tree list


open BatFloat

(* class [ 'a ] tree = *)
(*   let sample_tree = *)
(*       [Node ("tool1", 1, [ *)
(*         Node ("tool11", 1,[]); *)
(*         Node ("tool12", 1,[]) *)
(*         ]); *)
(*        Node ("tool2", 2, []); *)
(*        Node ("tool3", 3, [])]  *)
(*   in *)
(* object ( self : 'self ) *)
(*   inherit [ graphical ] composite as super *)
(*   inherit fixed *)
(*   method paint state rect = *)
(*     let rec loop ident i = function *)
(*       | Node (text, id, children) :: xs -> *)
(*         let w,_ = Rect.size self#window.Window.pos in *)
(*         let x, y, w, h =  *)
(*           Window.center_rect (BatInt.(Rect.place_in  *)
(*                                  (Rect.rect (0,i*15) (w,13))  *)
(*                                  self#window.Window.pos)) in *)
(*         let len = float (text_width text) in *)
(*         let ofs_x = (w -. len) /. 2. + x in *)
(*         let ofs_x = float ident*30. + x+15. in *)
(*         let ofs_y = (h -. 10.) /. 2. + y in *)
(*         let ofs_y = y+15. in *)
(*         draw_text (int_of_float ofs_x) (int_of_float ofs_y) text; *)
(*         loop BatInt.(ident+1) BatInt.(i+1) children; *)
(*         loop ident BatInt.(i+1+List.length children) xs *)
(*       | [] -> () *)
(*     in *)
(*     loop 0 0 sample_tree *)
    
(* end *)
open BatFloat

let caption_painter text _ state rect =
   let x, y, w, h = Window.center_rect rect in
   button_painter state rect;
   let len = float (text_width text) in
   let ofs_x = (w - len) / 2. + x in
   let ofs_y = (h - 10.) / 2. + y in
   draw_text (int_of_float ofs_x) (int_of_float ofs_y) text;
   ()

let caption_painter2 text _ state rect =
   let x, y, w, h = Window.center_rect rect in
   let len = float (text_width text) in
   let ofs_x = (w - len) / 2. + x in
   let ofs_y = (h - 10.) / 2. + y in
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
   GlDraw.ends ();

   draw_text (int_of_float ofs_x) (int_of_float ofs_y) text;
   ()

class button = 
  let normal_caption = "Push me!" in
  let pushed_caption = "Pushed" in
object ( self : 'self )
  inherit fixed as super
  val mutable caption = normal_caption
  method paint state = caption_painter caption 0 state
  method mouse_down _ _ = caption <- pushed_caption; true
  method mouse_up _ _ = caption <- normal_caption; true
end

class label name = 
object ( self : 'self )
  inherit fixed as super
  val mutable caption = name
  method paint state = caption_painter2 caption 0 state
end

open BatInt

class slider name left right step = object ( self : 'self )
  inherit draggable_constrained Horizontal as super
  val mutable value = 0.
  val mutable step = step
  val mutable drag_value = 0.0
  val left = left
  val right = right
  val name = name

  method value = string_of_float value
  
  method clamp v = 
    let open BatFloat in
    if v >= left then (if v <= right then v else right) else left
   
  method paint rect state = 
    caption_painter (self#caption (self # clamp (drag_value +. value))) 0 rect state

  method drag _ _ (dx,_) =
    drag_value <- step *. (float dx);


  method drag_end =
    let v = value +. drag_value in
    value <- self # clamp v;
    drag_value <- 0.;
    self # slide_end value;
    true
      
  method slide_end value = ()

  method caption value = 
    Printf.sprintf "%s: %2.2f" name value

  (* method value = value *)
end

open BatFloat
let round v = 
  if fst (modf v) >= 0.5 then
    snd (modf v) +. 1.
  else
    snd (modf v)

open BatInt
class int_slider name left right step = object ( self : 'self )
  inherit slider 
    name
    (float_of_int left)
    (float_of_int right)
    step

  method drag_end =
    value <- round (value +. drag_value);
    drag_value <- 0.;
    self # slide_end value;
    true

  method caption value = 
    Printf.sprintf "%s: %d" name (int_of_float (round value))

  (* method value = value *)
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

let fill_layout parent_rect _ _ _ = Rect.rect (0,0) (Rect.size parent_rect)

class frame layout = object ( self : 'self )
  inherit canvas as super
  inherit fixed
  method invalidate rect =
    super#invalidate rect;
    let count = List.length window.Window.children in
    BatList.iteri (fun i (_,w) ->
      let local_rect = layout (window.Window.pos) (w#window.Window.pos) count i in
      w # invalidate local_rect) widgets
end

open BatFloat
class graphics = object ( self : 'self )
  inherit fixed
  initializer
    window.Window.painter <- self#draw
    method draw rect =
      let x, y, w, h = Window.center_rect rect in
      GlDraw.begins `quads;
      GlDraw.color (0.,0.,0.);
      GlDraw.vertex ~x ~y ();
      GlDraw.vertex ~x:(x + w) ~y ();
      GlDraw.vertex ~x:(x + w) ~y:(y + h) ();
      GlDraw.vertex ~x ~y:(y + h) ();
      GlDraw.ends ();
      ()
end

open BatInt
class menu pos items = object ( self : 'self)
  inherit frame (fixed_vertical_layout 5 20)
  val mutable widget_items : (draggable * string) list = []
  initializer
  let max_width = ref 0 in
  widget_items <- List.fold_left (fun acc name -> acc @ [(new label name :> draggable), name]) [] items;
  List.iter 
    (fun (widget, name) -> 
      self # add widget;
      let len = text_width name in
      if len > !max_width then
        max_width := len) widget_items;
  self # invalidate (Rect.rect pos (!max_width+20, (List.length items) * 25 + 20))
    
  method clicked widget button pos =
    BatOption.may (fun parent -> parent # remove (self :> draggable)) parent;
    Event.run_events self # window 
      (Event.Custom ("menu_item", 
                     pos, 
                     (List.assq widget widget_items)))
    
end
