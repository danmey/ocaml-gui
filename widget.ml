open Draw
open GL

type state = Normal | Pressed | Dragged | Selected

(* let center_coord c = float c + 0.5 *)
(* let center_rect = Rect.lift22 center_coord float *)

let button_painter state rect =
  let tw,th = 32, 32 in
  let tcx i = float i /. float tw in
  let tcy i = float i /. float th in
  let x, y, w, h = Rect.coords rect in
  let quad (x, y) (w, h) (xt,yt) (wt, ht) =
    let open BatFloat in
    let x,y,w,h = 
      float x,
      float y,
      float w,
      float h
          in
    glColor3 ~r:1. ~g:1. ~b:1.;
    glTexCoord2 ~s:(tcx xt) ~t:(tcy yt);
    glVertex3 ~x ~y ~z:0.;
    glTexCoord2 ~s:(tcx BatInt.(xt + wt)) ~t:(tcy yt);
    glVertex3 ~x:(x + w) ~y ~z:0.;
    glTexCoord2 ~s:(tcx BatInt.(xt+wt)) ~t:(tcy BatInt.(yt+ht));
    glVertex3 ~x:(x + w) ~y:(y + h) ~z:0.;
    glTexCoord2 ~s:(tcx xt) ~t:(tcy BatInt.(yt+ht));
    glVertex3 ~x:x ~y:(y + h) ~z:0.
  in
  let open BatInt in
      let texid = Resource.get (match state with 
        | Normal  -> "button-normal"
        | Dragged -> "button-push" 
        | _       -> "button-normal")
      in

      glEnable GL_TEXTURE_2D;
      glEnable GL_BLEND;
      glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA;

      glBindTexture 
        ~target:BindTex.GL_TEXTURE_2D 
        ~texture:texid;
      glTexParameter 
        ~target:TexParam.GL_TEXTURE_2D 
        ~param:(TexParam.GL_TEXTURE_MAG_FILTER Mag.GL_LINEAR);
      glTexParameter 
        ~target:TexParam.GL_TEXTURE_2D 
        ~param:(TexParam.GL_TEXTURE_MIN_FILTER Min.GL_LINEAR);

      let draw_tex (x,y) (w,h) (tw,th)  b = 
        let b2 = b * 2 in
        quad (x+b, y)     (w-b2, b) (b, 0) (tw-b2, b);
        quad (x+b, y+h-b) (w-b2, b) (b, th-b) (tw-b2, b);
        quad (x, y+b)     (b, h-b2) (0, b) (b, th-b2);
        quad (x + w-b, y+b) (b, h-b2) (tw-b, b) (0, th-b2);
        
              (* Corners *)
        quad (x, y) (b, b) (0, 0) (b, b);
        quad (x+w-b, y) (b, b) (tw-b, 0) (b, b);
        quad (x+w-b, y+h-b) (b, b) (tw-b, th-b) (b, b);
        quad (x, y+h-b) (b, b) (0, th-b) (b, b);

              (* Centre *)
        quad (x+b, y+b) (w-b2, h-b2) (b,b) (tw-b2, th-b2)
      in
      glBegin GL_QUADS;
      draw_tex (x,y) (w,h) (tw, th) 4;
      glEnd ();
      (match state with
        | Selected ->
          print_endline "selected:::";
          begin
            let x, y, w, h = Rect.coords rect in
            let x,y,w,h = 
              float x,
              float y,
              float w,
              float h
            in
            let open BatFloat in
            glDisable GL_TEXTURE_2D;
            glEnable GL_BLEND;
            glBlendFunc Sfactor.GL_SRC_ALPHA Dfactor.GL_ONE_MINUS_SRC_ALPHA;
            glBegin GL_QUADS;
            glColor4 ~r:0. ~g:1. ~b:1. ~a:0.5;
            glVertex3 ~x ~y ~z:0.;
            glVertex3 ~x:(x + w) ~y ~z:0.;
            glVertex3 ~x:(x + w) ~y:(y + h) ~z:0.;
            glVertex3 ~x:x ~y:(y + h) ~z:0.;
            glEnd ();
            print_endline "Selected";
          end
        | _ -> ())
        

let bg_painter state rect =
  let x, y, w, h = Rect.coords rect in
  let quad (x, y) (w, h)  =
    let open BatFloat in
    let x,y,w,h = 
      float x,
      float y,
      float w,
      float h
    in
    glVertex3 ~x ~y ~z:0.;
    glVertex3 ~x:(x + w) ~y ~z:0.;
    glVertex3 ~x:(x + w) ~y:(y + h) ~z:0.;
    glVertex3 ~x:x ~y:(y + h) ~z:0.
    in

  glDisable GL_TEXTURE_2D;
  glDisable GL_BLEND;
  glBegin GL_QUADS;
  glColor3 ~r:(120./.256.) ~g:(100./.256.) ~b:(100./.256.);
  quad (x, y) (w, h);
  glEnd ()

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
  method paint = bg_painter
  method set_state new_state = state <- new_state
  method get_state = state
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
  method widgets = widgets
  method paint (state : state) (rect : Rect.t) = bg_painter state rect

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
  method paint = bg_painter

end and canvas = object ( self : 'self )
  inherit [ draggable ] generic_canvas
end and draggable = object ( self : 'self )
  inherit interactive as super
  val mutable dragged_pos = (0,0)
  val mutable parent : canvas option = None
  method key = ""
  method value = Event.Float 0.
  method set_value (value : float) = ()
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

  method paint = button_painter

end

class fixed = object ( self : 'self )
  inherit draggable
  method follow_drag dpos = () 
  method drag point pos dpos = ()
  method drag_end = false
  method paint = bg_painter
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
  method paint = button_painter
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

let splitter ~first ~second constr = 
  new splitter (first :> draggable) (second :> draggable) constr

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

let caption_painter text state rect =
   let x, y, w, h = Rect.coords rect in
   button_painter state rect;
   let len = float (text_width text) in
   let ofs_x = (float w - len) / 2. + float x in
   let ofs_y = (float h - 10.) / 2. + float y in
   glColor3 ~r:(200./.256.) ~g:(180./.256.) ~b:(180./.256.);
   draw_text (int_of_float ofs_x) (int_of_float ofs_y) text;
   ()

let caption_painter2 text state rect =
  button_painter state rect;
   let x, y, w, h = Rect.coords rect in
   let len = float (text_width text) in
   let ofs_x = (float w - len) / 2. + float x in
   let ofs_y = (float h - 10.) / 2. + float y in
   glColor3 ~r:(200./.256.) ~g:(180./.256.) ~b:(180./.256.);
   draw_text (int_of_float ofs_x) (int_of_float ofs_y) text;
   ()

let text_area_painter line text state rect =
  let open BatInt in
      let x, y, w, h = Rect.coords rect in
      glColor3 ~r:(200./.256.) ~g:(180./.256.) ~b:(180./.256.);
      draw_text x (y+line * 10) text;
      ()

class button name click = 
object ( self : 'self )
  inherit fixed as super
  method paint _ = caption_painter name state
  method mouse_down b _ = click self b
end

let button ~name ~click = new button name click

class label name = 
object ( self : 'self )
  inherit fixed as super
  val mutable caption = name
  method paint state = caption_painter2 caption state
end

let label ~name ~click = new label name

open BatInt

class slider name left right step default (change : slider -> unit)
  = object ( self : 'self )
  inherit draggable_constrained Horizontal as super
  val mutable value = 0.
  val mutable step = step
  val mutable drag_value = 0.0
  val left = left
  val right = right
  val name = name
  initializer 
    value <- default

  method clamp v = 
    let open BatFloat in
        if v >= left then (if v <= right then v else right) else left
          
  method paint rect state = 
    caption_painter (self#caption (self # clamp (drag_value +. value))) rect state

  method drag _ _ (dx,_) =
    drag_value <- step *. (float dx);

  method value = Event.Float value
  method set_value value' = value <- value'
  method key = name
  method drag_end =
    let v = value +. drag_value in
    value <- self # clamp v;
    drag_value <- 0.;
    self # slide_end value;
    change (self :> slider);
    true
      
  method slide_end value = change (self:>slider)

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
class int_slider name left right step default change
  = object ( self : 'self )
  inherit slider 
    name
    (float_of_int left)
    (float_of_int right)
    step
    (float_of_int default)
    change


  method value = Event.Int (int_of_float (round value))
  method drag_end =
    value <- round (value +. drag_value);
    drag_value <- 0.;
    self # slide_end value;
    change (self :> slider);
    true

  method caption value = 
    Printf.sprintf "%s: %d" name (int_of_float (round value))

  (* method value = value *)
end

let float_slider 
    ?min:(min=0.) 
    ?max:(max=1.)
    ?step:(step=0.01)
    ?value:(value=0.)
    ?change:(change=fun (w:slider) -> ())
    name
    = (new slider name min max step value change :> draggable)

let int_slider 
    ?min:(min=0) 
    ?max:(max=100)
    ?step:(step=1.)
    ?value:(value=0)
    ?change:(change=fun (w:slider) -> ()) 
    name
    = (new int_slider name min max step value change  :> draggable)
  
module Defaults = struct
  let widget_height = 25
  let widget_width = 25
  let spacing = 5
end
module Layout = struct
  type t = Rect.t -> Rect.t -> int -> int -> Rect.t

  let horizontal_fill 
      ?spacing:(spacing=Defaults.widget_height) 
      ?offset:(offset=0)
      parent_rect _ c i =
    let (w,h) = Rect.size parent_rect in
    let s = w / c in
    Rect.rect (offset + (i * s)+spacing, 0) (s-2*spacing- offset,h)

  let vertical_fill 
      ?spacing:(spacing=Defaults.spacing)
      ?offset:(offset=0)
      parent_rect _ c i =
    let (w,h) = Rect.size parent_rect in
    let s = h / c in
    Rect.rect (0, offset + (i * s)+spacing) (w, s-2*spacing - offset)

  let vertical 
      ?spacing:(spacing=Defaults.spacing) 
      ?size:(height=Defaults.widget_height) 
      parent_rect d c i =
    let (w,h) = Rect.size parent_rect in
    if (height + 2 * spacing) * c > h then
      vertical_fill ~spacing parent_rect d c i
    else
      let ofs = i * (height + 2 * spacing) + spacing in
      Rect.rect (spacing, ofs) (w - spacing * 2, height)

  let horizontal 
      ?spacing:(spacing=Defaults.widget_height) 
      ?size:(width=Defaults.widget_width) 
      parent_rect d c i =
    let (w,h) = Rect.size parent_rect in
    if (width + 2 * spacing) * c > w then
      horizontal_fill ~spacing parent_rect d c i
    else
      let ofs = i * (width + 2 * spacing) + spacing in
      Rect.rect (ofs, spacing) (width, h - spacing * 2)

  let fill parent_rect _ _ _ = Rect.rect (0,0) (Rect.size parent_rect)
  open BatInt
  let vertical_fixed 
      ?sizes:(sizes=[Defaults.widget_height]) 
      ?spacing:(spacing=Defaults.spacing)
      parent_rect d c i =
    let n = List.length sizes in
    if i < n then
      let size = List.nth sizes i in
      vertical ~spacing ~size parent_rect d c i
    else
      let accum_size = BatList.reduce ( + ) sizes in
      vertical_fill ~spacing ~offset:accum_size parent_rect d (c-n) (i-n)

  let horizontal_fixed 
      ?sizes:(sizes=[Defaults.widget_height]) 
      ?spacing:(spacing=Defaults.spacing)
      parent_rect d c i =
    let n = List.length sizes in
    if i < n then
      let size = List.nth sizes i in
      horizontal ~spacing ~size parent_rect d c i
    else
      let accum_size = BatList.reduce ( + ) sizes in
      horizontal_fill ~spacing ~offset:accum_size parent_rect d (c-n) (i-n)
end

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

class graphics = object ( self : 'self )
  inherit fixed
  initializer window.Window.painter <- self#draw
  method draw _ = ()
end

class menu pos items select = object ( self : 'self)
  inherit frame Layout.vertical
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
    select pos (List.assq widget widget_items);
    ()
    
end

let menu ~items ~pos ~select =
  new menu pos items select

type menu_entry = 
  | Entry of string * (unit -> unit)
  | SubMenu of string * menu_entry list


class menu_bar definition sizes =
object (self : 'self)
  inherit frame (Layout.horizontal_fixed ~sizes ~spacing:0)
  initializer
  let add_menu_entry = function
    | Entry (name, f) ->
      self # add (button ~name ~click:(fun _ _ -> f (); true))
  in
  List.iter add_menu_entry definition
end

let menu_bar definition =   
  let entry_size = function Entry (name, _ ) -> text_width name + 5 in
  let sizes = List.map entry_size definition in
  new menu_bar definition sizes

open BatStd
class edit_area =    
object ( self : 'self )
  inherit fixed as super
  val mutable lines = [""]

  method paint state rect =
    BatList.iteri 
      (fun line text -> 
        text_area_painter line text state rect) <| List.rev lines

  method event wind = function
    | Event.KeyPress (char, point) ->
      (match char with | '\r' -> lines <- "" :: lines 
        | _ ->
          begin
            lines <- (match lines with
              | last :: rest -> (last ^ BatString.of_char char) :: rest
              | lst -> lst)
          end);
      true
    | _ -> false
end
    
(* class content_pane = object (self : 'self) *)

(* let ref_pane content:= *)
(*   new frame Layout.fill *)
