open Window
open Draw


class graphical = object ( self : 'self )
  val window : window = (empty_window ())
  method invalidate rect = window.pos <- rect
  method window = window
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
  method event (window : Window.window) (ev : Event.event) = ()
  initializer 
    Event.register window
      (fun window ev -> self#event window ev)
end

type drag_state = Dragged | Placed

class draggable = object ( self : 'self )
  inherit interactive as super
  val mutable state = Placed
  val mutable dragged_pos = (0,0)
  method event window = function
    | Event.MouseDown p -> state <- Dragged; dragged_pos <- p;
    | Event.MouseUp p -> state <- Placed
    | Event.MouseMotion p -> 
      let window_pos = Rect.pos window.pos in
      let new_window_pos = 
        Pos.sub (Pos.add window_pos p) dragged_pos in
      self#drag_end new_window_pos;
      let dpos = Pos.sub new_window_pos window_pos in
      let w,h = Rect.size window.pos in
        Event.run_events window (Event.Drag dpos)
    | _ -> ()
  method drag_end pos =
    Rect.set_pos window.pos pos
    
end

class composite = object ( self : 'self )
  inherit interactive as super
  method add (widget : graphical) = Window.add window (widget#window)
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
  method drag_end pos =
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
  initializer
    self#add (split_widget :> graphical);
    self#add (first :> graphical);
    self#add (second :> graphical);
    ()

  method event (window : Window.window) (ev : Event.event) = 
    match ev with
      | Event.Drag (_, _) when split_widget#window == window -> 
        let x, y = Rect.pos window.pos in
        let w, h = Rect.size window.pos in
        let half,_ = Rect.pos split_widget#window.pos in
        let half =  half + 10 in
        let xs = half - 10 in
        first#invalidate (Rect.rect (0,0) (xs,h));
        second#invalidate (Rect.rect (xs+20,0) (w-20,h))
      | _ -> ()

  method invalidate rect = 
    super#invalidate rect;
    let x,y = Rect.pos rect in
    let w,h = Rect.size rect in
    let half = x+w/2 in
    let xs = half - 10 in
    split_widget#invalidate (Rect.rect (xs,0) (20,h));
    first#invalidate (Rect.rect (0,0) (xs,h));
    second#invalidate (Rect.rect (xs+20,0) (w,h))
end

class dialog = object ( self : 'self )
  inherit composite as super
  val bar_widget = new draggable
    
  initializer
    self#add (bar_widget :> graphical);
    ()

  method event (wind : Window.window) (ev : Event.event) = 
    match ev with
      | Event.Drag dpos when bar_widget#window == wind -> 
        self#invalidate (Rect.by window.pos dpos)
      | _ -> ()

  method invalidate rect = 
    super#invalidate rect;
    let x,y = Rect.pos rect in
    let w,h = Rect.size rect in
    bar_widget#invalidate (Rect.rect (0,0) (w,20))
end
