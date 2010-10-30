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
      self#drag_end new_window_pos
    | _ -> ()
  method drag_end pos =
    Rect.set_pos window.pos pos
    
end

class composite = object ( self : 'self )
  inherit graphical as super
  method add (widget : graphical) = Window.add window (widget#window)
end

class desktop =  object ( self : 'self )
  inherit composite as super
  initializer 
    Window.add Window.desktop window; ()
end

type constr = Horizontal | Vertical

class draggable_constrained = object ( self : 'self )
  inherit draggable
  method drag_end pos =
    window.pos.Rect.x <- fst pos
end

class splitter = object ( self : 'self )
  inherit composite as super
  val split_widget = new draggable_constrained
  initializer
    self#add (split_widget :> graphical);()
  method event (window : Window.window) (ev : Event.event) = if window == split_widget#window then print_endline "gotcha"; ()
  method invalidate rect = 
    super#invalidate rect;
    split_widget#invalidate (Rect.rect (40,40) (40, 40))
end
