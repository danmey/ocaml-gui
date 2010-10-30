open Window
open Draw


class graphical = object ( self : 'self )
  val window : window = add desktop Rect.o
  method invalidate rect = window.pos <- rect
  method draw (f : (Rect.t -> Draw.t list)) = draw_window window
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
  initializer 
    Event.register window
      (fun _ ->
        print_endline "gotcha!";
        window.painter <- quad_painter)
end

