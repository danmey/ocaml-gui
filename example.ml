open Widget
open Draw
let _ =
  Gui.init 
    (fun () ->
      let g = new desktop in
      let w1 = new frame (fixed_horizontal_layout 5 60) in
      let w2 = new frame (horizontal_layout 5) in
      let split = ((new splitter w1 w2 Vertical) :> graphical) in
      g#add split;
      for i = 0 to 10 do
        let block = (new slider :> graphical) in
        w1#add block
      done;
      let w,h = Display.display_size in
      split#invalidate (Rect.rect (10,10) (w-20,h-20));
      g#invalidate (Rect.rect (0,0) (w,h));
      ())
