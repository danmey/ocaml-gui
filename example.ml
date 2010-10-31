open Widget
open Draw
let _ =
  Gui.init 
    (fun () ->
      let g = new desktop in
      let w1 = new composite in
      let w2 = new composite in
      let split = ((new splitter w1 w2 Horizontal) :> graphical) in
      g#add split;
      for i = 0 to 0 do
        let block = (new slider :> graphical) in
        block#invalidate (Rect.rect (i*60+10,10) (50,30));
        w2#add block
      done;
      let w,h = Display.display_size in
      g#invalidate (Rect.rect (0,0) (w,h));
      split#invalidate (Rect.rect (10,10) (w-20,h-20));
      ())
