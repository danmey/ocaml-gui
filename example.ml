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
      for i = 0 to 1 do
        let block = (new button :> graphical) in
        block#invalidate (Rect.rect (i*60+10,10) (50,30));
        w2#add block
      done;
      g#invalidate (Rect.rect (50,50) (400,400));
      split#invalidate (Rect.rect (10,10) (350,350));
      ())
