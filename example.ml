open Widget
open Draw
let _ =
  Gui.init 
    (fun () ->
      let g = new desktop in
      (* let w1 = new graphical in *)
      (* let w2 = new graphical in *)
      (* let g1 = ((new splitter w1 w2 Horizontal) :> graphical) in *)
      for i = 1 to 5 do
        let block = (new draggable :> graphical) in
        block#invalidate (Rect.rect (i*80,10) (70,30));
        g#add block
      done;
      g#invalidate (Rect.rect (50,50) (400,400));
      ())
