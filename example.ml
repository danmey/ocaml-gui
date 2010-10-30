open Widget
open Draw
let _ =
  Gui.init 
    (fun () ->
      let g = new desktop in
      g#invalidate (Rect.rect (50,50) (400,400));
      (* let w1 = new graphical in *)
      (* let w2 = new graphical in *)
      (* let g1 = ((new splitter w1 w2 Horizontal) :> graphical) in *)
      let g1 = (new dialog :> graphical) in
      g1#invalidate (Rect.rect (10,10) (350,350));
      g#add g1;
      ())
