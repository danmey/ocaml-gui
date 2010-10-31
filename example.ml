open Widget
open Draw
let _ =
  Gui.init 
    (fun () ->
      let g = new desktop in
      let control_pane = new frame (fixed_vertical_layout 5 25) in
      let edit_pane = new frame (fixed_vertical_layout 5 25) in
      let split_control = ((new splitter control_pane edit_pane Vertical) :> graphical) in
      let graphical_pane = new graphical in
      let split_display = ((new splitter split_control graphical_pane Horizontal) :> graphical) in
      g#add split_display;
      for i = 0 to 10 do
        let block = (new slider :> graphical) in
        control_pane#add block;
        let block = (new slider :> graphical) in
        edit_pane#add block
      done;
      let w,h = Display.display_size in
      split_display#invalidate (Rect.rect (10,10) (w-20,h-20));
      g#invalidate (Rect.rect (0,0) (w,h));
      ())
