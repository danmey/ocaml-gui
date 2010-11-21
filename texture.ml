module Tga = struct
  let load file_name = 
    let ch = open_in_bin file_name in
      ignore (input_byte ch);
      ignore (input_byte ch);
      ignore (input_byte ch);

      ignore (input_byte ch); ignore (input_byte ch);
      ignore (input_byte ch); ignore (input_byte ch);
      ignore (input_byte ch);
      ignore (input_byte ch); ignore (input_byte ch);
      ignore (input_byte ch); ignore (input_byte ch);

      let input_word ch = 
	let b1 = input_byte ch in
	let b2 = input_byte ch in
	  b1 lor (b2 lsl 8) in
      let w = input_word ch in
      let h = input_word ch in
	Printf.printf "%d %d\n" w h;
      let pixel_array = Array.create (h*w) (0.,0.,0.,0.) in
      let pix n = float_of_int n /. 255.0 in
      let n = input_byte ch / 8 in
	ignore (input_byte ch);
	for y = 0 to h-1 do
	  for x = 0 to w-1 do
	    let b = pix (input_byte ch) in
	    let g = pix (input_byte ch) in
	    let r = pix (input_byte ch) in
	    let a = if n = 4 then pix (input_byte ch) else 1.0 in
	      pixel_array.(y*w+x) <- (r,g,b,a)
	  done;
	done;
      close_in ch;
      pixel_array

let image_width = 256
and image_height = 256

let make_image arr =
  let image =
    GlPix.create `ubyte ~height:image_height ~width:image_width ~format:`rgba in
  let raw = GlPix.to_raw image
  and pos = GlPix.raw_pos image in
    for i = 0 to image_width - 1 do
      for j = 0 to image_height - 1 do
	let r,g,b,a = arr.(image_width*i+j) in 
(*	let v = arr.(image_width*i+j) in *)
	Raw.sets raw ~pos:(pos ~x:j ~y:i)
	  (ArrayLabels.map ~f:(fun x -> truncate (255.0 *. x))
	   [|r;g;b;a|]);
      done;
    done;
    image

  let gl_load file_name  = 
    let tex = load file_name in
    let image = make_image tex in
    let tid = GlTex.gen_texture() in
      GlTex.bind_texture `texture_2d tid;
      GlTex.parameter ~target:`texture_2d (`mag_filter `nearest);
      GlTex.parameter ~target:`texture_2d (`min_filter `nearest); 
      GlTex.image2d image;
      tid

  let gl_maketex arr  = 
    let image = make_image arr in
    let tid = GlTex.gen_texture() in
      GlTex.bind_texture `texture_2d tid;
      GlTex.parameter ~target:`texture_2d (`mag_filter `nearest);
      GlTex.parameter ~target:`texture_2d (`min_filter `nearest); 
      GlTex.image2d image;
      tid

end
