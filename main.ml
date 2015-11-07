open Sprite
open Object

let load _ =
  let canvas_id = "canvas" in
  let canvas = 
    Js.Opt.get 
      (Js.Opt.bind ( Dom_html.document##getElementById(Js.string canvas_id)) 
        Dom_html.CoerceTo.canvas) 
      (fun () -> (Printf.printf "cant find canvas %s \n" canvas_id; failwith "fail")) in
  (*let () = Graphics_js.open_canvas canvas in*)
  (*let context = get_context() in*)
  let context = canvas##getContext (Dom_html._2d_) in
  let coin = Sprite.setup_sprite "coin.png" 10 (100.,100.) (0.,0.) in
  let obj_c1 = Object.new_object coin context (0.0,0.0) in
    Draw.init_draw canvas obj_c1;
  ()

let _ = Dom_html.window##onload <- Dom_html.handler (fun _ -> ignore (load()); Js._true)
