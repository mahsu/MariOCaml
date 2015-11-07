open Sprite
open Object

let load _ =
  let canvas_id = "canvas" in
  let canvas = 
    Js.Opt.get 
      (Js.Opt.bind ( document##getElementById(Js.String canvas_id)) 
        Dom_html.CoerceTo.canvas) 
      (fun () -> Printf.printf "cant find canvas %s \n" canvas_id) in
  (*let () = Graphics_js.open_canvas canvas in*)
  (*let context = get_context() in*)
  let context = canvas##getContext (Html._2d_) in
  let coin = new_sprite context "coin.png" 10 (0.0,0.0) (0.0,0.0) (100.0,100.0) in
  update_frame coin ;
  ()
let _ = Dom_html.window##onload <- Dom_html.handler (fun _ -> ignore (load ()); Js._true)
