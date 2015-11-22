open Actors
open Sprite
open Object
module Html = Dom_html

let loadCount =  ref 0
let imgsToLoad = 1

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
  let _ = Html.addEventListener Html.document Html.Event.keydown (Html.handler Director.keydown) Js._true in
  (*let _ = Html.addEventListener Html.document Html.Event.keyup (Html.handler Director.keyup) Js._true*)
  let obj_c1 = Object.spawn Coin context (0.0,0.0) in
  let obj_c2 = Object.spawn Coin context (200.0,300.0) in
  Director.update_loop canvas [obj_c1; obj_c2] ;
  ()

let inc_counter _ = 
  loadCount := !loadCount + 1;
  if !loadCount = imgsToLoad then load() else ()

let preload _ =
  let imgs = [ "coin.png" ] in
  List.map (fun img_src ->
    let img = (Dom_html.createImg Dom_html.document) in
    img##src <- (Js.string img_src) ;
    ignore(Html.addEventListener  img Dom_html.Event.load 
    (Html.handler (fun ev ->  inc_counter(); Js._true)) Js._true)) imgs


let _ = Dom_html.window##onload <- Dom_html.handler (fun _ -> ignore (load()); Js._true)
