open Actors
open Sprite
open Object
module Html = Dom_html

let loadCount =  ref 0
let imgsToLoad = 4

let load _ =
  let canvas_id = "canvas" in
  let canvas =
    Js.Opt.get
      (Js.Opt.bind ( Dom_html.document##getElementById(Js.string canvas_id))
        Dom_html.CoerceTo.canvas)
      (fun () ->
        Printf.printf "cant find canvas %s \n" canvas_id;
        failwith "fail"
      ) in
  (*let () = Graphics_js.open_canvas canvas in*)
  (*let context = get_context() in*)
  let context = canvas##getContext (Dom_html._2d_) in
  let _ = Html.addEventListener Html.document Html.Event.keydown (Html.handler Director.keydown) Js._true in
  let _ = Html.addEventListener Html.document Html.Event.keyup (Html.handler Director.keyup) Js._true in
  let player = Object.spawn (SPlayer Standing) context (200.,32.) in
  let obj_c1 = Object.spawn (SItem Coin) context (0.0,0.0) in
  let obj_c2 = Object.spawn (SItem Coin) context (200.0,300.0) in 
  let brick1 = Object.spawn (SBlock Brick) context (200.0,200.0) in
  let brick2 = Object.spawn (SBlock Brick) context (216.0,200.0) in
  let brick3 = Object.spawn (SBlock Brick) context (232.0,200.0) in
  let brick4 = Object.spawn (SBlock Brick) context (248.0,200.0) in
  let brick5 = Object.spawn (SBlock Brick) context (264.0,200.0) in
  let brick6 = Object.spawn (SBlock Brick) context (280.0,200.0) in
  let brick7 = Object.spawn (SBlock Brick) context (296.0,200.0) in
  let brick8 = Object.spawn (SBlock Brick) context (312.0,200.0) in
  let brick9 = Object.spawn (SBlock Brick) context (312.0,184.0) in
  let brick10 = Object.spawn (SBlock Brick) context (184.0,200.0) in
  let brick11 = Object.spawn (SBlock Brick) context (168.0,200.0) in
  let brick12 = Object.spawn (SBlock Brick) context (152.0,200.0) in
  let brick13 = Object.spawn (SBlock Brick) context (152.0,184.0) in
  let enemy1 = Object.spawn (SEnemy Goomba) context (248.0,160.0) in
  let enemy2 = Object.spawn (SEnemy GKoopa) context (280.0,160.0) in

  Director.update_loop canvas [player; obj_c1; obj_c2; brick1; brick2; brick3; brick4; brick5; enemy1;
                              brick6; brick7; brick8; brick9; brick10; brick11; brick12; brick13; enemy2; ] ;
  ()

let inc_counter _ =
  loadCount := !loadCount + 1;
  if !loadCount = imgsToLoad then load() else ()

let preload _ =
  let root_dir = "sprites/" in
  let imgs = [ "blocks.png";"items.png";"enemies.png";"mario-small.png" ] in
  List.map (fun img_src ->
    let img_src = root_dir ^ img_src in
    let img = (Dom_html.createImg Dom_html.document) in
    img##src <- (Js.string img_src) ;
    ignore(Html.addEventListener  img Dom_html.Event.load
    (Html.handler (fun ev ->  inc_counter(); Js._true)) Js._true)) imgs


let _ = Dom_html.window##onload <- Dom_html.handler (fun _ -> ignore (preload()); Js._true)
