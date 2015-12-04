open Actors
open Sprite
open Object
module Html = Dom_html
module Pg = Procedural_generator

let loadCount =  ref 0
let imgsToLoad = 4
let level_width = 1600./.16.
let level_height = (256./.16.) -. 1.

let get_width () = level_width

(*Canvas is chosen from the index.html file. The context is obtained from
 *the canvas. Listeners are added. A level is generated and the general
 *update_loop method is called to make the level playable.*)
let load _ =
  Random.self_init();
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
  Pg.init ();
  Director.update_loop canvas (Pg.generate level_width level_height context);
  ()

let inc_counter _ =
  loadCount := !loadCount + 1;
  if !loadCount = imgsToLoad then load() else ()

(*Used for concurrency issues.*)
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
