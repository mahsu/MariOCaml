open Object
open Actors
open Sprite
module Html = Dom_html

module Hashable = struct
  type t = int
  let equal i j =
end

module Hash = Hashtbl.Make(Hashable)

let h = Hash.create 1 in
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
let player = Object.spawn (SPlayer Standing) context (32.,32.) in

Hash.add h player "poop"