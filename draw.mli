
(* Renders a given object on the canvas *)
val render : Sprite.sprite -> float * float  -> unit

(* Clears the canvas *)
val clear_canvas : Dom_html.canvasElement Js.t -> unit

val draw_bgd : Sprite.sprite -> float -> unit

val render_bbox : Sprite.sprite -> float * float -> unit
(* Draws the fps on the canvas *)
val fps : Dom_html.canvasElement Js.t -> float -> unit

val hud : Dom_html.canvasElement Js.t -> int -> int -> unit
