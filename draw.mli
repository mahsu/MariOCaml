
(* Renders a given object on the canvas *)
val render : Object.obj -> unit

(* Clears the canvas *)
val clear_canvas : Dom_html.canvasElement Js.t -> unit

(* Draws the fps on the canvas *)
val fps : Dom_html.canvasElement Js.t -> float -> unit
