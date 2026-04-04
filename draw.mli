open Js_of_ocaml

(* Renders a given object on the canvas *)
val render : Sprite.sprite -> float * float -> unit

(* Clears the canvas *)
val clear_canvas : Dom_html.canvasElement Js.t -> unit

(* Draw the given sprite as a background *)
val draw_bgd : Sprite.sprite -> float -> unit

(* Draws the axis aligned bounding box of the sprite at the position *)
val render_bbox : Sprite.sprite -> float * float -> unit

(* Draws the fps on the canvas *)
val fps : Dom_html.canvasElement Js.t -> float -> unit

(* Draw the heads up display *)
val hud : Dom_html.canvasElement Js.t -> int -> int -> unit

(* Draw the game win screen *)
val game_win : Dom_html.canvasRenderingContext2D Js.t -> unit

(* Draw the game loss screen *)
val game_loss : Dom_html.canvasRenderingContext2D Js.t -> unit
