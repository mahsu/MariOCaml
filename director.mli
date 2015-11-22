open Dom_html

(* Initiates the main game loop *)
val update_loop : Dom_html.canvasElement Js.t -> (Object.obj list) -> unit

val keydown : (#keyboardEvent as 'a) Js.t -> bool Js.t 
