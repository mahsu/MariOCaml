


(* Initiates the main game loop *)
val update_loop : Dom_html.canvasElement Js.t 
                  -> (Object.collidable * Object.collidable list) 
                  -> float*float 
                  -> unit 

val keydown : #Dom_html.keyboardEvent Js.t -> bool Js.t

val keyup : #Dom_html.keyboardEvent Js.t -> bool Js.t
