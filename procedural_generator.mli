open Object
open Actors

type obj_coord

val init : unit -> unit

(* Procedurally generates a new map of default size*)
val generate : float -> float -> Dom_html.canvasRenderingContext2D Js.t ->
               collidable * collidable list