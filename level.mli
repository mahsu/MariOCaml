open Object


(* Represents the game map *)
type map


(* Gets a list of objects in a certain position in the grid *)
val get : unit -> obj list

(* Puts an object in a certain position in the grid *)
val put : unit -> obj list
