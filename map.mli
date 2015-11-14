open Object


(* Represents the game map *)
type map;

(* x,y coordinate vector *)
type xy;

(* Gets a list of objects in a certain position in the grid *)
val get : xy -> obj list

(* Puts an object in a certain position in the grid *)
val put : xy -> obj -> map
