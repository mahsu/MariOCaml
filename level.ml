open Object


(* Represents the game map *)
type map = {
  mutable width: float;
  mutable height: float;
  mutable objects: collidable list
}

(* x,y coordinate vector *)

(* Gets a list of objects in a certain position in the grid *)
let get xy = []

(* Puts an object in a certain position in the grid *)
let put xy = []