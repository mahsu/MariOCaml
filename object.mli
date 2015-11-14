open Sprite
open Actors

(* A xy coordinate vector *)
type xy = float * float


type direction = | Up | Down | Right | Left

(* Represents an object in the game *)
type obj = {
  sprite: sprite;
  pos: xy;
  speed: float;
  vel: xy;
  jumping: bool;
  grounded: bool;
  dir: direction;
}

(* Represents a collidable object *)
type collidable =
  | Player of actor * sprite * obj
  | Monster of actor * sprite * obj
  | Item of actor * sprite * obj
  | Block of actor * sprite * obj

(* Represents a noncollidable object *)
type noncollidable =
  | Dead of dead_type * sprite
  | Scenery of sprite * obj

(* Returns the sprite associated with the object *)
val get_sprite : collidable -> Sprite.sprite

(* Creates a new object with a given
 * actor type on the the canvas at a given position *)
val spawn : Character.actor  -> Dom_html.canvasRenderingContext2D Js.t
          -> xy -> collidable

(* Destroys the object, returning a list of destruction effect objects *)
val kill : obj -> noncollidable list option

(* Updates the velocity of the object *)
val update_vel : obj -> obj

(* Updates the position of the object *)
val update_pos : obj -> obj

(* Checks whether a collision occured between two objects, returning the
 * direction of the collision if one occurred. *)
val check_collision : collidable -> collidable -> direction option

