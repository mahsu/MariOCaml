open Sprite
open Actors

type xy = {
  mutable x: float;
  mutable y: float;
}
type direction = | Up | Down | Right | Left

type aabb = {
  center: xy;
  half: xy;
}

type obj = {
  sprite: sprite;
  pos: xy;
  speed: float;
  vel: xy;
  jumping: bool;
  grounded: bool;
  dir: direction;
  inv: int;
}

type collidable =
  | Player of player_typ * sprite * obj
  | Monster of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj

type noncollidable =
  | Scenery of sprite * obj

(* Returns the sprite associated with the object *)
val get_sprite : collidable -> Sprite.sprite

(* Creates a new object with a given
 * actor type on the the canvas at a given position *)
val spawn : Actors.spawn_typ  -> Dom_html.canvasRenderingContext2D Js.t
          -> float*float -> obj 

(* Destroys the object, returning a list of destruction effect objects *)
val kill : obj -> noncollidable list

(* Updates the velocity of the object *)
val update_vel : obj -> obj

(* Updates the position of the object *)
val update_pos : obj -> obj

(* Checks whether a collision occured between two objects, returning the
 * direction of the collision if one occurred. *)
val check_collision : collidable -> collidable -> direction option

