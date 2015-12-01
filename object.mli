open Sprite
open Actors

type xy = {
  mutable x: float;
  mutable y: float;
}

type aabb = {
  center: xy;
  half: xy;
}

type obj_params = {
  has_gravity: bool;
  max_speed: bool;
}
type obj = {
  params: obj_params;
  pos: xy;
  speed: float;
  vel: xy;
  mutable jumping: bool;
  mutable grounded: bool;
  mutable dir: direction;
  mutable invuln: int;
}

type collidable =
  | Player of player_typ * sprite * obj
  | Enemy of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj

type noncollidable =
  | Scenery of sprite * obj


(* Returns the sprite associated with the object *)
val get_sprite : collidable -> Sprite.sprite

val get_obj : collidable -> obj
(* Creates a new object with a given
 * actor type on the the canvas at a given position *)
val spawn : Actors.spawn_typ  -> Dom_html.canvasRenderingContext2D Js.t
          -> float*float -> collidable 

(* Destroys the object, returning a list of destruction effect objects *)
val kill : obj -> noncollidable list


val update_player -> obj -> Actor.1d_dir -> sprite option

(* Updates the velocity of the object *)
val update_vel : obj -> obj

(* Updates the position of the object *)
val update_pos : obj -> obj

(* Checks whether a collision occured between two objects, returning the
 * direction of the collision if one occurred. *)
val check_collision : collidable -> collidable -> Actors.2d_dir option

