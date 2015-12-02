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
  speed: float;
}
type obj = {
  params: obj_params;
  pos: xy;
  vel: xy;
  id: int;
  mutable jumping: bool;
  mutable grounded: bool;
  mutable dir: Actors.dir_1d;
  mutable invuln: int;
  mutable kill: bool;
}

type collidable =
  | Player of sprite * obj
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

val is_player : collidable -> bool

(* Destroys the object, returning a list of destruction effect objects *)
val kill : obj -> noncollidable list

val process_obj : collidable -> Dom_html.canvasRenderingContext2D Js.t -> unit

val update_player : obj -> Actors.controls list -> Dom_html.canvasRenderingContext2D Js.t -> sprite option

(* Checks whether a collision occured between two objects, returning the
 * direction of the collision if one occurred. *)
val check_collision : collidable -> collidable -> Actors.dir_2d option

val process_collision : Actors.dir_2d -> collidable -> collidable -> unit
