open Object
open Actors

type obj_coord

val init : unit -> unit

val avoid_overlap : obj_coord list -> obj_coord list -> obj_coord list

val choose_sblock_typ : int -> block_typ

val convert_to_block_obj : obj_coord list -> Dom_html.canvasRenderingContext2D Js.t -> collidable list

(* Procedurally generates a new map of default size*)
val generate : float -> float -> Dom_html.canvasRenderingContext2D Js.t -> collidable * collidable list

val generate_enemies : float -> float -> float -> float -> obj_coord list -> obj_coord list

val generate_helper : float -> float -> float -> float -> Dom_html.canvasRenderingContext2D Js.t -> collidable list

val mem_loc : (float*float) -> obj_coord list -> bool

val convert_list : obj_coord list -> obj_coord list

val choose_block_pattern : float -> float -> float -> float -> int -> obj_coord list

val generate_block_locs : float -> float -> float -> float -> obj_coord list-> obj_coord list

val generate_ground : float -> float -> float -> obj_coord list -> obj_coord list
