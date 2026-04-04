type viewport = {
  pos : Actors.xy; (* Absolute position of viewport relative to map *)
  v_dim : Actors.xy; (* Dimensions of viewport *)
  m_dim : Actors.xy; (* Dimensions of map *)
}

(* Makes a new viewport of viewport dimensions and map dimensions*)
val make : float * float -> float * float -> viewport

(* Calculates the viewport origin point *)
val calc_viewport_point : float -> float -> float -> float

(* Whether the supplied position is outside of the viewport *)
val in_viewport : viewport -> Actors.xy -> bool

(* Whether the supplied position is below the viewport *)
val out_of_viewport_below : viewport -> float -> bool

(* Converts absolute coordinates to viewport coodinates *)
val coord_to_viewport : viewport -> Actors.xy -> Actors.xy

(* Update the viewport *)
val update : viewport -> Actors.xy -> viewport
