open Actors

type viewport = {
  pos: Actors.xy;
  v_dim: Actors.xy;
  m_dim: Actors.xy;
}

val make : float*float -> float*float -> viewport

val calc_viewport_point : float -> float -> float -> float

val in_viewport : viewport -> Actors.xy -> bool

val out_of_viewport_below : viewport -> float -> bool

val coord_to_viewport : viewport -> Actors.xy -> Actors.xy

val update : viewport -> Actors.xy -> viewport
