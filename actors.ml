type dir_1d = | Left | Right
type dir_2d = | North | South | East | West

type controls =
  | CLeft
  | CRight
  | CUp
  | CDown

type pl_typ =
  | BigM
  | SmallM

type item_typ =
  | Mushroom
  | FireFlower
  | Star
  | Coin

type enemy_typ =
  | Goomba
  | GKoopa
  | RKoopa
  | GKoopaShell
  | RKoopaShell

type block_typ =
  | QBlock of item_typ
  | QBlockUsed
  | Brick
  | UnBBlock

type player_typ =
  | Standing
  | Jumping
  | Running
  | Crouching

type spawn_typ =
  | SPlayer of player_typ
  | SEnemy of enemy_typ
  | SItem of item_typ
  | SBlock of block_typ

