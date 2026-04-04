[MariOCaml](https://mahsu.github.io/mariocaml/)
=============
MariOCaml is an HTML 5 canvas web-browser implementation of the [Super Mario Bros](https://en.wikipedia.org/wiki/Super_Mario_Bros.) platformer written exclusively in [Ocaml](https://www.ocaml.org/).

![Gameplay preview](https://github.com/mahsu/MariOCaml/raw/master/screenshots/preview.gif)

[Try it out online!](https://mahsu.github.io/mariocaml/)

## Key Features
* 2D Mario platformer that emulates the platformer mechanics of Super Mario Bros.
* Web-based playable implementation with graphics (Ocaml transpiled to Javascript).
* Procedural level generator that randomly generates levels.

## Description
MariOcaml is an OCaml implementation of Super Mario Bros. The game includes several basic enemies, blocks, and items, with particle support. The underlying system follows a director model, with the director maintaining state based on game objects and tiles, including Mario, items, and obstacles. The implementation of enemies and items is designed to be easily extensible. Furthermore, MariOcaml supports a game-over and game-win states, as well as score and coin counts, emulating several of the core mechanics of Mario.

The design follows a clean, modular, and extensible system to ensure that more features can easily be added. Levels are procedurally generated (pseudo-randomly), algorithmically creating an environment of textures and objects that are interactable. The procedural generation generally generates a playable map, but due to the nature of random generation, maps may not be the most aesthetically appealing. Additionally, the game is interactive and graphical using `js_of_ocaml`, which enables cross-compiliation of Ocaml to javascript, allowing MariOcaml to be run in any modern web browser.

The underlying procedural generation utilizes a grid system to generate objects, taking account parameters such as proximity and location. During game-play, the director maintains the game state, including the location and status of both items, enemies, and the character.

## Modules
* **Director** - The heart of the game, executes the main game loop and performs updates on the map, canvas, and objects. Also executes the side effectual collision detection (files [director.ml](director.ml) and [director.mli](director.mli)).
* **Viewport** - Represents a slice of the game map which is currently in view and rendered on the canvas (files [viewport.ml](viewport.ml) and [viewport.mli](viewport.mli)).
* **Procedural Generator** - Generates the game level in a game map to be played (files [procedural.ml](procedural.ml) and [procedural.mli](procedural.mli)).
* **Actors** - Represents the characters, items, and obstacles in the Mario game world (files [actors.ml](actors.ml) and [actors.mli](actors.mli)).
* **Sprite** - The visual representation of an object to be drawn on the canvas. Contains the ability to create sprites from a template configuration based on the type of actor (files [sprite.ml](sprite.ml) and [sprite.mli](sprite.mli)).
* **Object** - The abstract representation of a specific actor on the game map. Contains the ability to create objects from a template configuration based on the type of actor (files [object.ml](object.ml) and [object.mli](object.mli)).
* **Particle** - Represents a non-collidable visually oriented object on the map. This includes destruction and creation effects (files [particle.ml](particle.ml) and [particle.mli](particle.mli)).
* **Draw** - Contains methods which update the canvas (files [draw.ml](draw.ml) and [draw.mli](draw.mli)).

## Building the Project

1. First, ensure that [`dune`](https://github.com/ocaml/dune) and [`js_of_ocaml`](https://github.com/ocsigen/js_of_ocaml) (>= 2.6) and its dependencies are installed by running:

        opam install dune js_of_ocaml

2. (If you don't have `opam` installed, [see the official instructions](https://opam.ocaml.org/doc/Install.html))

3. With `dune` installed, run:

        make all

to compile the binaries to the `_build` folder and run the `js_of_ocaml` cross-compiler.

4. Open `http://localhost:8000` in a web-browser to run the game!

## Contributing
Pull requests fixing bugs, adding functionality, or improving organization are welcome!

## Authors
MariOCaml was originally conceived for Cornell University's Fall 2015 CS3110 final project.
* Matthew Hsu ([@mahsu](https://github.com/mahsu))
* Ashley Xue ([@ashleyxue529](https://github.com/ashleyxue529))
* Liam Bui ([@LiamBui](https://github.com/liambui))
