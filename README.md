[MariOCaml](https://mahsu.github.io/mariocaml/)
=============
MariOCaml is an HTML 5 canvas web-browser implementation of the Super Mario Bros platformer written exclusively in Ocaml.

![Alt Text](https://github.com/mahsu/MariOCaml/raw/master/screenshots/preview.gif)

##Key Features
* 2D Mario platformer that emulates the platformer mechanics of Super Mario Bros.
* Web-based playable implementation with graphics (Ocaml transpiled to javascript).
* Procedural level generator that randomly generates levels.

##Description
MariOcaml is an OCaml implementation of Super Mario Bros. The game includes several basic enemies, blocks, and items, with particle support. The underlying system follows a director model, with the director maintaining state state based on game objects and tiles, including Mario, items, and obstacles. The implementation of enemies and items is designed to be easily extensible. Furthermore, MariOcaml supports a game-over and game-win states, as well as score and coin counts, emulating several of the core mechanics of Mario.

The design follows a clean, modular, and extensible system to ensure that more features can easily be added. Levels are procedurally generated (pseudo-randomly), algorithmically creating an environment of textures and objects that are interactable. The procedural generation generally generates a playable map, but due to the nature of random generation, maps may not be the most aesthetically appealing. Additionally, the game is interactive and graphical using js\_of\_ocaml, which enables cross-compiliation of Ocaml to javascript, allowing MariOcaml to be run in any modern web browser.

The underlying procedural generation utilizes a grid system to generate objects, taking account parameters such as proximity and location. During game-play, the director maintains the game state, including the location and status of both items, enemies, and the character.

##Modules
* **Director** - The heart of the game, executes the main game loop and performs updates on the map, canvas, and objects. Also executes the side effectual collision detection.
* **Viewport** - Represents a slice of the game map which is currently in view and rendered on the canvas.
* **Procedural Generator** - Generates the game level in a game map to be played.
* **Actors** - Represents the characters, items, and obstacles in the Mario game world.
* **Sprite** - The visual representation of an object to be drawn on the canvas. Contains the ability to create sprites from a template configuration based on the type of actor.
* **Object** - The abstract representation of a specific actor on the game map. Contains the ability to create objects from a template configuration based on the type of actor.
* **Particle** - Represents a non-collidable visually oriented object on the map. This includes destruction and creation effects.
* **Draw** - Contains methods which update the canvas.

##Building the Project
1. First, ensure that js\_of\_ocaml and its dependencies are installed by running:

        opam install js_of_ocaml
2. With the cs3110 tool installed, run:

        cs3110 compile -p js_of_ocaml -p js_of_ocaml.syntax main.ml
to compile the binaries to the `_build` folder.

3. To cross-compile to javascript, run

        js_of_ocaml _build/main.d.byte

4. Open `index.html` to run!

Alternatively, to build the files, run `./build.sh`.