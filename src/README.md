# src

All source code for vibe-knight is located here. Each module has its own file.

## World Structure

The world in Vibe Knight is a large, grid-based map designed for classic top-down adventure gameplay, inspired by games like Zelda 3.

- **Tiles:** The smallest unit, each tile is 16x16 pixels.
- **Screen:** The visible play area is 640x480 pixels, or 40x30 tiles.
- **Scene:** A scene is a 5x5 screen area, making each scene 200x150 tiles. Scenes are the building blocks of the world and can be loaded, generated, or rendered independently.
- **World:** The world is a 6x6 grid of scenes, for a total of 36 scenes. This makes the full world 1200x900 tiles (960x720 pixels).

### Navigation
- The player moves within a scene, and when reaching the edge, transitions to the adjacent scene.
- Scene and tile coordinates are tracked for both the player and world logic, allowing for seamless movement and large world exploration.

### Data Structures
- `scene`: Holds a 2D array of tiles and its position in the world grid.
- `world`: Holds a 2D array of scenes.
- Player position is tracked by scene and tile coordinates for efficient navigation and rendering.

This structure allows for efficient streaming, procedural generation, and classic adventure game design patterns.
