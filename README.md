## Kraft Physics Engine

Kraft Physics Engine is an open source Object Pascal physics engine library that can be used in 3D games.

Author : Benjamin 'BeRo' Rosseaux

## Features

Kraft Physics Engine has the following features :

- Rigid body dynamics
- Discrete collision detection
- Optional additional continuous collision detection, either with Bilateral Advancement or Conservative Advancement  
- Full one-shot contact manifold collision shapes (spheres, capsules, convex hulls, boxes, and for static geometries also triangle meshs)
- Multiple collision shapes per rigid body without the need for a compound shape
- Broadphase collision detection with a dynamic AABB tree
- Fast mid phase for static triangle mesh geometries
- Narrowphase collision detection
- Collision response and friction (Sequential impulses with post projection)
- Joint constraints (Grab, Distance, Rope, Ball Socket, Hinge, Slider, Fixed)
- Collision filtering with groups
- Ray casting
- Sleeping of inactive rigid bodies
- Island-based multithreading
- SIMD optimizations for x86-32 (later also for x86-64)

## Future possibly possible features

- Sphere casting / Linear casting
- Raycast vehicles      
- More joint constraint types (for example cone twist, universal and/or general 6D0F)
- Cloths / Soft body

## License

The Kraft Physics Engine is released under the open-source [ZLib license](http://opensource.org/licenses/zlib).

## Documentation

Later . . .

## Branches

The "master" branch contains the development state of the physics engine, which is frequently updated and can be quite unstable. The "release" branch contain the last stable released version and some possible bug fixes, which is then the most stable version. 

## Issues

If you find any issue with the library, you can report it on the issue tracker [here](https://github.com/BeRo1985/kraft/issues).
