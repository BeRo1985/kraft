> [!IMPORTANT]
> The primary repository has moved to [git.rosseaux.net/BeRo1985/kraft](https://git.rosseaux.net/BeRo1985/kraft).
> This GitHub repository is kept up-to-date via push mirroring.

## Kraft Physics Engine

Kraft Physics Engine is an open source Object Pascal physics engine library that can be used in 3D games.

Author : Benjamin 'BeRo' Rosseaux

Compatible with: Delphi >= 10.2 and FreePascal >= 2.6.2 (with almost all FPC-supported targets including Android and iOS) 

## Support me

[Support me at Patreon](https://www.patreon.com/bero)

## Features

Kraft Physics Engine has the following features:

- Rigid body dynamics
- Discrete collision detection
- Optional additional continuous collision detection with Motion Clamping or with Box2D-style time of impact sub stepping, either with Bilateral Advancement or Conservative Advancement.  
- Optional additional support for speculative contacts as faster and more inaccurate fake continuous collision detection mode.
- Full one-shot contact manifold collision shapes (spheres, capsules, convex hulls, boxes, planes, signed distance fields, and for static geometries also triangle meshes) based on combinations of for-warm-start-simplex-caching-able GJK, Gauss-Map optimized Clipping-SAT and implicit collision algorithms.
- Signed distance field collision shapes: user-implementable implicit surfaces (analytic or heightfield-like, where the local origin may even lie outside the solid), with a dedicated sampling narrow phase (point-feature shapes sample their features against the field, signed distance field bodies sample surface-projected pseudo features against planes and other fields) producing full one-shot manifolds with warm-start-stable per-sample feature ids, plus sphere-tracing ray and sphere casts
- Optional MPR-based (Minkowski Portal Refinement) incremental persistent contact manifold work mode (see TKraft.PersistentContactManifold boolean), but its usage isn't recommended, because the full one-shot contact manifold work mode is faster, more robust and more tested. It is implemented only as comparison reference (for example for debugging purposes), and for more reasons against incremental persistent contact manifold real usage, see http://media.steampowered.com/apps/valve/2015/DirkGregorius_Contacts.pdf .
- Multiple collision shapes per rigid body without the need for a compound shape
- Broadphase collision detection with a dynamic AABB tree
- Fast mid phase for static triangle mesh geometries
- Runtime-selectable internal/ghost edge handling for triangle meshes (none, contact-normal clamping into the neighbour-face fan, or feature-ownership arbitration over the shared mesh contact pair), so convex bodies do not snag on the shared inner edges of tessellated surfaces
- Runtime-selectable mesh contact manifold mode (per-triangle, or clustered per coplanar-ish surface patch into one representative manifold with central friction per patch instead of per triangle)
- Optional contact recycling: contact pairs whose relative body pose stayed within a configurable distance since their last real narrow phase run skip the narrow phase and keep their result (with a conservative-advancement bound over the relative translation plus rotation, and a GJK-proven separation bound for the not-touching case, so a beginning touch can never be missed)
- The choice of either Box2D-style post position correction/projection or alternately baumgarte stabilization
  - Post position correction/projection is slower but it's more precise, so it's also the default enabled setting
  - Baumgarte stabilization (which is also the old-school way) is faster but it can be inaccurate in some situations
- Narrowphase collision detection
- Collision response and friction (sequential impulses with post projection), with optional per-shape rolling resistance
- The choice of two velocity solvers, selectable at runtime: the classic sequential impulse solver or a substepped TGS soft-constraint solver with soft contacts and joints, where joints can run either through the classic solve path per step or substep or through native per-substep soft implementations, selectable per world and overridable per joint
- Central friction model in the TGS soft solver (two-axis tangential friction, twist friction and rolling resistance anchored at the contact centroid, one shared set per manifold instead of per contact point), with cross-frame warm starting
- Joint constraints (Grab, Distance, Rope, World Plane Distance, Ball Socket with optional swing/twist limits, Fixed, Hinge with limits and motor, Slider with limits and motor, Pulley, Parallel, Motor, Wheel with suspension spring, spin motor and steering, and a freely configurable 6-DOF joint with per-degree-of-freedom locked/free/limited modes, an elliptical swing cone, optional soft limits per limit group and a full set of position and velocity drives)
- Ready-to-use vehicle add-on units (KraftArcadeCarPhysics and KraftRayCastVehicle)
- Collision filtering with groups
- Ray casting and sphere casting (ray "with" spherical thickness)
- Sleeping of inactive rigid bodies
- Island-based multithreading, and optionally (compile-time define KraftConstraintGraphColoring) a persistent constraint-graph-coloring parallel solver architecture with a lock-free block-claiming solver stage pipeline, runtime-switchable between island-granular and colored parallelization
- Optional wide (four-lane SoA) TGS soft contact solver with hand-written x86-64 SSE assembler kernels (warm start, velocity solve and restitution), bit-identical to the scalar path, with a pure Pascal fallback for other CPUs and for double precision
- Cross-run-deterministic broadphase pair ordering, keyed on stable shape ids rather than on the shape pointers
- Optional large-world double precision world positions (compile-time define KraftDoublePositions), for larger but not overly gigantic worlds: only the absolute world positions (the rigid body sweep center of mass) become double while all other math and the SSE solver kernels stay single precision and operate relative to a base offset, with double precision far-from-origin query overloads; unlike the full double precision build this keeps the fast single precision solver
- It can be used also for 1D and 2D physics, and not only for 3D physics
- SIMD optimizations for x86-32 and x86-64

## Future possibly possible features

- Linear casting
- Cloths / Soft body

## License

The Kraft Physics Engine is released under the open-source [ZLib license](http://opensource.org/licenses/zlib).

## PasMP

For to use Kraft with PasMP, you must set a project global define called KraftPasMP (or use -dKraftPasMP as compiler command line parameter)

## Compile-time defines

Most features are switchable at runtime through properties, but a few are gated behind project-global compile-time defines (set them in the project options or pass them as -d\<Define\> on the compiler command line):

- **KraftPasMP** - use the PasMP job system instead of the built-in job manager (see above)
- **KraftConstraintGraphColoring** - compile in the persistent constraint-graph-coloring parallel solver and its block-claiming solver stage pipeline; the parallelization architecture is then selected at runtime via TKraft.SolverParallelMode (defaults to the island-granular mode)
- **KraftDoublePositions** - large-world double precision world positions, the middle step for larger but not overly gigantic worlds: only the absolute world positions (the rigid body sweep center of mass) become double while all other math and the SSE solver kernels stay single precision and operate relative to a base offset; this changes the ABI of TKraftSweep and adds double precision far-from-origin query overloads, and is byte for byte the classic path when undefined
- **KraftUseDouble** - build the whole engine in double precision (TKraftScalar becomes Double; implies NonSIMD), for truly gigantic worlds where even the broadphase / BVH trees and the collision and solver math need 64-bit floating point (for example large open-world or open-space space games)

## Sandbox

The sandbox subdirectory contains a demo application with many test scenes, among them a joint gallery which shows every joint type in action, two ragdoll scenes (one built from ball sockets and hinges, one built entirely from the 6-DOF joint), vehicles, stacking tests and two signed distance field scenes (a dynamic rounded-box signed distance field body dropping onto a plane, and a sphere rolling on a signed distance field heightfield terrain).

## Documentation

Later . . .

## Branches

The "master" branch contains the development state of the physics engine, which is frequently updated and can be quite unstable. The "release" branch contain the last stable released version and some possible bug fixes, which is then the most stable version. 

## Issues

If you find any issue with the library, you can report it on the issue tracker [here](https://github.com/BeRo1985/kraft/issues).
