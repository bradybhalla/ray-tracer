# ray-tracer

A CPU ray tracer I made to learn more about ray tracing and learn more about
OCaml. This program supports both Whitted tracing and random walk tracing.
Whitted tracing is (mostly) deterministic and gets good results quicker, but is
not as powerful as the stochastic random walk tracer.

Some features supported by this ray tracer are:
- basic shapes (spheres, planes, triangles)
- creating meshes from ".obj" files
- rendering reflective and refractive materials
- rendering materials with arbitrary BSDFs
- applying linear transforms to shapes

## Sample images
TODO GENERATE

## Usage
- `make render` -- render a single image from "single_render.ml"
- `make animation` -- render an animation from "animation.ml"
- `make profile` (only macOS) -- render single image while running profiler

## Setup
Make sure `opam` and `ffmpeg` are installed. Run `opam install dune domainslib`.

## TODOs
- [ ] Make a better stochastic ray tracer using importance sampling.
- [ ] Make scene definition files instead of recompiling to change/modify scenes.
- [ ] Start multiple processes manually instead of using domainslib (for some
      reason the GC takes up most of the time when I increase the number of
      domains)
