1.0.3.2 2023-09-21
==================
- src/HandBrake/Paths should be generated at build time

1.0.3.1 2023-07-10
==================
- upgrade dependencies, particularly stdmain to 1.6.1.1

1.0.3.0 2022-01-26
==================
- replace --no-audio-copy with --aencoder
- add 'd' profile (dead video)

1.0.2.1 2021-10-13
==================
- upgrade dependencies

1.0.2.0 2021-09-16
==================
- Better output file/dir checking, factored into stdmain

1.0.1.1 2021-08-23
==================
- Fix chapter handling, so (a) -c is optional again (defaulting to "all
  chapters" and (b) using a single chapter (-c 6) really works

1.0.1.0 2021-08-21
==================
- Properly handle chapter ranges (e.g., 6-7); only single ranges (or single
  chapters) are handled, as that's all that HandBrakeCLI will do.
- Set default quality to 26.0

1.0.0.0 2021-08-17
==================
- hb executable capable of encoding files.  No support for specification via
  input file nor for scan-parsing.
