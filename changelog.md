1.0.1.0 2021-08-21
==================
- Properly handle chapter ranges (e.g., 6-7); only single ranges (or single
  chapters) are handled, as that's all that HandBrakeCLI will do.
- Set default quality to 26.0

1.0.0.0 2021-08-17
==================
- hb executable capable of encoding files.  No support for specification via
  input file nor for scan-parsing.
