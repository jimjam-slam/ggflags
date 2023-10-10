# ggflags 0.0.4

- Some more internal rejiggering to ensure that you can use ggflags without explicitly loading the package
  - If you're not loading the package and need to use it to plot things, prefix the package functions with `ggflags::` (eg. `ggflags::geom_flag`).
  - If you need to access the internal flag list, either load it into the global environment as `lflags` using `data(lflags)` or access it prefixed with `ggflags::lflags`

# ggflags 0.0.3

- Adds Rémi Thériault as an author and contributor!
- Package now complies with R CMD CHECK, paving the way for possible CRAN submission in the future!
- The package will also promptly be available on https://jimjam-slam.r-universe.dev

# 0.0.2

- Fork from ellisp/ggflags
- Switches from the use of rectangular PNG flags in ellisp/ggflags to circular SVG flags. This makes them much better for plotting at high resolution, and especially for use in bubble plots.
