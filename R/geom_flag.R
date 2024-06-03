#' @title flag geom for ggplot2
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @examples
#' data(lflags)
#' set.seed(1234)
#' d <- data.frame(
#'   x = rnorm(10), y = rnorm(10),
#'   country = sample(c("ar", "fr"), 10, TRUE),
#'   stringsAsFactors = FALSE
#' )
#' ggplot2::ggplot(d, ggplot2::aes(x = x, y = y, country = country, size = x)) +
#'   geom_flag() +
#'   scale_country()
#' @export
geom_flag <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomFlag, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @title Draw flag key
#' @param country A country whose fla gwill be used for the legend key
#' @return A function that draws the flag key
#' @export
draw_key_flag <- function(country = "nz") {
  function(data, params, size) {
    flagGrob(0.5, 0.5, country = country, size = data$size)
  }
}

#' @title scale countries
#' @param guide guide
#' @param ... ...
#' @export
scale_country <- function(..., guide = "legend") {
  sc <- ggplot2::discrete_scale("country", "identity", scales::identity_pal(), ...,
    guide = guide,
    super = ggplot2::ScaleDiscreteIdentity
  )
  sc
}

#' @noRd
GeomFlag <- ggplot2::ggproto("GeomFlag", ggplot2::Geom,
  required_aes = c("x", "y", "country"),
  default_aes = ggplot2::aes(size = 5, country = "nz"),
  draw_key = draw_key_flag("nz"),
  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)

    make_flag_grob <- function(i) {
      flagGrob(
        coords$x[i],
        coords$y[i],
        coords$country[i],
        coords$size[i]
      )
    }

    # build a flag for each coord row
    svg_grobs <- lapply(seq_len(nrow(coords)), make_flag_grob)
    flag_tree <- do.call(grid::grobTree, svg_grobs)
    return(flag_tree)
  }
)

#' @noRd
flagGrob <- function(x, y, country, size = 1, alpha = 1) {
  flag_size <- size * grid::unit(1, "mm")
  flag_grob <- ggsvg::svg_to_rasterGrob(
    svg_text = ggflags::lflags[[country]],
    width = grid::convertUnit(flag_size, "pt") * 4,
    height = grid::convertUnit(flag_size, "pt") * 4,
    just = "centre",
    vp = grid::viewport(
      x = x, y = y,
      # TODO - currently adjusting this to scale to taste, but
      # it feels like it's nonlinear...
      width = flag_size * 0.75, height = flag_size * 0.75)
    )
  return(flag_grob)
}
