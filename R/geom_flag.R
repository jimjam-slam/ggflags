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
geom_flag <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomFlag, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
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
  draw_key = function(data, params, size) {
    flagGrob(0.5, 0.5, country = data$country, size = data$size)
  },
  draw_group = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    flagGrob(coords$x, coords$y, coords$country, coords$size)
  }
)

#' @noRd
flagGrob <- function(x, y, country, size = 1, alpha = 1) {
  grid::gTree(x = x, y = y, country = country, size = size, cl = "flag")
}

#' @noRd
#' @exportS3Method grid::makeContent
makeContent.flag <- function(x) {
  flag_pics <- lapply(
    seq_along(x$country),
    function(ii) {
      grImport2::pictureGrob(
        picture = ggflags::lflags[[x$country[[ii]]]],
        x = x$x[ii], y = x$y[ii],
        width = x$size[ii] * grid::unit(1, "mm"),
        height = x$size[ii] * grid::unit(1, "mm"),
        distort = FALSE
      )
    }
  )
  grid::setChildren(x, do.call(grid::gList, flag_pics))
}
