#' @title Create interactive flags
#'
#' @description
#' An interactive version of [geom_flag()], designed to work with the `ggiraph`
#' package.
#'
#'
#' @param ... arguments passed to base function,
#' plus any of the [ggiraph:::interactive_parameters()].
#' @examples
#' # add interactive flags to a ggplot -------
#' @seealso [ggiraph::girafe()]
#' @export
geom_flag_interactive <- function(...)
  ggiraph:::layer_interactive(geom_flag, ...)

#' @format NULL
#' @usage NULL
#' @export
GeomInteractiveFlag <- ggproto(
  "GeomInteractiveFlag",
  GeomFlag,
  default_aes = ggiraph:::add_default_interactive_aes(GeomFlag),
  parameters = ggiraph:::interactive_geom_parameters,
  draw_key = ggiraph:::interactive_geom_draw_key,
  draw_panel = function(self, data, panel_params, coord, ..., .ipar = IPAR_NAMES) {
    zz <- GeomFlag$draw_panel(data, panel_params, coord, ...)
    coords <- coord$transform(data, panel_params)
    browser('BEFORE ATTRIBUTE INSERTION')
    # may need to loop over the flagGrob list manually!
    for (i in seq_along(zz$children)) {
      zz$children[[i]] <- ggiraph:::do_add_interactive_attrs(
        zz, coords[i, , drop = FALSE], ipar = .ipar)
      browser(paste('AFTER ATTRIBUTE INSERTION ', i))
    }
    browser('ALL DONE')
    zz
  }
)