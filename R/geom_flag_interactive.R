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
    message('ABOUT TO ADD INTERACTIVE ATTRS TO FLAGS')
    # browser()
    for (i in seq_along(zz$children)) {
      zz$children[[i]] <- ggiraph:::do_add_interactive_attrs(zz$children[[i]],
        coords[i, , drop = FALSE], ipar = .ipar,
        cl = "interactive_flags_grob"
        )
    }
    # ggiraph:::do_add_interactive_attrs(zz, coords, ipar = .ipar)
    return(zz)
    message('HAVE ADDED INTERACTIVE ATTRS TO FLAGS')
  }
)

#' @title Create interactive flags grob
#'
#' @description
#' The grob is based on [flagsGrob()].
#' See the documentation for that function for more details.
#'
#' @param ... arguments passed to base function,
#' plus any of the [interactive_parameters()].
#' @return An interactive grob object.
#' @seealso [girafe()]
#' @export
interactive_flags_grob <- function(...) {
  ggiraph:::grob_interactive(grid::flagsGrob, ...)
}

# shapes_with_lines <- c(3, 4, 7, 8, 9, 10, 11, 12, 13, 14)

#' @export
drawDetails.interactive_flags_grob <- function(x, recording) {
  # shapes <- unique(x$pch)
  # shape_index <- shapes %in% shapes_with_lines
  # if (length(shapes) > 1 && any(shape_index)) {
  #   # if some shapes contain lines, split the grob to multiple ones:
  #   # one grob for all flags without these shapes and then
  #   # a grob for each different shape
  #   shapes_with_lines_present <- intersect(shapes, shapes_with_lines)
  #   grobs <- lapply(c(NA, shapes_with_lines_present), function(shape) {
  #     partialFlagsGrob(x, pch = shape)
  #   })
  # } else {
  #   grobs <- list(x)
  # }

  # i *think* this calls the nextMethod of each flag in the geom... but instead
  # of using NextMethod, it manually removes a class and galls the generic
  # again?
  grobs <- list(x)
  purrr::walk(grobs, function(x) {
    ggiraph:::dsvg_tracer_on()
    class(x) <- class(x)[-1]
    grid::drawDetails(x, recording)
    ids <- ggiraph:::dsvg_tracer_off()
    ggiraph:::interactive_attr_toxml(x = x, ids = ids)
  })
  invisible()
}

# partialFlagsGrob <- function(gr, pch = NA) {
#   if (is.na(pch)) {
#     index <- !(gr$pch %in% shapes_with_lines)
#   } else {
#     index <- gr$pch %in% pch
#   }
#   if (!any(index)) {
#     return(zeroGrob())
#   }
#   gr$name <- paste0(gr$name, ".", pch)
#   for (m in c("x", "y", "pch", "size")) {
#     if (length(gr[[m]]) > 1) {
#       gr[[m]] <- gr[[m]][index]
#     }
#   }
#   for (m in c("col", "fill", "fontsize", "lwd")) {
#     if (length(gr$gp[[m]]) > 1) {
#       gr$gp[[m]] <- gr$gp[[m]][index]
#     }
#   }
#   ipar <- get_ipar(gr)
#   for (m in ipar) {
#     if (length(gr$.interactive[[m]]) > 1) {
#       gr$.interactive[[m]] <- gr$.interactive[[m]][index]
#     }
#   }
#   gr
# }