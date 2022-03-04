
flagGrob <- function(x, y, country, size=1, alpha=1){
  # grob(x=x, y=y, country=country, size=size, cl = "flag")

  message(">>> Making flag for country = ",
    paste(country, collapse = " AND "))
  browser()
  
  flag_tree <- gTree(cl = "flag")

  flag_pics <- lapply(seq_along(country),
    function(ii) {
      # TODO - good place to validate the flag code here!
      grImport2::pictureGrob(
        picture = .flaglist[[country[ii]]],
        x = x[ii], y = y[ii],
        width = size[ii] * unit(1, "mm"),
        height = size[ii] * unit(1, "mm"),
        distort = FALSE)
    })

  setChildren(flag_tree, do.call(gList, flag_pics))
}

# #' @export
# makeContent.flag <- function(x) {
  # flag_pics <- lapply(seq_along(x$country),
  #   function(ii) {
  #     # TODO - good place to validate the flag code here!
  #     grImport2::pictureGrob(
  #       picture = .flaglist[[x$country[[ii]]]],
  #       x = x$x[ii], y = x$y[ii],
  #       width = x$size[ii] * unit(1, "mm"),
  #       height = x$size[ii] * unit(1, "mm"),
  #       distort = FALSE)
  #   })
#     browser('MAKECONTENT FLAG')
#     # NOTE - this happens after ggiraph does its work, so i think
#     # interactive attributes aren't applied (the content is essentially empty at the time they're to be applied)
#   setChildren(x, do.call(gList, flag_pics))
# }

#' @export
scale_country <- function(..., guide = "legend") {
  sc <- discrete_scale("country", "identity", scales::identity_pal(), ..., guide = guide,
                       super = ScaleDiscreteIdentity)

  sc
}

GeomFlag <- ggproto("GeomFlag", Geom,
                    required_aes = c("x", "y", "country"),
                    default_aes = aes(size = 5, country="nz"),
                    
                    draw_key = function (data, params, size) 
                    {
                      flagGrob(0.5,0.5, country=data$country,  size=data$size)
                    },
                    
                    draw_panel = function(data, panel_params, coord) {
                      coords <- coord$transform(data, panel_params)     
                      flagGrob(coords$x, coords$y, coords$country, coords$size)
                    }
)


#' geom_flag
#'
#' @param mapping 
#' @param data 
#' @param stat 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#' @param ... 
#'
#' @examples 
#' data(lflags)
#' set.seed(1234)
#' d <- data.frame(x=rnorm(10), y=rnorm(10), 
#'                 country=sample(c("ar","fr"), 10, TRUE), 
#'                 stringsAsFactors = FALSE)
#' ggplot(d, aes(x=x, y=y, country=country, size=x)) + 
#'   geom_flag() + 
#'   scale_country()
#' @importFrom grid unit gTree gList makeContent setChildren
#' @importFrom grImport2 pictureGrob
#' @export
geom_flag <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, ...) {
  layer(
    geom = GeomFlag, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

