#' A 'ggplot2' plot to draw a circle with absolute radius
#'
#' @section Aesthetics:
#'
#' - x (required)
#' - y (required)
#' - alpha
#' - colour
#' - fill
#' - linetype
#' - linewidth
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for `ggplot2::geom_text()`.
#' @param radius Radius of the circle. A `grid::unit()` object. Defaults to 50
#' mm.
#'
#' @export
geom_abs_circle <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  radius = grid::unit(50, "mm"),
  ...
) {
  ggplot2::layer(
    geom = GeomAbsCircle,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius,
      ...
    )
  )
}

#' GeomAbsCircle
#' @noRd
GeomAbsCircle <- ggplot2::ggproto(
  "GeomAbsCircle",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    alpha = 1,
    colour = "black",
    fill = "grey65",
    linetype = 1,
    linewidth = 0.5
  ),

  setup_params = function(data, params) {
    params
  },

  setup_data = function(data, params) {
    data
  },

  draw_key = ggplot2::draw_key_polygon,

  draw_panel = function(
    data,
    panel_scales,
    coord,
    radius = grid::unit(50, "mm")
  ) {
    
    # Transform data to plot scales
    data <- coord$transform(data, panel_scales)

    # Set up gTree
    gt <- grid::gTree(
      data = data,
      radius = radius,
      cl = "abscircletree"
    )
    gt$name <- grid::grobName(gt, "geom_abs_circle")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.abscircletree <- function(gt) {

  # Extract data
  data <- gt$data

  # Prepare a grob for each circle
  circlegrobs <- lapply(seq_len(nrow(data)), function(i) {
    circle <- data[i, ]
    grid::circleGrob(
      x = circle$x,
      y = circle$y,
      r = gt$radius,
      gp = grid::gpar(
        alpha = circle$alpha,
        fill = circle$fill,
        col = circle$colour,
        lty = circle$linetype,
        lwd = circle$linewidth
      )
    )
  })
  class(circlegrobs) <- "gList"
  grid::setChildren(gt, circlegrobs)
}
