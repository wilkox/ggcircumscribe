#' A 'ggplot2' geom to fit text inside a circle
#'
#' `geom_circumscribe()` shrinks, grows, and wraps text to fit inside a circle.
#'
#' @details
#'
#' Except where noted, `geom_circumscribe()` behaves like
#' `ggplot2::geom_text()`.
#'
#' If the text is too big to fit in the circle, it will shrink to fit the
#' circle. If `grow = TRUE` is set, the text will be made as large as possible
#' whether that means shrinking or growing it.
#'
#' If `reflow = TRUE` is set, the text will be reflowed (wrapped) to fill the
#' circle as tightly as possible. If the text is still too big for the circle
#' after reflowing, it will be shrunk as usual. Existing line breaks are
#' respected when reflowing.
#'
#' TODO need to figure out a principled mapping of data to radius, or
#' alternatively another aesthetic that could reasonably be mapped to radius.
#'
#' @section Aesthetics:
#'
#' - label (required)
#' - x (required)
#' - y (required)
#' - radius (required)
#' - alpha
#' - angle
#' - colour
#' - family
#' - fontface
#' - lineheight
#' - size
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for `ggplot2::geom_text()`.
#' @param padding Padding between the text and the circle. A `grid::unit()`
#' object. Defaults to `grid::unit(4, "mm")`.
#' @param grow If `TRUE`, text will be made as large as able to fit in the
#' circle.
#' @param reflow If `TRUE`, text will be reflowed (wrapped) to fill the circle
#' as tightly as possible.
#'
#' @export
geom_circumscribe <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  padding = grid::unit(4, "mm"),
  grow = FALSE,
  reflow = FALSE,
  ...
) {
  ggplot2::layer(
    geom = GeomCircumscribe,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      padding = padding,
      grow = grow,
      ...
    )
  )
}

#' GeomCircumscribe
#' @noRd
GeomCircumscribe <- ggplot2::ggproto(
  "GeomCircumscribe",
  ggplot2::Geom,
  required_aes = c("label", "x", "y", "radius"),
  default_aes = ggplot2::aes(
    alpha = 1,
    angle = 2,
    colour = "black",
    family = "",
    fontface = 1,
    lineheight = 1.4,
    size = 12
  ),

  setup_params = function(data, params) {
    params
  },

  setup_data = function(data, params) {
    data
  },

  draw_key = ggplot2::draw_key_label,

  draw_panel = function(
    data,
    panel_scales,
    coord,
    padding = grid::unit(4, "mm"),
    grow = FALSE,
    reflow = FALSE
  ) {
    
    # Transform data to plot scales
    data <- coord$transform(data, panel_scales)

    # Set up gTree
    gt <- grid::gTree(
      data = data,
      padding = padding,
      grow = grow,
      reflow = reflow,
      cl = "circumscribetree"
    )
    gt$name <- grid::grobName(gt, "geom_circumscribe")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.circumscribetree <- function(gt) {

  # Extract data
  data <- gt$data

  # Prepare a grob for each text label
  textgrobs <- lapply(seq_len(nrow(data)), function(i) {

    text <- data[i, ]

    # Return a simple textGrob
    tg <- grid::textGrob(label = text$label, x = 0.5, y = 0.5, gp = grid::gpar(fontsize = 70))
    return(tg)

    # Dev parameters
    dev_label <- "Early morning joggers"
    dev_x <- 0.5 # the x-coordinate of the circle centre
    dev_y <- 0.5 # the y-coordinate of the circle centre
    dev_radius <- unit(50, "mm")
    dev_fontsize <- 90
    dev_gpar_text <- gpar(fontsize = dev_fontsize)
    dev_lineheight <- 1.4
    dev_padding <- 4 # in mm

    # Draw the circle
    grid.circle(r = dev_radius, x = dev_x, y = dev_y, gp = gpar(fill = "blue", alpha = "0.5"))

    # Split the label into lines
    label <- dev_label
    lines <- unlist(stringi::stri_split(label, regex = "\n"))

    # Identify all the line-wise locations of whitespace in the text (i.e.
    # possible widths to wrap to). Include the full lengths of each line, to
    # allow for the wrapless wrap
    breakpoints <- lapply(
      lines,
      function(line) stringi::stri_locate_all(line, regex = "\\s")[[1]][,1]
    )
    breakpoints <- c(breakpoints, stringi::stri_length(lines))
    breakpoints <- sort(unique(unlist(breakpoints)))

    # Generate wraps for those lengths
    wraps <- data.frame(wrapwidth = breakpoints)
    wraps$wrap <- vapply(wraps$wrapwidth, function(w) {
      paste0(lapply(lines, function(line) {
        paste0(
          stringi::stri_wrap(str = line, width = w, normalise = FALSE),
          collapse = "\n"
        )
      } ), collapse = "\n")
    }, character(1))

    # Calculate each wrap's aspect ratio
    wraps$tg <- lapply(wraps$wrap, function(wrap) { textGrob(label = wrap, gp = dev_gpar_text) })
    wraps$width <- vapply(wraps$tg, tgWidth, double(1))
    wraps$height <- vapply(wraps$tg, tgHeight, double(1))
    wraps$aspect_ratio <- wraps$width / wraps$height

    # Pick the wrap with the aspect ratio closest to 1
    wraps$ar_diff <- abs(1 - wraps$aspect_ratio)
    label <- wraps[which(wraps$ar_diff == min(wraps$ar_diff))[1], "wrap"]

    # Split the label into lines
    lines <- data.frame(label = unlist(stringi::stri_split(label, regex = "\n")))

    # Generate a textGrob for each line
    lines$tg <- lapply(
      lines$label,
      function(label) textGrob(label = label, gp = dev_gpar_text)
    )

    # Calculate the height, descender-height, and width of each line
    lines$width <- vapply(lines$tg, tgWidth, double(1))
    lines$height <- vapply(lines$tg, tgHeight, double(1))
    lines$dheight <- vapply(lines$tg, tgDheight, double(1))

    # Set a lineheight in npc based on the tallest line height
    lineheight <- grid::unit(dev_lineheight * max(lines$height), "mm")
    lineheight <- grid::convertHeight(lineheight, "npc", valueOnly = TRUE)

    # Distribute the lines, centred on (0, 0)
    lines$x <- 0
    lines$y <- 0:(nrow(lines) - 1) * -lineheight
    lines$y <- lines$y + abs(mean(range(lines$y)))

    # Determine the coordinates of the right-sided vertices of the bounding boxes
    # (including descenders) for each line. We can ignore the left side as the
    # boxes are horizontally symmetric. To allow for non-square coordinate fields,
    # these coordinates are expressed in mm
    xs <- rep((lines$width / 2), 2)
    topys <- grid::convertHeight(grid::unit(lines$y, "npc"), "mm", valueOnly = TRUE) + (lines$height / 2)
    bottomys <- grid::convertHeight(grid::unit(lines$y, "npc"), "mm", valueOnly = TRUE) - (lines$height / 2) - (lines$dheight)
    ys <- c(topys, bottomys)

    # Determine the distance from the origin of each vertex, in mm, with padding
    ds <- sqrt(abs(xs ^ 2 + ys ^ 2)) + dev_padding

    # Find the ratio between the highest distance and the radius of the circle;
    # this is the scaling factor
    scaling_factor <- as.numeric(dev_radius) / max(ds)

    # Scale everything down
    lines$tg <- lapply(
      lines$tg,
      function(tg) {
        tg$gp$fontsize <- tg$gp$fontsize * scaling_factor
        tg
      }
    )
    lines$y <- lines$y * scaling_factor

    # Re-centre the textGrobs on the circle centre
    lines$x <- lines$x + dev_x
    lines$y <- lines$y + dev_y

    # Pan-sear the textGrobs to really seal in the Cartesian coordinates
    lines$tg <- lapply(1:nrow(lines), function(i) {
      tg <- lines$tg[[i]]
      tg$x <- grid::unit(lines$x[i], "npc")
      tg$y <- grid::unit(lines$y[i], "npc")
      tg
    })

    # Draw the lines
    for (tg in lines$tg) {
      grid.draw(tg)
    }


  })

  class(textgrobs) <- "gList"
  grid::setChildren(gt, textgrobs)
}

