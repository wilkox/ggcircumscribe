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
#' For now, radius is expressed as an absolute parameter rather than a plot
#' aesthetic. This is liable to change in future.
#'
#' @section Aesthetics:
#'
#' - label (required)
#' - x (required)
#' - y (required)
#' - alpha
#' - angle
#' - colour
#' - family
#' - fontface
#' - lineheight
#' - fontsize
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for `ggplot2::geom_text()`.
#' @param radius Radius of the circle. A `grid::unit()` object. Defaults to 50
#' mm.
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
  radius = grid::unit(50, "mm"),
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
      radius = radius,
      padding = padding,
      grow = grow,
      reflow = reflow,
      ...
    )
  )
}

#' GeomCircumscribe
#' @noRd
GeomCircumscribe <- ggplot2::ggproto(
  "GeomCircumscribe",
  ggplot2::Geom,
  required_aes = c("label", "x", "y"),
  default_aes = ggplot2::aes(
    alpha = 1,
    angle = 0,
    colour = "black",
    family = "",
    fontface = 1,
    lineheight = 1.4,
    fontsize = 12
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
    radius = grid::unit(50, "mm"),
    padding = grid::unit(4, "mm"),
    grow = FALSE,
    reflow = FALSE
  ) {
    
    # Transform data to plot scales
    data <- coord$transform(data, panel_scales)

    # Set up gTree
    gt <- grid::gTree(
      data = data,
      radius = radius,
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

    # Set up base gpar
    base_gpar <- grid::gpar(
      alpha = text$alpha,
      angle = text$angle,
      col = text$colour,
      fontsize = text$fontsize,
      fontfamily = text$family,
      fontface = text$fontface
    )

    # Function to generate a collection of textGrobs for the text, and
    # calculate the radii of their bounding boxes
    lay_out <- function(t) {

      # Split the label into lines
      layout <- data.frame(line = unlist(stringi::stri_split(t, regex = "\n")))

      # Generate a textGrob for each line
      layout$tg <- lapply(
        layout$line,
        function(line) grid::textGrob(label = line, gp = base_gpar)
      )

      # Calculate the height, descender-height, and width of each line
      layout$width <- vapply(layout$tg, tgWidth, double(1))
      layout$height <- vapply(layout$tg, tgHeight, double(1))
      layout$dheight <- vapply(layout$tg, tgDheight, double(1))

      # Set a lineheight in npc based on the tallest line height
      lineheight <- grid::unit(text$lineheight * max(layout$height), "mm")
      lineheight <- grid::convertHeight(lineheight, "npc", valueOnly = TRUE)

      # Distribute the lines, centred on (0, 0)
      layout$x <- 0
      layout$y <- 0:(nrow(layout) - 1) * -lineheight
      layout$y <- layout$y + abs(mean(range(layout$y)))

      # Determine the coordinates of the right-sided vertices of the bounding
      # boxes (including descenders) for each line. We can ignore the left side
      # as the boxes are horizontally symmetric. To allow for non-square
      # coordinate fields, these coordinates are expressed in mm
      layout$bb_x <- lapply(layout$width, function(w) rep(w / 2, 2))
      layout$bb_y <- lapply(1:nrow(layout), function(i) {
        topy <- grid::convertHeight(grid::unit(layout$y[i], "npc"), "mm", valueOnly = TRUE) + (layout$height[i] / 2)
        bottomy <- grid::convertHeight(grid::unit(layout$y[i], "npc"), "mm", valueOnly = TRUE) - (layout$height[i] / 2) - (layout$dheight[i])
        c(topy, bottomy)
      })

      # Determine the radius (distance from the origin) of each vertex, in mm
      layout$radius <- lapply(1:nrow(layout), function(i) {
        sqrt(abs(layout$bb_x[[i]] ^ 2 + layout$bb_y[[i]] ^ 2))
      })

      return(layout)
    }

    # Start by generating a layout for the text as-is
    layout <- lay_out(text$label)

    # Function to test if layout fits inside the bounding circle
    fits_bounding_circle <- function(layout) {
      all(unlist(layout$radius) < as.numeric(gt$radius) - grid::convertWidth(gt$padding, "mm", valueOnly = TRUE))
    }

    # Function to take a finalised layout and prepare a final list of textGrobs
    # to be returned
    prepare_finalised_tgs <- function(layout) {

      # Re-centre the textGrobs on the circle centre
      layout$x <- layout$x + text$x
      layout$y <- layout$y + text$y

      # Pan-sear the textGrobs to really seal in the Cartesian coordinates
      layout$tg <- lapply(1:nrow(layout), function(i) {
        tg <- layout$tg[[i]]
        tg$x <- grid::unit(layout$x[i], "npc")
        tg$y <- grid::unit(layout$y[i], "npc")
        tg
      })

      return(layout$tg)
    }

    # If the as-is layout fits and grow is not set, we can stop at this point
    # and return the textGrobs
    if (fits_bounding_circle(layout) & ! gt$grow) {
      return(prepare_finalised_tgs(layout))
    }

    # If reflow is set, at this point we generate the optimal reflow
    if (gt$reflow) {

      # We use a somewhat efficient greedy algorithm to select the optimal
      # reflow.
      #
      # 1. Identify the one or two breakpoints for each line of the text (i.e.
      # the existing line breaks are kept as-is) that most closely bisect the
      # line, and every combination of those breakpoints across lines (i.e.
      # every possible combination where zero, one, or two new bisecting
      # breakpoints is added to each line).
      # 2. For each breakpoint, calculate the maximum radius of the text
      # bounding boxes in the resulting layout.
      # 3. Select the breakpoint from step 1 that among those breakpoints that
      # minimise the radius, has the minimal range in radius, and add a line
      # break at this point.
      # 4. Repeat steps 1-3 until no breakpoint is identified that results in a
      # radius smaller than that from the previous step, or until no new
      # breakpoints remain. 

      # As a preparatory step, strip out unnecessary whitespace
      text$label <- stringi::stri_replace_all(text$label, "", regex = "^\\s+|\\s+$")
      text$label <- stringi::stri_replace_all(text$label, " ", regex = "[^\\S\\r\\n]+")

      best_radius <- Inf
      cli::cli_h1("Preparing to reflow")
      cli::cli_text(text$label)
      while (TRUE) {

        cli::cli_h2("Starting a reflow iteration")
        cli::cli_alert_info("Current text:")
        cli::cli_verbatim(text$label)
        cli::cli_alert_info("With radius == {best_radius}")

        # For each line, identify the one or two breakpoints (whitespace) that
        # most closely bisect the line
        lines <- unlist(stringi::stri_split(text$label, regex = "\n"))
        breakpoints <- lapply(lines, function(line) {
          breakpoints <- stringi::stri_locate_all(line, regex = "\\s")[[1]][,1]
          midpoint <- (stringi::stri_length(line) / 2) + 0.5
          dist_from_midpoint <- abs(midpoint - breakpoints)
          breakpoints[which(dist_from_midpoint == min(dist_from_midpoint))]
        })

        # If there are no possible new reflows, the current reflow is selected
        cli::cli_alert_info("Identified {length(na.omit(unlist(breakpoints)))} breakpoints to test")
        if (length(na.omit(unlist(breakpoints))) == 0) break

        # For each line, generate a reflow with the text broken at each
        # breakpoint, as well as with no new breaks
        lines <- lapply(1:length(lines), function(i) {
          c(vapply(breakpoints[[i]], function(breakpoint) {
            stringi::stri_sub_replace(lines[i], breakpoint, breakpoint, replacement = "\n")
          }, character(1)), lines[i])
        })
        lines <- lapply(lines, na.omit)

        # Generate all combinations of the line reflows
        reflows <- data.frame(reflow = do.call(paste, c(do.call(expand.grid, lines), sep = "\n")))
        cli::cli_alert_info("Generated {nrow(reflows)} reflows from these breakpoint combinations")

        # Generate a layout for each reflow
        reflows$layout <- lapply(reflows$reflow, lay_out)

        # Calculate the maximum radius for each reflow
        reflows$max_r <- vapply(reflows$layout, function(layout) {
          max(unlist(layout$radius))
        }, double(1))

        # If the reflow with the smallest maximum radius still has a radius
        # larger than or equal to the current best radius, stop the search here
        if (min(reflows$max_r) >= best_radius) {
          cli::cli_alert_info("Smallest maximum radius is {min(reflows$max_r)}, this does not beat current best radius, stopping here")
          break
        }

        # Otherwise, select the reflow which, among those reflows with the
        # minimal maximum radius, has the minimal range in radius, and
        # continue the search
        cli::cli_alert_info("There are {length(which(reflows$max_r == min(reflows$max_r)))} reflows with equal lowest max radius")
        reflows <- reflows[which(reflows$max_r == min(reflows$max_r)), ]
        reflows$r_range <- vapply(reflows$layout, function(layout) {
          diff(range(unlist(layout$radius)))
        }, double(1))
        cli::cli_alert_info("There are {length(which(reflows$r_range == min(reflows$r_range)))} reflows with equal lowest range in radius")
        reflows <- reflows[which(reflows$r_range == min(reflows$r_range))[1], ]
        text$label <- reflows$reflow
        best_radius <- reflows$max_r
        cli::cli_alert_info("Best reflow is:")
        cli::cli_verbatim(text$label)
        cli::cli_alert_info("With radius == {best_radius}")

      }

      cli::cli_alert_info("Exited reflowing with text:")
      cli::cli_verbatim(text$label)

      # Update the working layout with the reflowed text
      layout <- lay_out(text$label)

      # If the reflowed layout fits and grow is not set, we can stop at this
      # point and return the textGrobs
      if (fits_bounding_circle(layout) & ! gt$grow) {
        return(prepare_finalised_tgs(layout))
      }

    }

    # Now it is time to resize the text, either because it is still too big to
    # fit the circle (even after rescaling if that was set), or because grow
    # has been set so we want to make it as large as possible

    # Find the ratio between the largest bounding box radius in the current
    # layout and the radius of the bounding circle minus padding; this is the
    # scaling factor
    scaling_factor <- (as.numeric(gt$radius) - grid::convertWidth(gt$padding, "mm", valueOnly = TRUE)) / max(unlist(layout$radius))

    # Resize the text to fit the bounding circle
    layout$tg <- lapply(
      layout$tg,
      function(tg) {
        tg$gp$fontsize <- tg$gp$fontsize * scaling_factor
        tg
      }
    )
    layout$y <- layout$y * scaling_factor

    return(prepare_finalised_tgs(layout))
  })

  textgrobs <- unlist(textgrobs, recursive = FALSE)
  class(textgrobs) <- "gList"
  grid::setChildren(gt, textgrobs)
}
