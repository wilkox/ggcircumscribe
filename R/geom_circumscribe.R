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

    # If reflow is set, reflow the text.
    #
    # For efficiency, we use two different algorithms to select the reflow.
    #
    # Reflow circularity is scored as the reciprocal of the sum of the maximum
    # radius of any vertex of any bounding box in the text layout and the
    # standard deviation of all the vertices.
    #
    # For text with <= 9 breakpoints (<= 512 possible reflows), all possible
    # reflows (combinations of breakpoints) are considered, and the
    # most circular reflow is selected.
    #
    # For text with > 9 breakpoints, a genetic algorithm is used to select a
    # satisficing reflow.
    if (gt$reflow) {

      # As a preparatory step, strip out unnecessary whitespace
      text$label <- stringi::stri_replace_all(text$label, "", regex = "^\\s+|\\s+$")
      text$label <- stringi::stri_replace_all(text$label, " ", regex = "[^\\S\\r\\n]+")

      cli::cli_h1("Preparing to reflow")
      cli::cli_verbatim(text$label)

      # Identify all potential breakpoints in the text
      breakpoints <- unname(stringi::stri_locate_all(
        text$label,
        regex = "[^\\S\\r\\n]+"
      )[[1]][,1])

      # Function that will generate a data frame of reflows, layouts, and
      # circularity scores for a list of sets of breakpoints
      generate_reflows_from_breakpoints <- function(breakpoints_list) {

        reflows <- data.frame(breakpoints = I(breakpoints_list))

        # Generate a reflow with each combination of breakpoints
        reflows$reflow <- vapply(
          reflows$breakpoints,
          function(breakpoints) stringi::stri_sub_replace_all(
            text$label,
            breakpoints,
            breakpoints,
            replacement = "\n"
          ),
          character(1)
        )

        # Generate a layout for each reflow
        reflows$layout <- lapply(reflows$reflow, lay_out)

        # For each layout, calculate the circularity score
        reflows$sd_radii <- vapply(
          reflows$layout,
          function(layout) layout$radius |> unlist() |> sd(),
          double(1)
        )
        reflows$max_radius <- vapply(
          reflows$layout,
          function(layout) layout$radius |> unlist() |> max(),
          double(1)
        )
        reflows$circularity <- 1 / (reflows$max_radius + reflows$sd_radii)

        return(reflows)
      }

      # If the total number of breakpoints <= 9, consider all possible reflows
      if (length(breakpoints) <= 9) {

        # Generate all possible combinations of breakpoints
        breakpoints_list <- lapply(0:length(breakpoints), function(size) {
            
          # Because combn is stupid
          if (length(breakpoints) == 1) {
              breakpoints[size]
            } else {
              combn(breakpoints, size, simplify = FALSE)
          }

          }) |>
            unlist(recursive = FALSE)

        # Generate a reflow, layout, and circularity score for each combination
        # of breakpoints
        reflows <- generate_reflows_from_breakpoints(breakpoints_list)

        # Select the most circular reflow, updating the working text and layout
        reflow <- reflows[which(reflows$circularity == max(reflows$circularity))[1], ]
        text$label <- reflow$reflow
        layout <- reflow$layout[[1]]

      # If the total number of breakpoints > 9, use a genetic algorithm to
      # select a satisficing reflow
      } else {

        cli::cli_h2("Entering genetic algorithm")

        # Set seed to ensure deteministic results
        set.seed(1)

        # Function to breed together two parents. Homozygote allele has 90%
        # probability of propogating, heterozygote is a 50/50 coin toss
        breed <- function(p1, p2) {
          vapply(
            (0.4 * (p1 + p2)) + 0.1,
            function(p) sample(c(TRUE, FALSE), 1, prob = c(p, 1 - p)),
            logical(1)
          )
        }

        # Generate initial population, seeding with a 50% probability of a
        # break at any given point
        population <- lapply(1:10, function(i) {
          breakpoints %in% sample(breakpoints, size = sample(1:length(breakpoints), 1))
        })



      }
      
      cli::cli_alert_info("Exited reflowing with text:")
      cli::cli_verbatim(text$label)

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
