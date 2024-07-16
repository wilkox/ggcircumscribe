# Set up grid
library(grid)
grid.newpage()
warning("This assumes a square plot area, to account for non-square plot area, units will need to be converted to absolute values (mm)")

# Dev parameters
dev_label <- "Early morning joggers"
dev_x <- 0.5 # the x-coordinate of the circle centre
dev_y <- 0.5 # the y-coordinate of the circle centre
dev_radius <- unit(0.3, "npc")
dev_fontsize <- 90
dev_gpar_text <- gpar(fontsize = dev_fontsize)
dev_lineheight <- 1.4

#' Calculate the width of a textGrob, in npc
#' 
#' @noRd
tgWidth <- function(tg) {
  grid::convertWidth(grid::grobWidth(tg), "npc", valueOnly = TRUE)
}

#' Calculate the height of a textGrob, in npc
#' 
#' @noRd
tgHeight <- function(tg) {
  grid::convertHeight(grid::grobHeight(tg), "npc", valueOnly = TRUE)
}

#' Calculate the descender-height of a textGrob, in npc
#' 
#' @noRd
tgDheight <- function(tg) {
  grid::convertHeight(grid::grobDescent(tg), "npc", valueOnly = TRUE)
}

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
lineheight <- dev_lineheight * max(lines$height)

# Distribute the lines, centred on (0, 0)
lines$x <- 0
lines$y <- 0:(nrow(lines) - 1) * -lineheight
lines$y <- lines$y + abs(mean(range(lines$y)))

# Determine the left-sided vertices of the bounding boxes (including
# descenders) for each line. We can ignore the right side as the boxes are
# horizontally symmetric
xs <- rep(-(lines$width / 2), 2)
topleftys <- lines$y + (lines$height / 2)
bottomleftys <- lines$y - (lines$height / 2) - lines$dheight
ys <- c(topleftys, bottomleftys)

# Determine the distance from the origin of each vertex
ds <- sqrt(abs(xs ^ 2 + ys ^ 2))

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
