#' Calculate the width of a textGrob, in mm
#' 
#' @noRd
tgWidth <- function(tg) {
  grid::convertWidth(grid::grobWidth(tg), "mm", valueOnly = TRUE)
}

#' Calculate the height of a textGrob, in mm
#' 
#' @noRd
tgHeight <- function(tg) {
  grid::convertHeight(grid::grobHeight(tg), "mm", valueOnly = TRUE)
}

#' Calculate the descender-height of a textGrob, in mm
#' 
#' @noRd
tgDheight <- function(tg) {
  grid::convertHeight(grid::grobDescent(tg), "mm", valueOnly = TRUE)
}
