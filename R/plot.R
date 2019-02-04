# ggpixel: write words on tiles
#
# Copyright (C) 2018 Simon Dirmeier
#
# This file is part of ggpixel
#
# ggpixel is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ggpixel is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ggpixel If not, see <http://www.gnu.org/licenses/>.



#' @title Plot a word on tiles
#'
#' @description The method takes a string and translates it into a tile plot
#'  using \code{ggplot2}. The resulting lpot looks like a heamtap with a string
#'  engraved.
#'
#' @export
#'
#' @param x  a \code{character} vector
#' @param noise.difference \code{numeric} value that is added to the letter
#'   tiles in order to create a contrast. The higher the stronger the contrast.
#' @param color.palette  obviously the colorpalette.
#'   One of the viridis colors.
#' @param margin.left how many pixels are inserted to the left from the string
#' @param margin.right how many pixels are inserted to the right
#'   from the string
#' @param margin.top how many pixels are inserted to the top  from the string
#' @param margin.bottom how many pixels are inserted to the bottom
#'   from the string
#' @param letter.spacing  how many pixels are used for spacing two characters
#' @param na.value color used for NAs
#' @param color color used for the lines separating the tiles
#' @param aspect.ratio \code{ggplot2} \code{aspect.ratio} parameter. Modifies
#'   the dimensions of the tiles.
#'
#' @return returns a \code{ggplot} object
#'
#' @examples
#' \dontrun{
#'   obiwan <- ggpixel("hello there!")
#'
#'   grievous <- ggpixel("general kenobi!", 3, "magma", 10, 10, 10, 10)
#' }
ggpixel <- function(
  x,
  noise.difference = 5,
  color.palette    = c("viridis", "magma", "plasma", "inferno"),
  margin.left      = 5,
  margin.right     = 5,
  margin.top       = 10,
  margin.bottom    = 10,
  letter.spacing   = 3,
  na.value         = "white",
  color            = "grey30",
  aspect.ratio     = .1)
{
  UseMethod("ggpixel")
}

#' @export
#' @method ggpixel character
ggpixel.character  <- function(
  x,
  noise.difference = 5,
  color.palette    = c("viridis", "magma", "plasma", "inferno"),
  margin.left      = 5,
  margin.right     = 5,
  margin.top       = 10,
  margin.bottom    = 10,
  letter.spacing   = 3,
  na.value         = "white",
  color            = "grey30",
  aspect.ratio     = .1)
{

  m <- build.matrix(x, noise.difference, margin.left, margin.right,
                    margin.top, margin.bottom, letter.spacing)

  color.palette <- match.arg(color.palette)
  col.option <- switch(
    color.palette,
    "magma"   = "A",
    "inferno" = "B",
    "plasma"  = "C",
    "viridis" = "D",
    stop("Please choose either magma/plasma/inferno/viridis!", call.= FALSE)
  )

  .ggpixel(m, na.value, color, aspect.ratio, col.option)
}


#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#' @importFrom reshape2 melt
#' @importFrom rlang .data
.ggpixel <-  function(
  m,
  na.value     = "white",
  color        = "grey30",
  aspect.ratio = .1,
  col.option    = "D")
{
  df <- reshape2::melt(m)
  df$Var1 <- factor(df$Var1, levels = rev(unique(df$Var1)))
  ggplot2::ggplot(df) +
    ggplot2::geom_tile(ggplot2::aes(
      .data$Var2, .data$Var1, fill = .data$value), color = color) +
    ggplot2::theme_void() +
    viridis::scale_fill_viridis(option = col.option, na.value = na.value) +
    ggplot2::theme(aspect.ratio = aspect.ratio) +
    ggplot2::guides(fill = FALSE)
}
