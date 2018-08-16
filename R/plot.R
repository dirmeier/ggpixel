# pixelmap: write words on tiles
#
# Copyright (C) 2018 Simon Dirmeier
#
# This file is part of pixelmap
#
# pixelmap is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pixelmap is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with pixelmap If not, see <http://www.gnu.org/licenses/>.



#' @export
pixelmap <- function(x,
                     noise.difference=3,
                     color.palette  = "viridis",
                     margin.left    = 5,
                     margin.right   = 5,
                     margin.top     = 10,
                     margin.bottom  = 10,
                     letter.spacing = 3,
                     na.value       = "white",
                     color          = "grey30",
                     aspect.ratio   = .1)
{
  UseMethod("pixelmap")
}


#' @export
#' @method pixelmap character
pixelmap.character  <- function(x,
                                noise.difference = 5,
                                color.palette    = "viridis",
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

  col.option <- switch(
    color.palette,
    "magma"   = "A",
    "inferno" = "B",
    "plasma"  = "C",
    "viridis" = "D",
    stop("Please choose either magma/plasma/inferno/viridis!")
  )

  .plot.matrix(m, na.value, color, aspect.ratio, col.option)
}


#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#' @importFrom dplyr mutate
#' @importFrom reshape2 melt
.plot.matrix <-  function(m,
                          na.value     = "white",
                          color        = "grey30",
                          aspect.ratio = .1,
                          col.option    = "D")
{
  reshape2::melt(m) %>%
    dplyr::mutate(Var1 = factor(Var1, level=rev(unique(Var1)))) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(Var2, Var1, fill=value), color = color) +
    ggplot2::theme_void() +
    viridis::scale_fill_viridis(option = col.option, na.value = na.value) +
    ggplot2::theme(aspect.ratio = aspect.ratio)+
    ggplot2::guides(fill = FALSE)
}
