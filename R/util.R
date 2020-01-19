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


#' @noRd
#' @importFrom stats rnorm
#' @importFrom stringr str_split
build.matrix <- function(x,
                          noise.difference=5,
                          margin.left=5,
                          margin.right=5,
                          margin.top=10,
                          margin.bottom=10,
                          letter.spacing=3)
{
  letter.size <- 10
  symbols     <- stringr::str_split(x, "")[[1]]
  spacing     <- matrix(0, letter.size, letter.spacing)

  m <- NULL
  for (i in seq(symbols))
  {
    m <- cbind(m, get.letter(symbols[i]))
    if (i < length(symbols)) m <- cbind(m, spacing)
  }

  m <- cbind(matrix(0, letter.size, margin.left),
             m,
             matrix(0, letter.size, margin.right))
  m <- rbind(matrix(0, margin.top, ncol(m)),
             m,
             matrix(0, margin.bottom, ncol(m)))

  m.idx    <- m == 1
  m[,]     <- stats::rnorm(length(m), 0, 1)
  m[m.idx] <- stats::rnorm(sum(m.idx), noise.difference, .5)

  m
}


#' @noRd
get.letter <- function(symbol)
{
  idx <- get.index(symbol)
  if (!idx %in% names(font) && idx != " ") {
     stop(paste0(symbol, " could not be found in the font!"), call. = FALSE)
  }

  font[[idx]]
}


#' @noRd
get.index <- function(symbol)
{
  idx <- switch(
    symbol,
    "!" = "excl",
    "." = "dot",
    "," = "comma",
    "?" = "ques",
    symbol
  )

  idx
}


#' @noRd
#' @importFrom dplyr %>% rename bind_rows slice
#' @importFrom purrr map_df
get.boundary.shape <- function(overlay, overlay.radius, plot.dim)
{
  # define boundary
  size.x <- plot.dim[2] + 1
  size.y <- plot.dim[1] + 1

  rim <- c(
    0, 0,
    size.x, 0,
    size.x, size.y,
    0, size.y,
    0, 0
  ) %>%
    matrix(ncol=2, byrow=TRUE) %>%
    as.data.frame %>%
    rename(x=.data$V1, y=.data$V2)

  # define overlay shape
  offset.x <- size.x / 2
  offset.y <- size.y / 2

  shape <- map_df(overlay:1, function(i) {
    data.frame(
      x=offset.x + overlay.radius * cos(pi/2+2 * pi * i / overlay),
      y=offset.y + overlay.radius * sin(pi/2+2 * pi * i / overlay)
    )
  })
  shape <- bind_rows(
    shape,
    shape %>% slice(1) # close outline
  )

  return(bind_rows(rim, shape))
}
