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
  m[,]     <- stats::rnorm(length(m), 5, 1)
  m[m.idx] <- stats::rnorm(sum(m.idx), 5 + noise.difference, .5)

  m
}


#' @noRd
get.letter <- function(symbol)
{
  idx <- get.index(symbol)
  if (!idx %in% names(pixelmap::font) && idx != " ") {
     stop(paste0(symbol, " could not be found in the font!"))
  }

  pixelmap:::font[[idx]]
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
