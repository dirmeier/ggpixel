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


context("ggpixel")

test_that("ggpixel prints the plot", {
  p <- ggpixel(paste0("?.!,", paste0(letters, collapse="")))
  testthat::expect_s3_class(p , "gg")
})
