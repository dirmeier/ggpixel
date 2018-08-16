.letters <- function()
{
  letters. <- purrr:::map(letters, function(.) {
    matrix(0, 10, 10)
  })
  names(letters.) <- letters
}

a <- matrix(0, 5, 5)
a[1,1:4] <- 1
a[2:4,5] <- 1
a[3,2:5] <- 1
a[4, 1] <- 1
a[5, 2:4] <- 1
a
library(ggplot2)
library(reshape2)
library(dplyr)

melt(a) %>%
  dplyr::mutate(Var1 = factor(Var1, level=rev(unique(Var1)))) %>%
  ggplot(aes(Var2, Var1, fill=value)) +
  viridis::scale_fill_viridis() +
  geom_tile()
