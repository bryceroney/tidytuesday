library(tidyverse)
library(tidytuesdayR)
library(extrafont)

loadfonts(quiet = T)
loadfonts(device = "win", quiet = T)
loadfonts(device = "postscript", quiet = T)

knitr::opts_chunk$set(echo = T, message = F, warning = F, fig.width = 7, fig.height = 3, dpi = 120)

theme_set(theme_minimal(
  base_family = "Roboto",
  base_size = 10
))
