library(magrittr)
library(ggplot2)
library(scales)

rm(list = ls())
options(encoding = "UTF-8")
tpltheme::set_tpl_theme(style = "print", font = "lato")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

county <- create_data.county()

state <- create_data.state()

usa <- create_data.usa()

world <- create_data.world()