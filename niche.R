if (!require('devtools')) install.packages('devtools')
devtools::install_github('luismurao/nichetoolbox')



ntbox_pkgs <- c("devtools", "shiny", "rgeos", "rgdal", "sp", "raster", "maptools", "dismo", 
                "rgl", "dygraphs", "png", "rmarkdown", "knitr", "stringr", "MASS",
                "animation", "mgcv", "googleVis","rasterVis", "shinyBS","shinyjs",
                "rglwidget", "car", "maps", "corrplot", "dplyr", "cluster", "sqldf",
                "fields", "devtools", "psych", "shinythemes", "grid", "RColorBrewer",
                "ade4", "spocc")

missing_pkgs <- ntbox_pkgs[which(!ntbox_pkgs %in% installed.packages())]

if(length(missing_pkgs))
  install.packages(ntbox_pkgs)
install.packages(ntbox_pkgs)
devtools::install_github("luismurao/leaflet")
devtools::install_github("AnalytixWare/ShinySky")
devtools::install_github("ENMGadgets", "narayanibarve")


library(nichetoolbox)
run_nichetoolbox()
