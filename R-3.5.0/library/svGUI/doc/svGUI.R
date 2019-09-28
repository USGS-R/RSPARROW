## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(svGUI)
.GUI

## ------------------------------------------------------------------------
names(.GUI)

## ------------------------------------------------------------------------
gui_ask(.GUI)
# Turn the ask property off, so that the GUI does not interrupt R any more
gui_ask(.GUI) <- FALSE
gui_ask(.GUI)

## ------------------------------------------------------------------------
gui_add("tk_gui", widgets = "tcltk", ask = TRUE)
gui_list()

