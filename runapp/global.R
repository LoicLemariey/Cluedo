#globals
library(shiny)
library(bslib)
library(gridExtra)
library(gtools)
library(stringr)
library(dplyr)
library(stringr)
library(ggplot2)
library(shinyjs)


#------------------parameters-------------
source("functions.R")
source("config.R")

#source("compute_rank.R")
options(warn = -1)

#plan(multisession, workers = min(availableCores() - 1))
# handlers("txtprogressbar")

#  load data--------------------------------------------------------------------

#recover proba




