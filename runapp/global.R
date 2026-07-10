#globals
library(shiny)
library(bslib)
library(gridExtra)
library(gtools)
library(stringr)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(DT)


#------------------parameters-------------
source("functions.R")
source("config.R")

#source("compute_rank.R")
options(warn = -1)

#plan(multisession, workers = min(availableCores() - 1))
# handlers("txtprogressbar")

#  load data--------------------------------------------------------------------

#recover proba

nb_categories<-n_dimension
n_tot_combi<-nb_total_combi(cartes_df,n_joueurs)
n_solution_start<-length(lieux)*length(armes)*length(characters)
scenario_per_solution_start<-n_tot_combi/n_solution_start





