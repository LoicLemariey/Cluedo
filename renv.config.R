#renvconfig
library(renv)

.libPaths()

renv_lib<-"C:/Users/loicl/OneDrive/Documents/Loïc/Divers_non_disque/Data_science/Projet/MesProjets/Cluedo/Cluedo_app/renv/library/R-4.2/x86_64-w64-mingw32"

lib2<-"C:/Users/loicl/AppData/Local/R/cache/R/renv/sandbox/R-4.2/x86_64-w64-mingw32/0cdf27ab"


project_runapp<-"~/Loïc/Divers_non_disque/Data_science/Projet/MesProjets/Coupe_du_monde_2026/runapp"

.libPaths(renv_lib)
.libPaths(lib2)

.libPaths()
renv::init(bare = TRUE)





install.packages("shiny")
install.packages("bslib")
install.packages("shinyjs")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("gtools")
install.packages("gridExtra")
install.packages("DT")


renv::install("tidyselect",dependencies = TRUE)
renv::install("jsonlite")
renv::install("cli")
renv::install("cpp11")
renv::install(c("generics","glue","lifecycle","magrittr","mime","pillar","R6","stringi","tibble","vctrs","withr"))
renv::install(c("pkgconfig","utf8"))
#renv
.libPaths()
renv::status(library = renv_lib)
renv::snapshot(library = renv_lib)
dep<-renv::dependencies()

library(rsconnect)
.libPaths()
#manifest

deps <- rsconnect::appDependencies()
deps[deps$Package == "data.table", ]

rsconnect::writeManifest(verbose = TRUE)
rsconnect::writeManifest(appDir = ".", verbose = TRUE)
