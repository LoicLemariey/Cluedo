#config

n_dimension<-2
lieux<-paste("l",1:3,sep="")
armes<-paste("a",1:5,sep="")
cartes<-c(lieux,armes)

cartes_df<-data.frame(cartes=cartes,
                      type=c(rep("lieux",length(lieux)),
                             rep("armes", length(armes))))
nb_categories<-length(unique(cartes_df$type))
n_joueurs<-4
n_tot_combi<-nb_total_combi(cartes_df,n_joueurs)

scenario_per_solution_start<-n_tot_combi/(3*5)