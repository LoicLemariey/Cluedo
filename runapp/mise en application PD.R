 # mise en application
#programmation dynamqiue

rm(list=ls())
library(gridExtra)
library(gtools)
library(stringr)
library(dplyr)



# function----------------------------------------------------------------------

nb_total_combi<-function(cartes_df,n_joueurs){
    n_type<-length(unique(cartes_df$type))
    rep<-table(cartes_df$type)
    solution<-prod(rep)
    nb_cartes_restantes<-length(cartes_df$cartes)-n_type
    distribution<-c(rep(nb_cartes_restantes%/%n_joueurs,n_joueurs),
                    nb_cartes_restantes%%n_joueurs)
    distribution<-distribution[distribution!=0]
    res<-solution*factorial(nb_cartes_restantes)/prod(factorial(distribution))
    return(res)
}

build_clauses<-function(){
    contraintes <- data.frame(
        joueur = character(),
        cartes = I(list())
    )
    return(contraintes)
}
build_globales<-function(cartes_df,n_joueurs){
    type<-unique(cartes_df$type)
    n_type<-length(type)
    nb_cartes_restantes<-length(cartes_df$cartes)-n_type
    distribution<-c(rep(nb_cartes_restantes%/%n_joueurs,n_joueurs),
                    nb_cartes_restantes%%n_joueurs)
    quotas<-c(rep(1,n_type),distribution)
    ncol<-n_type+n_joueurs+as.numeric((nb_cartes_restantes%%n_joueurs>0))
    colnames<-rep("Communes",ncol)
    colnames[1:(n_type+n_joueurs)]<-c(paste0("S_",type),paste0("J",1:n_joueurs))
    
    nrow<-length(cartes_df$cartes)
    globales<-matrix(0,nrow = nrow,
                     ncol= ncol)
    colnames(globales)<-colnames
    rownames(globales)<-cartes_df$cartes
    
    
    for ( rtype in type){
        index<-which(cartes_df$type!=rtype)
        globales[index,str_detect(colnames,rtype)]<--1
    }
    
    quotas<-quotas[1:ncol]
    names(quotas)<-colnames
    res<-list(globales,quotas)
    names(res)<-c("globales","quotas")
    return(res)
    
}



ajouter_clause <- function(df, joueur, cartes) {
    nouvelle_ligne <- data.frame(
        joueur = joueur,
        cartes = I(list(cartes))
    )
    rbind(df, nouvelle_ligne)
}
ajouter_globale<-function(globales,joueur,carte,own_bool){
    
    #mets +1
    if(own_bool){
        if(globales$globales[carte,joueur]!=1){
            globales$globales[carte,joueur]<-1
            globales$quotas[joueur]<-globales$quotas[joueur]-1
        }
        
        
        
        globales$globales[carte,
                          which(colnames(globales$globales)!=joueur)]<--1
        
    }else{#mets des -1
        globales$globales[carte,joueur]<--1
    }
    
    return(globales)
    
}



#cette function n'est pas exhaustive
propagation_manuelle<-function(globales,clause){
    
    n_clause<-dim(clause)[1]
    #1) mettre -1 partoiut si le quotas vaut 0
    dim<-dim(globales$globales)
    for (i in 1:dim[2]){
        if(globales$quotas[i]==0){
            globales$globales[which(globales$globales[,i]==0),i]<--1
        }
        
        #2 si le nombre quotas = nombre restant
        nombre_possibilites<-sum(globales$globales[,i]!=-1)
        if(nombre_possibilites==globales$quotas[i]){
            globales$globales[which(globales$globales[,i]==0),i]<-1
            globales$quotas[i]<-0
        }
        
        
        
    }
    
    #3 si une carte a plus qu'une seule possibilité
    for (i in 1:dim[1]){
        
        
        #3bis si il y a un 1, on met des -1 partour
        is1<-!prod(globales$globales[i,]!=1)
        if(is1){
            globales$globales[i,globales$globales[i,]!=1]<--1
        }else{
            index_zero<-globales$globales[i,]==0
            if(sum(index_zero)==1){
                globales$globales[i,index_zero]<-1
                globales$quotas[index_zero]<-globales$quotas[index_zero]-1
            }
        }
        
        
        
        
        
        
    }
    
    
    if(n_clause>0){
        for (i in 1:n_clause){
            
            
            #4 on enleve ce qui n'est pas possible dans les clause
            index_joueur<-colnames(globales$globales)==clause$joueur[i]
            col_joueurs<-globales$globales[,index_joueur]
            eliminated<-names(col_joueurs[col_joueurs==-1])
            # print(clause$cartes[i][[1]])
            # print(eliminated)
            # print(setdiff(clause$cartes[i][[1]],eliminated))
            clause$cartes[i][[1]]<-setdiff(clause$cartes[i][[1]],eliminated)
            
            #5 si il y a qu'une seul possibilite on affecte 1 si ce n'est pas deja 1
            if(length(clause$cartes[i][[1]])==1){
                index_carte<-rownames(globales$globales)%in%clause$cartes[i][[1]]
                if(globales$globales[index_carte,index_joueur]!=1){
                    globales$globales[index_carte,index_joueur]<-1
                    globales$quotas[index_joueur]<-globales$quotas[index_joueur]-1
                }
            }
        }
    }
    
    
    res<-list(globales,clause)
    names(res)<-c("globales","clause")
    return(res)
    
}

boucle_propagation<-function(globales,clause){
    stop<-FALSE
    tour<-1
    #print(globales)
    while(!stop){
        print(paste("tour :",tour))
        res<-propagation_manuelle(globales,clause)
        new_globales<-res$globales
        #print(new_globales)
        new_clause<-res$clause
        globales_identiques<-prod(globales$globales==res$globales$globales)
        quotas_identiques<-prod(globales$quotas==res$globales$quotas)
        clause_identique<-identical(clause,res$clause)
        if(!globales_identiques|
           !quotas_identiques|
           !clause_identique){
            globales<-new_globales
            clause<-new_clause
            
            # print(globales)
            # print(clause)
        }else{
            stop<-TRUE
        }
        tour<-tour+1
    }
    return(res)
}

order_cards_by_zeros <- function(admissible_df) {
    
    # S'assurer que c'est une matrice pour la vitesse
    mat <- as.matrix(admissible_df)
    
    # Compter les 0 par carte (colonne)
    zero_count <- rowSums(mat == 0)
    
    # Garder uniquement les cartes avec au moins un 0
    keep <- zero_count > 0
    
    if (!any(keep)) {
        return(admissible_df[, FALSE, drop = FALSE])
    }
    
    # Ordre croissant du nombre de 0
    ord <- order(zero_count[keep])
    
    # Retourner le dataframe réordonné
    admissible_df[keep, , drop = FALSE][ord, , drop = FALSE]
}
clean_clauses <- function(admissible, clauses_df) {
    
    keep <- rep(TRUE,nrow(clauses_df))
    
    for (m in seq_len(nrow(clauses_df))) {
        j <- clauses_df$joueur[m]
        S <- clauses_df$cartes[[m]]
        
        # Clause satisfaite ?
        if (any(admissible[S,j] == 1)) {
            keep[m] <- FALSE
        } 
    }
    
    clauses_df[keep, , drop = FALSE]
}

# fonction pour créer une clé unique pour chaque état
state_key <- function(k, q, sat) {
    paste(k, paste(q, collapse = ","), paste(sat, collapse = ""), sep = "|")
}


DP_pre_computation<-function(contraintes_propage){
    clean_globales<-order_cards_by_zeros(contraintes_propage$globales$globales)
    
    #on enleve les clauses valide
    cleaned_clauses<-clean_clauses(contraintes_propage$globales$globales,
                                   contraintes_propage$clause)
    #on renomme
    admissible<-clean_globales
    quotas<-contraintes_propage$globales$quotas
    clauses_df<-cleaned_clauses
    C <- nrow(admissible)      # nombre de cartes libres
    J <- ncol(admissible)      # nombre de joueurs         
    M <- nrow(clauses_df)       # nombre de clauses
    
    
    
    #element pour accélerer la vérification des clauses
    player_names <- names(quotas)
    player_index <- setNames(
        seq_along(player_names),
        player_names
    )
    clause_player <- if(M > 0) player_index[clauses_df$joueur] else integer(0)
    clause_card <- matrix(FALSE, nrow = M, ncol = C)
    if (M > 0) {
        for (m in 1:M) {
            index_cards<-which(row.names(admissible)%in%clauses_df$cartes[[m]])
            clause_card[m, index_cards] <- TRUE
        }
    }
    
    initial_sat <- if(M > 0) rep(0, M) else integer(0)
    k<-1#nrow(contraintes_propage$globales$globales)- nrow(clean_globales)
    
    liste_out<-list(M =M, C= C, J= J,
                    admissible = admissible,
                    clause_player = clause_player,
                    clause_card=clause_card,
                    initial_sat= initial_sat,
                    k=k,
                    q=quotas)
    #print(liste_out)
    return(liste_out)
}

count_assignments <- function(k,
                              q,
                              sat,
                              admissible,
                              clause_card,
                              clause_player,
                              C,
                              M,
                              J,
                              memo) {
    #print(paste0("k",k))
    # print(environment())
    # print(exists("memo"))
    # print(length(ls(memo)))
    
    
    key <- state_key(k, q, sat)
    #print(key)
    if (exists(key, memo)) return(memo[[key]])
    
    # cas terminal : toutes les cartes traitées
    if (k > C) {
        res <- as.integer(all(q == 0) && all(sat == 1))
        memo[[key]] <- res
        return(res)
    }
    
    total <- 0
    
    # pour chaque joueur (colonnes)
    for (j in 1:J) {
        
        
        if (q[j] == 0) next                   # quota rempli
        if (admissible[k, j]==-1) next           # impossible pour ce joueur
        
        
        #print(paste("joueurs",j))
        q2 <- q
        q2[j] <- q2[j] - 1
        
        sat2 <- sat
        if (M > 0) {
            for (m in which(sat == 0 & clause_player == j)) {
                if (clause_card[m, k]) sat2[m] <- 1
            }
        }
        
        # pruning : vérifier clauses impossibles
        impossible <- FALSE
        if (M > 0) {
            for (m in which(sat2 == 0)) {
                remaining_cards <- k:C
                j2 <- clause_player[m]
                
                card_solution_for_m<-  clause_card[m, remaining_cards]
                possible_for_j <- admissible[remaining_cards, j2] >= 0
                carte_solution_possible<-card_solution_for_m&possible_for_j
                
                if (!any(carte_solution_possible)) {
                    impossible <- TRUE
                    break
                }
            }
        }
        
        if (!impossible) {
            total <- total + count_assignments(k + 1,
                                               q2,
                                               sat2,
                                               admissible,
                                               clause_card,
                                               clause_player,
                                               C,
                                               M,
                                               J,
                                               memo)
        }
        
        
        #dans le pruning il faut vérifier que tous les cotas des joueurs qui ont des clauses a respecter sont non nuls
    }
    
    memo[[key]] <- total
    total
}

count_assignments_without_pruning <- function(k, q, sat) {
    #print(paste0("k",k))
    
    key <- state_key(k, q, sat)
    #print(key)
    if (exists(key, memo)) return(memo[[key]])
    
    # cas terminal : toutes les cartes traitées
    if (k > C) {
        #print("calcul de fin")
        res <- as.integer(all(q == 0) && all(sat == 1))
        memo[[key]] <- res
        return(res)
    }
    
    total <- 0
    
    # pour chaque joueur (colonnes)
    for (j in 1:J) {
        if (q[j] == 0) next                   # quota rempli
        if (admissible[k, j]==-1) next           # impossible pour ce joueur
        
        
        
        #prepare les nouvelles conditions pour le nouvelle appel
        q2 <- q
        q2[j] <- q2[j] - 1
        
        sat2 <- sat
        if (M > 0) {
            for (m in which(sat == 0 & clause_player == j)) {
                if (clause_card[m, k]) sat2[m] <- 1
            }
        }

        total <- total + count_assignments_without_pruning(k + 1, q2, sat2)
    }
    
    memo[[key]] <- total
    return(total)
}


build_all_possible_solution<-function(my_global){
    liste_sol<-list()
    for(i in 1:n_dimension){
        vec<-my_global[,i]
        vec<-names(vec[vec!=-1])
        liste_sol[[i]]<-vec
    }
    df <- do.call(expand.grid, liste_sol)
    return(df)
}

find_proba_to_one_solution<-function(vecteur_solution,contraintes_propage){
    memo <- new.env(hash = TRUE)
    print(vecteur_solution)
    print("new memo")
    print(length(memo))
    n<-length(vecteur_solution)
    globales<-contraintes_propage$globales
    for(i in 1:n){
        globales<-ajouter_globale(globales,colnames(globales$globales)[i],
                                  vecteur_solution[i],TRUE)
    }
    
    contraintes_propage_final<-boucle_propagation(globales,contraintes_propage$clause)
    
    dp_preli<-DP_pre_computation(contraintes_propage_final)
    
    
    memo <- new.env(hash = TRUE)
    nombre_combi<-count_assignments(q = dp_preli$q,
                                    k=dp_preli$k,
                                    sat =dp_preli$initial_sat,
                                    admissible = dp_preli$admissible,
                                    clause_card = dp_preli$clause_card,
                                    clause_player = dp_preli$clause_player,
                                    C = dp_preli$C,
                                    J=dp_preli$J,
                                    M = dp_preli$M,
                                    memo = memo)
    
    print(nombre_combi)
    return(nombre_combi)
}



fill_all_probabilty<-function(contraintes_propage){
    
    
    df<-build_all_possible_solution(contraintes_propage$globales$globales)
    df$nb_combi<-0
    df$nb_combi <- apply(
        df[,1:n_dimension],
        1,
        function(x) find_proba_to_one_solution(
            vecteur_solution = x,
            contraintes_propage = contraintes_propage
        )
    )
    return(df)
}


entropy <- function(p) {
    p <- p[p > 0]   # retire les zéros pour éviter log(0)
    -sum(p * log(p))
}


progress_enquete<-function(df_solution,n_tot_combi,scenario_per_solution_start){
    
    n_current<-sum(df_solution$nb_combi)-max(df_solution$nb_combi)
    denom<-n_tot_combi-scenario_per_solution_start
    progress<-1-n_current/denom
    return(progress)
}


#------------description du jeux------------------------------------------------
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


#----------ajouter des contraintes----------------------------------------------

clause<-build_clauses()
globales<-build_globales(cartes_df,4)
globales$globales
globales$quotas
clause



# -----------------scenario start-----------------------------------------------
memo <- new.env(hash = TRUE)
contrainte_start<-list(globales,clause)
names(contrainte_start)<-c("globales","clause")



preli_start<-DP_pre_computation(contrainte_start)
count_assignments(q = preli_start$q,
                  k=preli_start$k,
                  sat =preli_start$initial_sat,
                  admissible = preli_start$admissible,
                  clause_card = preli_start$clause_card,
                  clause_player = preli_start$clause_player,
                  C = preli_start$C,
                  J=preli_start$J,
                  M = preli_start$M,
                  memo=memo)

df_start<-fill_all_probabilty(contrainte_start)
#---------------------------------scenario 1------------------------------------
globales<-ajouter_globale(globales,"J1","a1",TRUE)
globales<-ajouter_globale(globales,"J2",c("a2","l1"),FALSE)
clause<-ajouter_clause(clause,"J3",c("a3","l2"))


#(globales_test<-ajouter_globale(globales,"S_lieux","l3",TRUE))
#globales<-ajouter_globale(globales,"Communes","a3",TRUE)
#globales<-ajouter_globale(globales,"Communes","l3",TRUE)
globales$globales
globales$quotas
clause


#-------------------Propagation de contraintes----------------------------------

contraintes_propage<-boucle_propagation(globales,clause)
contraintes_propage$clause
contraintes_propage$globales$quotas
contraintes_propage$globales$globales







memo <- new.env(hash = TRUE)
length(memo)

preli_s1<-DP_pre_computation(contraintes_propage)
count_assignments(q = preli_s1$q,
                  k=preli_s1$k,
                  sat =preli_s1$initial_sat,
                  admissible = preli_s1$admissible,
                  clause_card = preli_s1$clause_card,
                  clause_player = preli_s1$clause_player,
                  C = preli_s1$C,
                  J=preli_s1$J,
                  M = preli_s1$M,
                  memo = memo)
memo <- new.env(hash = TRUE)
df_s1<-fill_all_probabilty(contraintes_propage)


#entropy
entropy_start<-entropy(df_start$nb_combi/sum(df_start$nb_combi))
entropy(df_s1$nb_combi/sum(df_s1$nb_combi))




#scenario 2 --------------------------------------------------------------------

globales<-ajouter_globale(globales,"S_lieux","l3",TRUE)
globales<-ajouter_globale(globales,"S_armes","a4",TRUE)
contraintes_s2<-boucle_propagation(globales,clause)

memo <- new.env(hash = TRUE)
df_s2<-fill_all_probabilty(contraintes_s2)


preli_s2<-DP_pre_computation(contraintes_s2)
count_assignments(q = preli_s2$q,
                  k=preli_s2$k,
                  sat =preli_s2$initial_sat,
                  admissible = preli_s2$admissible,
                  clause_card = preli_s2$clause_card,
                  clause_player = preli_s2$clause_player,
                  C = preli_s2$C,
                  J=preli_s2$J,
                  M = preli_s2$M)


memo <- new.env(hash = TRUE)
find_proba_to_one_solution(c("l3","a4"),contraintes_propage)



#-------------------------------------------------------------------------------

memo <- new.env(hash = TRUE)
find_proba_to_one_solution(c("l3","a4"),contraintes_propage)
memo <- new.env(hash = TRUE)
find_proba_to_one_solution(c("l2","a2"),contraintes_propage)
memo <- new.env(hash = TRUE)
find_proba_to_one_solution(c("l2","a3"),contraintes_propage)
memo <- new.env(hash = TRUE)
find_proba_to_one_solution(c("l3","a3"),contraintes_propage)
memo <- new.env(hash = TRUE)
find_proba_to_one_solution(c("l1","a2"),contraintes_propage)
memo <- new.env(hash = TRUE)
find_proba_to_one_solution(c("l3","a2"),contraintes_propage)



#-----------DP- preliminaire----------------------------------------------------
#les cartes sont ordonnées et filtrées pour augmenter la rapidité.







clean_globales<-order_cards_by_zeros(contraintes_propage$globales$globales)

#on enleve les clauses valide
cleaned_clauses<-clean_clauses(contraintes_propage$globales$globales,
                               contraintes_propage$clause)
#on renomme
admissible<-clean_globales
quotas<-contraintes_propage$globales$quotas
clauses_df<-cleaned_clauses
C <- nrow(admissible)      # nombre de cartes libres
J <- ncol(admissible)      # nombre de joueurs         
M <- nrow(clauses_df)       # nombre de clauses



#element pour accélerer la vérification des clauses
player_names <- names(quotas)
player_index <- setNames(
    seq_along(player_names),
    player_names
)
clause_player <- if(M > 0) player_index[clauses_df$joueur] else integer(0)
clause_card <- matrix(FALSE, nrow = M, ncol = C)
if (M > 0) {
    for (m in 1:M) {
        index_cards<-which(row.names(admissible)%in%clauses_df$cartes[[m]])
        clause_card[m, index_cards] <- TRUE
    }
}

# memoisation/ permet de gagner en rapidité avec l'adressage mémoire
memo <- new.env(hash = TRUE)
length(memo)




#-------DP----------------------------------------------------------------------



#test cas 1

memo <- new.env(hash = TRUE)
initial_sat <- if(M > 0) rep(0, M) else integer(0)
t1<-Sys.time()
result <- count_assignments_without_pruning(
    k = 1,
    q = quotas,
    sat = initial_sat
)
t2<-Sys.time()
difftime(t2,t1)
print(result)




#test avec pruning
memo <- new.env(hash = TRUE)
initial_sat <- if(M > 0) rep(0, M) else integer(0)
t1<-Sys.time()
result <- count_assignments(
    k = 1,
    q = quotas,
    sat = initial_sat
)
t2<-Sys.time()
difftime(t2,t1)
result

length(memo)
View(memo)