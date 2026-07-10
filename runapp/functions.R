#function

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
    while(!stop){
        res<-propagation_manuelle(globales,clause)
        new_globales<-res$globales
        new_clause<-res$clause
        globales_identiques<-prod(globales$globales==res$globales$globales)
        quotas_identiques<-prod(globales$quotas==res$globales$quotas)
        clause_identique<-identical(clause,res$clause)
        if(!globales_identiques|
           !quotas_identiques|
           !clause_identique){
            globales<-new_globales
            clause<-new_clause
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

    
    
    key <- state_key(k, q, sat)
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
    
    key <- state_key(k, q, sat)

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


compute_proba_per_card<-function(contraintes_propage){
    proba_table<-contraintes_propage$globales$globales
    proba_table[proba_table == 0] <- NA
    proba_table[proba_table == -1] <- 0
    dp_preli<- DP_pre_computation(contraintes_propage)
    
    memo <- new.env(hash = TRUE)
    n_combi <- count_assignments(q = dp_preli$q,
                                 k=dp_preli$k,
                                 sat =dp_preli$initial_sat,
                                 admissible = dp_preli$admissible,
                                 clause_card = dp_preli$clause_card,
                                 clause_player = dp_preli$clause_player,
                                 C = dp_preli$C,
                                 J=dp_preli$J,
                                 M = dp_preli$M,
                                 memo = memo)
    for (i in seq_len(nrow(proba_table))) {
        for (j in seq_len(ncol(proba_table))) {
            if (is.na(proba_table[i, j])) {
                new_globales<-ajouter_globale(contraintes_propage$globales,
                                              colnames(proba_table)[j],
                                              rownames(proba_table)[i],TRUE)
                new_contrainte<-boucle_propagation(new_globales,clause)
                dp_preli<- DP_pre_computation(new_contrainte)
                
                memo <- new.env(hash = TRUE)
                proba_table[i, j] <- round(count_assignments(q = dp_preli$q,
                                                             k=dp_preli$k,
                                                             sat =dp_preli$initial_sat,
                                                             admissible = dp_preli$admissible,
                                                             clause_card = dp_preli$clause_card,
                                                             clause_player = dp_preli$clause_player,
                                                             C = dp_preli$C,
                                                             J=dp_preli$J,
                                                             M = dp_preli$M,
                                                             memo = memo)/n_combi,3)
            }
            
            
            
        }
    }
    
    return(proba_table)
}



metrique_cartes <- function(proba_table, voisin = "J2") {
    
    # Entropie de la carte
    H <- apply(proba_table, 1, function(p) {
        p <- p[!is.na(p) & p > 0]
        -sum(p * log2(p))
    })
    
    # Probabilité que le voisin possède la carte
    p_voisin <- proba_table[,voisin]
    
    # Probabilité que la carte soit dans la solution
    p_solution <- apply(proba_table[,1:n_dimension],1,sum)
    
    # Métrique
    metrique <- round(H * (p_voisin + 0.3 * p_solution),3)
    
    out<-data.frame(
        cartes = rownames(proba_table),
        H = H,
        P_voisin = p_voisin,
        P_solution = p_solution,
        Metrique = metrique,
        row.names = NULL
    )
    return(out)
}


suggestion_max <- function(cartes, proba_table,
                           moi = "J1",
                           joueur_lointain = "J4") {
    
    res <- list()
    
    for (t in unique(cartes$type)) {
        
        sous <- cartes[cartes$type == t, ]
        
        # Colonne solution correspondante
        col_solution <- switch(
            t,
            "armes" = "S_armes",
            "lieux" = "S_lieux",
            "suspects" = "S_suspects"
        )
        
        # Le type est-il résolu ?
        resolu <- any(proba_table[sous$cartes, col_solution] == 1)
        
        if (!resolu) {
            
            # Cas normal : meilleure métrique
            choix <- sous[which.max(sous$Metrique), ]
            
        } else {
            
            ## 1. Une carte de ma main
            idx <- sous$cartes[proba_table[sous$cartes, moi] == 1]
            
            if (length(idx) > 0) {
                
                choix <- sous[match(idx[1], sous$cartes), ]
                
            } else {
                
                ## 2. Une carte du joueur lointain
                p <- proba_table[sous$cartes, joueur_lointain]
                
                if (max(p, na.rm = TRUE) > 0) {
                    
                    idx <- which.max(p)
                    choix <- sous[idx, ]
                    
                } else {
                    
                    ## 3. Sinon meilleure métrique
                    choix <- sous[which.max(sous$Metrique), ]
                    
                }
            }
        }
        
        res[[t]] <- choix
    }
    
    do.call(rbind, res)
}


# my_cards<-"Lounge"
# shared<-"Billiard Room"

build_contrainte_from_ui<-function(my_cards,shared,suggestion,globales,clause,n_dimension,n_player){
    suggestion<-suggestion[,-1]
    
    for (cards in my_cards){
        globales<-ajouter_globale(globales,"J1",cards,TRUE)
    }
    
    for (cards in shared){
        globales<-ajouter_globale(globales,"Communes",cards,TRUE)
    }
    
    n_suggestion<-dim(suggestion)[1]
    if(n_suggestion>0){
        for(i in 1:n_suggestion){
            for(j in 1:n_player){
                running_suggest<-as.vector(t(suggestion[i, 1:n_dimension]))
                if(suggestion[i,n_dimension+j]=="No card"){
                    globales<-ajouter_globale(globales,
                                              paste0("J",j),
                                              running_suggest,
                                              FALSE)
                }
                
                if(suggestion[i,n_dimension+j]=="One of the three"){
                    clause<-ajouter_clause(clause,
                                           paste0("J",j),
                                           running_suggest)
                }
                if(suggestion[i,n_dimension+j]=="Character"){
                    globales<-ajouter_globale(globales,
                                              paste0("J",j),
                                              suggestion$Character[i],
                                              TRUE)
                }
                if(suggestion[i,n_dimension+j]=="Weapon"){
                    globales<-ajouter_globale(globales,
                                              paste0("J",j),
                                              suggestion$Weapon[i],
                                              TRUE)
                }
                if(suggestion[i,n_dimension+j]=="Room"){
                    globales<-ajouter_globale(globales,
                                              paste0("J",j),
                                              suggestion$Room[i],
                                              TRUE)
                }
                
                
                
            }
        }
        
        
        
    }
    
    
    contraintes_propage<-boucle_propagation(globales,clause)
    return(contraintes_propage)
    
    
}
