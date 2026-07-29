server <- function(input, output, session){
    
        
    suggestions <- reactiveVal(
        data.frame(
            id = integer(),
            Character = character(),
            Weapon = character(),
            Room = character()
        )
    )
    
    clause_init<-reactiveVal(build_clauses())
    clause<-reactiveVal(build_clauses())
    globales_init<-reactiveVal()
    globales<-reactiveVal()
    contrainte<-reactiveVal()
    
    #sortie apres compuation
    solution_df<-reactiveVal()
    entropy<-reactiveVal()
    remaining_combination<-reactiveVal()
    remaining_solution<-reactiveVal()
    entropy_txt<-reactiveVal()
    combination_txt<-reactiveVal()
    solution_txt<-reactiveVal()
    proba_detail<-reactiveVal()
    next_suggestion<-reactiveVal()
    
    n_players<-reactiveVal()
    observe(n_players(as.numeric(input$num_players)))

    
    
    
    observe({
        req(n_players())
        globales_init(build_globales(cartes_df,n_players()))
        globales(build_globales(cartes_df,n_players()))
        })
    


    
    # # Interface dynamique
    # output$investigationInput <- renderUI({
    #     
    #     req(input$num_players)
    #     
    #     nb_inputs <- 3 + as.numeric(input$num_players)
    #     
    #     width <- floor(12 / nb_inputs)
    #     
    #     
    #     fluidRow(
    #         
    #         column(
    #             width = width,
    #             selectInput(
    #                 "room",
    #                 "Room",
    #                 choices = lieux
    #             )
    #         ),
    #         
    #         
    #         column(
    #             width = width,
    #             selectInput(
    #                 "weapon",
    #                 "Weapon",
    #                 choices = armes
    #             )
    #         ),
    #         
    #         
    # 
    #         
    #         column(
    #             width = width,
    #             selectInput(
    #                 "character",
    #                 "Character",
    #                 choices = characters
    #             )
    #         ),
    #         
    #         
    #         lapply(1:input$num_players, function(i){
    #             
    #             column(
    #                 width = width,
    #                 
    #                 selectInput(
    #                     paste0("player_", i),
    #                     paste("Player", i),
    #                     choices = response_choices
    #                 )
    #             )
    #             
    #         }),
    #         
    #         
    #         column(
    #             width = 12,
    #             
    #             actionButton(
    #                 "addSuggestion",
    #                 "Add investigation",
    #                 class = "btn-primary"
    #             )
    #             
    #         )
    #         
    #     )
    #     
    # })
    
    
    
    
    output$investigationInput <- renderUI({
        
        req(input$num_players)
        
        div(
            style = "
            display: flex;
            align-items: flex-end;
            gap: 10px;
            flex-wrap: wrap;
        ",
            
            div(
                style = "flex: 1; min-width: 120px;",
                selectInput(
                    "room",
                    "Room",
                    choices = lieux
                )
            ),
            
            div(
                style = "flex: 1; min-width: 120px;",
                selectInput(
                    "weapon",
                    "Weapon",
                    choices = armes
                )
            ),
            
            div(
                style = "flex: 1; min-width: 120px;",
                selectInput(
                    "character",
                    "Character",
                    choices = characters
                )
            ),
            
            lapply(seq_len(input$num_players), function(i) {
                
                div(
                    style = "flex: 1; min-width: 120px;",
                    selectInput(
                        paste0("player_", i),
                        paste("Player", i),
                        choices = response_choices
                    )
                )
                
            }),
            
            div(
                style = "padding-bottom: 20px;",
                actionButton(
                    "addSuggestion",
                    "Add investigation",
                    class = "btn-primary"
                )
            )
            
        )
        
    })
    
    
    # Ajout d'une enquête
    observeEvent(input$addSuggestion, {
        
        old <- suggestions()
        
        
        new_id <- if(nrow(old) == 0){
            1
        } else {
            max(old$id) + 1
        }
        
        
        new_row <- data.frame(
            id = new_id,
            Room = input$room,
            Weapon = input$weapon,
            Character = input$character,
            stringsAsFactors = FALSE
        )
        
        
        for(i in 1:input$num_players){
            
            new_row[[paste0("Player_", i)]] <-
                input[[paste0("player_", i)]]
            
        }
        
        
        suggestions(
            rbind(old, new_row)
        )
        
    })
    
    
    
    
    # Affichage tableau avec croix à droite
    output$suggestionTable <- renderDT({
        df <- suggestions()
        
        req(nrow(df) > 0)
        
        
        df_display <- df
        
        
        # colonne suppression
        df_display$Delete <- paste0(
            '<button class="delete_btn" id="delete_',
            df$id,
            '">❌</button>'
        )
        
        
        # On met Delete en dernière colonne
        df_display <- df_display[, c(
            setdiff(names(df_display), c("id", "Delete")),
            "Delete"
        )]
        
        
        datatable(
            
            df_display,
            escape = FALSE,
            rownames = FALSE,
            selection = "none",
            options = list(
                dom = "t",        # affiche uniquement le tableau
                ordering = FALSE, # désactive le tri
                paging = FALSE    # désactive les pages
            ),
            
            
            callback = JS("
        table.on('click', '.delete_btn', function() {

          var id = this.id.replace('delete_', '');

          Shiny.setInputValue(
            'delete_row',
            id,
            {priority: 'event'}
          );

        });
      ")
            
        )
        
    })
    
    
    
    
    # Suppression d'une ligne
    observeEvent(input$delete_row, {
        df <- suggestions()
        df <- df[
            df$id != as.numeric(input$delete_row),
        ]
        
        
        suggestions(df)
        
    })
    
    
    
    
    observeEvent(input$num_players, {

        
        
        suggestions(
            data.frame(
                id = integer(),
                Character = character(),
                Weapon = character(),
                Room = character()
            )
        )
        
    })
    
        #----------calculer les contraintes-------------------------------------
    observeEvent(input$btn_contrainte, {
            
            
            my_cards <- cartes_df$cartes[
                sapply(cartes_df$cartes, function(x) {
                    isTRUE(input[[paste0("my_", x)]])
                })
            ]
            
            shared_cards <- cartes_df$cartes[
                sapply(cartes_df$cartes, function(x) {
                    isTRUE(input[[paste0("shared_", x)]])
                })
            ]
            

            
            contrainte(build_contrainte_from_ui(my_cards,
                                     shared_cards,
                                     suggestions(),
                                     globales(),
                                     clause(),
                                     n_dimension,
                                     n_players()))
            
            # print(contrainte())
            #View(contrainte()$clause$cartes)
        
    })
    
    output$cardsInput <- renderUI({
        
        req(cartes_df$cartes)
        
        div(
            style = "
            width: 250px;
            height: 550px;
            overflow: auto;
        ",
            tags$style("
            table {
                font-size: 12px;
            }
            td, th {
                padding: 2px 5px !important;
            }
        "),
            
            tableOutput("cards_table")
        )
        
    })
    
    
    
    # init----------------------------------------------------------------------
    # output$cards_table <- renderTable({
    #     
    #     data.frame(
    #         Carte = cartes_df$cartes,
    #         My_cards = sapply(cartes_df$cartes, function(x) {
    #             as.character(
    #                 checkboxInput(
    #                     paste0("my_", x),
    #                     label = NULL,
    #                     value = FALSE
    #                 )
    #             )
    #         }),
    #         Shared = sapply(cartes_df$cartes, function(x) {
    #             as.character(
    #                 checkboxInput(
    #                     paste0("shared_", x),
    #                     label = NULL,
    #                     value = FALSE
    #                 )
    #             )
    #         }),
    #         stringsAsFactors = FALSE
    #     )
    #     
    # }, 
    # sanitize.text.function = function(x) x)
    
    
    output$cards_table <- renderTable({
        
        data.frame(
            Carte = cartes_df$cartes,
            
            My_cards = sapply(cartes_df$cartes, function(x) {
                paste0(
                    '<input type="checkbox" id="my_', x, 
                    '" style="transform:scale(0.8);">'
                )
            }),
            
            Shared = sapply(cartes_df$cartes, function(x) {
                paste0(
                    '<input type="checkbox" id="shared_', x,
                    '" style="transform:scale(0.8);">'
                )
            }),
            
            stringsAsFactors = FALSE
        )
        
    },
    sanitize.text.function = function(x) x)
    
    #outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
 
    
    # output$tab_card_cleaned<-renderTable({
    #     req(contrainte())
    #     icon_cluedo(contrainte()$globales$globales)
    # 
    # },, rownames = TRUE)
    
    
    
   
   
   #----------------2) computation-------------------------------------------------
   observeEvent(input$btn_computation, {
      
       solution_df(fill_all_probabilty(contrainte()))
       proba_detail(compute_proba_per_card(contrainte()))
       
   })
   
   
   observe({
       
       req(proba_detail())
       metric<-merge(cartes_df,metrique_cartes(proba_detail()[,-c(1,2)]))
       next_suggestion(suggestion_max(metric,proba_detail()[,-c(1,2)])$cartes)

   })
    


   
   observeEvent(solution_df(),{
       entropy(compute_entropy(solution_df()$Probability/100))
       remaining_solution(length(solution_df()$Probability))
       remaining_combination(sum(solution_df()$`Number of combination`))
       
       solution_txt(paste0("🎯",
                           "Remaining solutions: ",
                           remaining_solution(),
                           " / ",
                           n_solution_start))
       combination_txt(paste0("🧩",
                           "Remaining Combinations: ",
                           remaining_combination(),
                           " / ",
                           n_tot_combi))
       entropy_txt(paste0("🔎",
                          "Entropy: ",
                          round(entropy(),2)))
   })
   
   
   
   
   # ------------render---------------------------------------------------------
   

   output$solution_stats <- renderUI({
       tagList(
           div(entropy_txt()),
           div(solution_txt()),
           div(combination_txt())
       )
       
   })
   
   
   output$tab_solution_df<-renderTable({
       req(solution_df())
       solution_df() 
   })
   
   outputOptions(output, "tab_solution_df", suspendWhenHidden = FALSE)
   outputOptions(output, "solution_stats", suspendWhenHidden = FALSE)
   
   output$txt_next_suggest<-renderText({
       req(next_suggestion())
       txt<-paste(next_suggestion(), collapse = "/")
       paste0("💡 Next hypothesis: ", txt)
   })
   

   
   
   
   
   

   
   
   output$tab_logical_clause<-renderTable({
       req(contrainte())
       data<-clean_clause_table(contrainte()$clause)
       data
   })
   
   
   
   output$tab_card_cleaned2<-renderDT({
       req(contrainte())
       tab<-icon_cluedo2(contrainte()$globales$globales)
       names(tab)<-friendly_names(names(tab))
       
       DT::datatable(
           tab,
           rownames = FALSE,
           escape = FALSE,
           class = "display no-stripe",
           options = list(
               dom = "t",
               paging = FALSE,
               ordering = FALSE,
               searching = FALSE,
               autoWidth = TRUE,
               columnDefs = list(
                   list(
                       targets = 1,
                       visible = FALSE
                   ),
                   list(
                       className = "dt-center",
                       targets = "_all"
                   )
               )
           )
       ) %>%
           DT::formatStyle(
               "type",
               target = "row",
               backgroundColor = DT::styleEqual(
                   c("suspect", "armes", "lieux"),
                   c("#f8e6e6", "#e6f0f8", "#e8f845")
               )
           )
   })
   
   
   
   
   
   
   output$proba_detail<-renderDT({
       req(proba_detail())
       tab<-proba_detail()
       names(tab)<-friendly_names(names(tab))
       
       
       DT::datatable(
           tab,
           rownames = FALSE,
           escape = FALSE,
           class = "display no-stripe",
           options = list(
               dom = "t",
               paging = FALSE,
               ordering = FALSE,
               searching = FALSE,
               autoWidth = TRUE,
               columnDefs = list(
                   list(
                       targets = 1,
                       visible = FALSE
                   ),
                   list(
                       className = "dt-center",
                       targets = "_all"
                   )
               )
           )
       ) %>%
           DT::formatStyle(
               "type",
               target = "row",
               backgroundColor = DT::styleEqual(
                   c("suspect", "armes", "lieux"),
                   c("#f8e6e6", "#e6f0f8", "#e8f845")
               )
           )
   })
   
   
   
   # observeEvent(input$btn_cards, {
   #     
   #     showModal(
   #         modalDialog(
   #             title = "Pick initial cards",
   #             
   #             uiOutput("cardsInput"),
   #             
   #             
   #             size = "m",
   #             style = "width: 300px;
   #                      display: flex;
   #                      justify-content: center;
   #                      align-items: center;",
   #             easyClose = TRUE,
   #             footer = NULL
   #         )
   #     )
   #     
   # })
   
   
   observeEvent(input$btn_cards, {
       
       showModal(
           modalDialog(
               title = "Pick initial cards",
               
               div(
                   style = "
                    display: flex;
                    justify-content: center;
                    width: 100%;
                ",
                   uiOutput("cardsInput")
               ),
               
               easyClose = TRUE,
               footer = NULL,
               
               tags$style("
                .modal-dialog {
                    width: 300px;
                    max-width: 300px;
                }

                .modal-body {
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    padding: 10px;
                }
            ")
           )
       )
       
   })
   
   shinyjs::disable("btn_computation")
   observe({
       shinyjs::toggleState(
           id = "btn_computation",
           condition = input$btn_contrainte > 0
       )
   })


}
