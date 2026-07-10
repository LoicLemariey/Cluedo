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
    
    n_players<-reactiveVal()
    observe(n_players(as.numeric(input$num_players)))
    
    
    
    
    observe({
        req(n_players())
        globales_init(build_globales(cartes_df,n_players()))
        globales(build_globales(cartes_df,n_players()))
        })
    


    
    # Interface dynamique
    output$investigationInput <- renderUI({
        
        req(input$num_players)
        
        nb_inputs <- 3 + as.numeric(input$num_players)
        
        width <- floor(12 / nb_inputs)
        
        
        fluidRow(
            
            column(
                width = width,
                selectInput(
                    "room",
                    "Room",
                    choices = lieux
                )
            ),
            
            
            column(
                width = width,
                selectInput(
                    "weapon",
                    "Weapon",
                    choices = armes
                )
            ),
            
            

            
            column(
                width = width,
                selectInput(
                    "character",
                    "Character",
                    choices = characters
                )
            ),
            
            
            lapply(1:input$num_players, function(i){
                
                column(
                    width = width,
                    
                    selectInput(
                        paste0("player_", i),
                        paste("Player", i),
                        choices = response_choices
                    )
                )
                
            }),
            
            
            column(
                width = 12,
                
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
            
            print(contrainte())
        
    })
    
    output$cardsInput <- renderUI({
        
        req(cartes_df$cartes)
        
        tagList(
            tableOutput("cards_table")
        )
        
    })
    
    
    
    # init----------------------------------------------------------------------
    output$cards_table <- renderTable({
        
        data.frame(
            Carte = cartes_df$cartes,
            My_cards = sapply(cartes_df$cartes, function(x) {
                as.character(
                    checkboxInput(
                        paste0("my_", x),
                        label = NULL,
                        value = FALSE
                    )
                )
            }),
            Shared = sapply(cartes_df$cartes, function(x) {
                as.character(
                    checkboxInput(
                        paste0("shared_", x),
                        label = NULL,
                        value = FALSE
                    )
                )
            }),
            stringsAsFactors = FALSE
        )
        
    }, 
    sanitize.text.function = function(x) x)
    
    #outputOptions(output, "show_panel", suspendWhenHidden = FALSE)
 
    
    
    


}