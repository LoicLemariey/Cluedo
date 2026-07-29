#ui



ui <- page_navbar(
    useShinyjs(),
    
    header = tags$head(
        includeCSS("www/styles.css")
    ),
    
    title = "Cluedo",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly"
    ),
    

    
    
#--------------INPUT -------------------------------------------------
    
    nav_panel(
        "Input",
        
        h5("Initialisation"),
        # fluidRow(
        #     column(
        #         width = 2,
        #         uiOutput("cardsInput")
        #     )
        # ),
        # checkboxGroupInput(
        #     "my_cards",
        #     "Mes cartes",
        #     choices = cartes_df$cartes
        # ),
        # 
        # checkboxGroupInput(
        #     "shared_cards",
        #     "Cartes communes",
        #     choices = cartes_df$cartes
        # ),
        
        div(class= "grid_init",
            uiOutput("cardsInput")),
        
        div(
        h5("Enter element of the investigation"),
        div(class= "investigation",
            uiOutput("investigationInput")),

        br(),
        DTOutput("suggestionTable")),
        
        actionButton(
            "btn_contrainte",
            "Compute summary",
            style = "width:auto;"
        ),
        br(),
        # h5("table 1"),
        # tableOutput("tab_card_cleaned"),
        
        fluidRow(
            column(
                width = 7,
                h5("Card Deduction Grid"),
                div(
                    class = "grid_container",
                    DTOutput("tab_card_cleaned2")
                )
            ),
            
            column(
                width = 5,
                h5("Logical contraints"),
                tableOutput("tab_logical_clause")
            )
        )
        
        

            
            #titlePanel("States"),
        
        
        # fluidRow(
        #     
        #     column(
        #         width = 9,
        #         
        #         fileInput(
        #             "file",
        #             "Enter State file",
        #             accept = ".xlsx"
        #         )
        #     ),
        #     
        #     column(
        #         width = 3,
        #         br(),
        #         downloadButton(
        #             outputId = "download_template",
        #             label = "Télécharger le template"
        #         )
        #     )
        # ),
        

    ),
    
#------------SIMULATIONS--------------------------------------------------------
    

    nav_panel(
        "Solution probability",
        div(
            style = "display:flex; align-items:center; gap:10px;",
            
            h5(
                "Compute probability of solution given investigation",
                style = "margin:0;"
            ),
            
            actionButton(
                "btn_computation",
                "Compute",
                style = "width:auto;"
            )
        ),
        
        
        #---------------conditionnal pannel-------------------------------------
        
        conditionalPanel(
            condition = "input.btn_computation > 0",
            uiOutput("solution_stats"),
            textOutput("txt_next_suggest"),
            h5("Solution"),
            tableOutput("tab_solution_df"),
            h5("Distribution probability"),
            #tableOutput("tab_distribution_proba")
            div(
                class = "grid_container",
                DTOutput("proba_detail")
            )
        )
    ),




#--------------Param -------------------------------------------------    
nav_panel(
    "parameters",
    h5("Parameters"),
    selectInput(
        inputId = "num_players",
        label = "Number of players",
        choices = 2:6,
        selected = 4
    )
)



)