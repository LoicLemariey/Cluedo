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
    
#--------------TOURNAMENT STATE-------------------------------------------------
    
    nav_panel(
        "Input",
        h5("Enter element of the investigation"),


            
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
                "btn_simulation",
                "Simulate",
                style = "width:auto;"
            )
        ),
        
        
        #---------------conditionnal pannel-------------------------------------
        
        conditionalPanel(
            condition = "output.show_panel",
           
            #div(DTOutput("table_path"),height="700px")
        )
    )
)