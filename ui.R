library(shiny)

shinyUI(fluidPage(
    #tags$style(height="100vh", width="100vw"),

    # Application title
    h3("Report updater interface"),
    
    # Settings
    #position= "fixed-top",
div(
    # Menu
    div(fluidRow(
            column(4,
              textOutput("updatetime"),
            ),
            column(4,
              actionButton("knit", "Update"),
              actionButton("git", "Version update")
            )
        
        , style="position:static; width:inherit;background-color:lightblue")
    ),
    DT::dataTableOutput("parameters"),
    tableOutput("par")
, style="height:90vh")
))
