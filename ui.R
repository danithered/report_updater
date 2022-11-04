library(shiny)

shinyUI(fluidPage(
    #tags$style(height="100vh", width="100vw"),

    # Application title
    h3("MCRS max paramterised reports"),
    
    # Settings
    #position= "fixed-top",
div(
    # Menu
    div(fluidRow(
            column(4,
               uiOutput("reports"),
            ),
            column(4,
               # numericInput("abrmax",
               #      label= "Set maximum of timepoints to plot (add negative value to show them all)",
               #      value= -1
               # ),
               # 
               # numericInput("abrmin",
               #      label= "Set minimum of timepoints to plot",
               #      value= 0
               # ),
               uiOutput("scale")
            ),
            column(4,
               # checkboxInput("kompl",
               #     label= "Would you like to output complemeter plots too? (lots of time)",
               #     value= FALSE
               # ),
               uiOutput("kompl"),
               actionButton("knit", "See report")
            )
        
        , style="position:static; width:inherit;background-color:lightblue")
    ),
    tableOutput("parameters"),
    htmlOutput("rep", style="overflow-y: scroll; height:75%; padding:10px")
, style="height:90vh")
))
