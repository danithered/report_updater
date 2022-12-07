library(shiny)
library(plotly)
library(DT)

shinyUI(fluidPage(
  # Settings
  div(
    # Menu
    div(fluidRow(
      column(4,
             h4("State"),
             actionButton("pop", "Input data"),
             uiOutput("reports"),
             textOutput("state_data"),
             dataTableOutput("table"),
             plotlyOutput("plot")
      ),
      column(4, style="position:static;background-color:lightblue",
             h4("Cell"),
             tabsetPanel(
               tabPanel("MFE", plotOutput("hist_mfe")),
               tabPanel("Pfold", plotOutput("hist_Pfold")),
               tabPanel("R", plotOutput("hist_R")),
               tabPanel("active sites", plotOutput("hist_no_sites")),
               tabPanel("A", plotOutput("hist_no_acts")),
               tabPanel("Pdeg", plotOutput("hist_Pdeg"))
             ),
             plotOutput("nice")
      ),
      column(4,
             h4("Replicator"),
             dataTableOutput("reps"),
             tabsetPanel(
              tabPanel("activities", 
                plotOutput("replicator"),
                plotOutput("acts")
              ),
              tabPanel("properties",
                       textOutput("seq"),
                       textOutput("str"),
                       tableOutput("rep_props")       
              )
             )
      )
    ))
)))
