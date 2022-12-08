library(shiny)
library(shinydashboard)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Status", tabName = "status"),
    menuItem("Updated directories", tabName = "sources"),
    actionButton("knit", "Update reports"),
    "Last updated:",
    textOutput("updatetime"),
    actionButton("git", "Version update")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "status",
            fluidPage(
              fluidRow(
                DT::dataTableOutput("parameters")
              ),
              fluidRow(
                tableOutput("par")
              )
            )    
    ),
    
    tabItem(tabName = "sources",
            fluidPage(
                DT::dataTableOutput("par_to_choose")
            )
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Report updater interface"),
  sidebar,
  body
)



# ui <- fluidPage(
#   
#   # App title ----
#   titlePanel("Report updater interface"),
#   
#   
#   
#   # Output: Tabset w/ plot, summary, and table ----
#   tabsetPanel(type = "tabs", 
#               tabPanel("Status", fluid=T,{
#                 fluidPage(
#                   # Menu
#                   br(),
#                   fluidRow(
#                     column(4,
#                            textOutput("updatetime"),
#                     ),
#                     column(4,
#                            actionButton("knit", "Update reports"),
#                            actionButton("git", "Version update")
#                     )
#                     
#                     , style="position:static; width:inherit;background-color:lightblue"
#                   ),
#                   DT::dataTableOutput("parameters"),
#                   tableOutput("par")
#                 )
#               }),
#               tabPanel("Summary", {
#                 sidebarLayout(
#                   sidebarPanel(
#                        #uiOutput("reports")
#                   ), 
#                   mainPanel(
#                     DT::dataTableOutput("par_to_choose")
#                   )
#                 )
#               })
#   )
# )

# shinyUI(fluidPage(
#     #tags$style(height="100vh", width="100vw"),
# 
#     # Application title
#     h3("Report updater interface"),
#     
#     # Settings
#     #position= "fixed-top",
# 
# div(
#     # Menu
#     div(fluidRow(
#             column(4,
#               textOutput("updatetime"),
#             ),
#             column(4,
#               actionButton("knit", "Update"),
#               actionButton("git", "Version update")
#             )
#         
#         , style="position:static; width:inherit;background-color:lightblue")
#     ),
#     DT::dataTableOutput("parameters"),
#     tableOutput("par")
# , style="height:90vh")
# ))
