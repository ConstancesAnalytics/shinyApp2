library(shiny)
library('plotly')



shinyUI(pageWithSidebar(
  # Application title
  headerPanel("para"),

  sidebarPanel( dateRangeInput('dateRange',
                               label = 'Date range input: yyyy-mm-dd',
                               start = '2012-02-09', end = Sys.Date() + 2
  ),
  selectInput("VAR", "VAR", choices = c(levels(dic_nom_para$cat)), selected =c(levels(dic_nom_para$cat))[3] ),

  width = 2),
  mainPanel(
    tabsetPanel(

    tabPanel("Panel1",
             fluidPage(theme = "www/bootstrap.css",
                       tags$head(
                         tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
                       ),
              sidebarLayout(
               sidebarPanel(
                 selectInput("CES", "CES", choices = c(levels(para_num$CES), "All"), selected = "All"),


                 width = 2
               ),
               mainPanel(
                 DT::dataTableOutput('datatable1')
                         )
             ))
    ),

    tabPanel("Panel3",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 selectInput("variable02", "variable02", choices = c("CES"="par_ces","Antenne"="SOC_CES_Antenne"), selected = c("par_ces")),
                 uiOutput("variable2"),
                 width = 2
               ),
               mainPanel(verbatimTextOutput("text1"),
                         DT::dataTableOutput('summary'),
                 DT::dataTableOutput('datatable3'))
             ))
    ),
    tabPanel("Panel4",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 uiOutput("variable3"),
                 uiOutput("variable4"),
                 width = 2
               ),
               mainPanel(DT::dataTableOutput('datatable4'))
             ))
    ),
    tabPanel("TDB",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 uiOutput("variable5a"),
                 selectInput("variable5b", "variable5b", choices = c("clas_age3","clas_age5","clas_age45an")),
                 width = 2
               ),
               mainPanel(DT::dataTableOutput('datatable5'))
             ))
    ),
    tabPanel("TDB GRAPHIQUE",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 uiOutput("variable6a"),
                 width = 2
               ),
               mainPanel(
                 plotOutput("plot1")
                 )
             ))
    ),

    tabPanel("Panel7",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 uiOutput("variable7a"),
                 uiOutput("variable7b"),
                 uiOutput("variable7c"),
                 width = 2
               ),
               mainPanel(
                 plotOutput("plot2")
               )
             ))
    ),
    tabPanel("Panel8",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 selectInput("variable10", "variable10", choices = c("par_ces","SOC_CES_Antenne"), selected = c("par_ces")),
                 uiOutput("variable8"),
                 width = 2
               ),
               mainPanel(
                 plotlyOutput("plot3")
               )
             ))
    ),
    tabPanel("Panel9",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 uiOutput("variable9"),

                width = 2
               ),
               mainPanel(DT::dataTableOutput('mytable1'))

             ))
    ),
    tabPanel("Panel10",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 uiOutput("variable10a"),
                 uiOutput("variable10b"),
                             width = 2
                 ),
                 mainPanel(
                   plotOutput("plot4"))

               )))


))))





