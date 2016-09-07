library(shiny)
library('plotly')

shinyUI(pageWithSidebar(

    headerPanel("Données paracliniques"),

    sidebarPanel(
        dateRangeInput('dateRange',
                        label = 'période',
                        start = '2012-02-09',
                        end = Sys.Date() + 2),
        width = 3
    ),

    mainPanel(
        tabsetPanel(
            tabPanel(
                "Summary CES 1",
                fluidPage(
                    hr(),
                    fluidRow(
                        column(6,
                               selectInput("CES", "CES", choices = c(levels(para_num$CES), "All"), selected = "All")
                        ),
                        column(6,
                               selectInput("panel1var1", "catégorie", choices = c(levels(dic_nom_para$cat)), selected =c(levels(dic_nom_para$cat))[3] )
                        )
                    ),
                    hr(),
                    DT::dataTableOutput('datatable1')
                )
            ),


            tabPanel(
                "Summary CES 2",
                fluidPage(
                    hr(),
                    fluidRow(
                        column(4,
                            selectInput("panel2var1", "CES/Antenne", choices = c("CES"="par_ces","Antenne"="SOC_CES_Antenne"), selected = c("par_ces"))
                        ),
                        column(4,
                               selectInput("panel2var2", "catégorie", choices = c(levels(dic_nom_para$cat)), selected =c(levels(dic_nom_para$cat))[3] )
                        ),
                        column(4,
                            uiOutput("panel2var3")
                        )
                    ),
                    hr(),
                    #verbatimTextOutput("panel2_text"),
                    DT::dataTableOutput('panel2_summary'),
                    DT::dataTableOutput('datatable2')
                )
            ),


            tabPanel("Panel3",
                     fluidPage(sidebarLayout(
                       sidebarPanel(
                         uiOutput("variable3"),
                         uiOutput("variable4"),
                         width = 2
                       ),
                       mainPanel(DT::dataTableOutput('datatable3'))
                     ))
            ),


            tabPanel(
                "Panel4",
                fluidPage(
                    hr(),
                    fluidRow(
                        column(6,
                               selectInput("panel4var1", "CES", choices = c(levels(para_num$CES), "All"), selected = "All"),
                               selectInput("panel4var2", "segmentation", choices = c("clas_age3","clas_age5","clas_age45an"))
                        ),
                        column(6,
                               selectInput("panel4var3", "catégorie", choices = c(levels(dic_nom_para$cat)), selected =c(levels(dic_nom_para$cat))[3]),
                               uiOutput("panel4var4")
                        )
                    ),
                    hr(),
                    DT::dataTableOutput('datatable4')
                )
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





