library(shiny)
library(shinydashboard)

webPageTitle = "TBS-ATI NLP Exploration"

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol {
                                   height: auto;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ), 
  tags$head(tags$script(HTML("$(document).on('click', '.needed', function () {
                                Shiny.onInputChange('last_btn',this.id);
                             });")))
  )

UIFacColWidth = 4
UINumColWidth = 4
UIWcColWidth = 4

header = dashboardHeader(
  title = span(
     HTML(paste(webPageTitle))) # <sub>v0.1</sub>
  , titleWidth = 350
  , dropdownMenu(type = "messages", icon = icon("info-circle")
                 ,messageItem(from = "Most Important Terms", message = tags$div("TF-IDF"), icon = icon(NULL)   )
                 ,messageItem(from = "Topics", message = tags$div("Topic Modelling"), icon = icon(NULL)   )
  ) # END dropdown
  
) # END Header 


sidebar = dashboardSidebar(
  tags$head(tags$style(HTML('
                            /* menu */
                            .skin-purple .sidebar-menu > li.active > a {
                            background-color: #545096;
                            }
                            #                           '))), # https://www.quackit.com/css/css_color_codes.cfm
  
  # width = 450,
  
  sidebarMenu(id= "menu"
              # , selectInput("dataSource", "Choose data source:", choices = sub(".csv", "", list.files(pattern = "csv")))

              , fluidRow(
                column(6, selectInput("filterDataSample", "Sample", c("Dept", "All"), "Dept"))
                ,column(6, h3("N = ", textOutput("dataNtotal", inline= T)))
              )
              
              , fluidRow(column(12, selectInput("dept", "Department", ownersTopN)))
              , fluidRow(column(8, numericInput("bigramN", "Top N for Bigrams", 20, 5, 50, 5)))
              
              , h3("Stop Word Management")
              , fluidRow(column(12, checkboxInput("removeDeptName", "Remove department name?", FALSE)))
              , fluidRow(column(12,div(style = "margin-top:-2em", checkboxInput("ignoreAllStops", "Ignore all custom stop words?", FALSE))))
              , fluidRow(column(12, textInput("stopWordInput", "Add custom stop words (CSV):")))
              , fluidRow(column(12, actionButton("goStop", "Enhance!", icon = icon("laptop"))))
              
              , verbatimTextOutput("testText1") # Testing - remove
), width = 300) # END Sidebar 

body = dashboardBody(
  tags$head(
    tags$style(
      HTML(".shiny-notification {position: fixed; top: calc(80%); left: calc(50%); font-size: 32px;}")
    )), 
  tabsetPanel(
    tabPanel("Overview"
             , tabsetPanel(
               tabPanel("Most Important Terms",
                        fluidPage(
                          fluidRow(
                            column(1, numericInput("nGramN", "N-grams", 1, 1, 4, 1))
                            ,column(1, numericInput("tfTopN", "Top N", 10, 6, 20, 2))
                            ,column(2, br(), downloadButton("downPlotTFIDF", "Download Plots", 
                                                       style="color: #fff; background-color: #4C4CB2; border-color: #2e6da4"))
                          ) # END row
                          
                          ,plotOutput("plotTFIDF", height = 800)
                          
                        ) # END FluidPage
                        
               ) # END Tab TF-IDF
               
               , tabPanel("Topics",
                          fluidPage(
                            fluidRow(
                              column(1, numericInput("topicN", "Topics", 9, 4, 16, 1))
                              # ,column(1, numericInput("topicTopN", "Top N", 10, 6, 20, 2))
                              ,column(2, br(), downloadButton("downPlotTopics", "Download Plots", 
                                                              style="color: #fff; background-color: #4C4CB2; border-color: #2e6da4"))
                            ) # END row
                            
                            ,plotOutput("plotTopics", height = 800)
                            
                          ) # END FluidPage
                          
               ) # END Tab Topic
             ) # END tabsetPanel
             ) # END tabPanel Overview
             
             
             , tabPanel("Deep Dive"
                        , tabsetPanel(id = "tabs",
                          tabPanel("Univariate"
                                   , fluidPage(
                                     fluidRow(
                                       column(UIFacColWidth, uiOutput("selectFac") )
                                       ,column(UINumColWidth, uiOutput("selectNum") )
                                     ) # END row
                                     
                                     ,fluidRow(
                                       column(UIFacColWidth, 
                                              fluidRow(
                                                column(3, numericInput("maxLabelUni", "Max Length", 20, 6, 40, 2))
                                                ,column(3, numericInput("maxBarsUni", "Max Bars", 8, 2, 40, 2))
                                                
                                                ,column(3, div(style = "margin-top:-0em", checkboxInput("iParetoUni", "Pareto order",  value= TRUE))
                                                        , div(style = "margin-top:-1em", checkboxInput("iLogBarUni", "log transform", value= FALSE)))
                                                
                                                ,column(3, div(style = "margin-top:-0em", checkboxInput("iRotateXUni", "Rotate labels", value= FALSE))
                                                        , div(style = "margin-top:-1em", checkboxInput("iLabelBarUni", "Add labels", value= FALSE)))
                                              ) # END row
                                       ) # END col
                                       
                                       ,column(UINumColWidth, 
                                               fluidRow(
                                                 column(6, numericInput("numBinsHistUni", "Hist Bins", 30, 6, 50, 4))
                                                 ,column(6, checkboxInput("iLogHistUni", "log transform", value= FALSE))
                                               )
                                       ) # END col
                                       
                                     ) # END row above graphs
                                     
                                     ,fluidRow(
                                       column(UIFacColWidth, plotOutput("plotBar", height = 600))
                                       ,column(UINumColWidth, plotOutput("plotHist", height = 600))
                                       ,column(UIWcColWidth, plotOutput("plotWC", height = 600))
                                     ) # END row
                                     
                                     ,fluidRow(
                                       column(UIFacColWidth, h3("Percent Complete", gaugeOutput("gaugeFac")))
                                       ,column(UINumColWidth, h3("Percent Complete", gaugeOutput("gaugeNum")))
                                     ) # END row
                                     
                                   ) # END FluidPage
                          ) # END Tab Uni
                          
                          , tabPanel("Bivariate",
                                     fluidPage(
                                       fluidRow(
                                         column(10 
                                                ,fluidRow(
                                                  column(3, uiOutput("selectMultivarX"))
                                                  ,column(1, br(), actionButton("switchVarsBi", "", icon = icon("exchange")))
                                                  ,column(3, uiOutput("selectMultivarY"))
                                                  ,column(3, uiOutput("selectMultivarG"))
                                                )
                                                
                                                ,fluidRow(
                                                  column(3
                                                         , h5("Avg:  ",   textOutput("summStatsAvgX",  inline = T))
                                                         , h5("SD:   ",   textOutput("summStatsSdX",   inline = T))
                                                  )
                                                  ,column(1, h5(""))
                                                  ,column(3
                                                          , h5("Avg:  ",   textOutput("summStatsAvgY",  inline = T))
                                                          , h5("SD:   ",   textOutput("summStatsSdY",   inline = T))
                                                  )
                                                  ,column(3
                                                          , radioButtons("catXcatType", "Plot Type", c("Bar", "Circle", "Tile"), inline = T)
                                                          , numericInput("maxLabelMulti", "Max Label Length", 20, 6, 40, 2)
                                                  )
                                                ) # END row
                                         ) # END col
                                         ,column(2, h3("Complete Data", gaugeOutput("gaugeBiComp")))
                                       ) # END row
                                       
                                       ,fluidRow(
                                         column(1, numericInput("maxLevelsMulti", "Max Levels", 8, 2, 40, 2))
                                         ,column(1, numericInput("maxGroupsMulti", "Max Groups", 4, 1, 10, 1))
                                         ,column(1, div(style = "margin-top:-0em", checkboxInput("iKeepFac", "Keep Levels",  value= TRUE))
                                                 , div(style = "margin-top:-1em", checkboxInput("iLogMulti", "log transform", value= FALSE)))
                                         ,column(1, div(style = "margin-top:-0em", checkboxInput("iFreqPoly", "Polygon", value= FALSE))
                                                 , div(style = "margin-top:-1em", checkboxInput("iRotateXMulti", "Rotate labels", value= FALSE)))
                                         ,column(1, numericInput("numBinsMulti", "Hist Bins", 30, 6, 50, 4))
                                         ,column(1, numericInput("cutoffMulti", "Numeric Cutoff", 100000, 0, 1000000, 100))
                                         ,column(1, div(style = "margin-top:-0em", checkboxInput("iLinearMulti", "Linear fit",  value= FALSE))
                                                 , div(style = "margin-top:-1em", checkboxInput("iXYMulti", "Y = X", value= FALSE)))
                                         ,column(1, checkboxInput("iJitterMulti", "Jitter", value= FALSE))
                                       )
                                       
                                       ,fluidRow(plotOutput("plotMulti", height = 600))
                                       
                                     ) # END FluidPage
                                     
                          ) # END Tab Multi
                          
                          , tabPanel("Bigrams - N",
                                     fluidPage(
                                       column(4, DT::dataTableOutput("tableBigramN"))
                                       ,column(8, plotOutput("plotBigramN", height = 800))
                                       
                                     ) # END FluidPage
                                     
                          ) # END Tab Bigrams
                          
                          , tabPanel("Bigrams - Corr",
                                     fluidPage(
                                       column(4
                                              , numericInput("bigramCorrMin", "Min Corr", 0.3, -0.5, 1, 0.05)
                                              , DT::dataTableOutput("tableBigramCorr"))
                                       ,column(8, plotOutput("plotBigramCorr", height = 800))
                                       
                                     ) # END FluidPage
                          ) # END Tab Bigrams
                        ) # END tabsetPanel 
             ) # END tabPanel Deep Dive
    , selected = "Overview"
  ) # END tabsetPanel 
) # END Body

dashboardPage(header, sidebar, body, skin = "purple", title = HTML(webPageTitle))
