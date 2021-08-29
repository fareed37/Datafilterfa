#setwd("C:/Users/ranjan.o.kumar/OneDrive - Accenture/CBA_Optimisation_Tool_v1")

rm(list=ls())
library(markdown)
library(shinyjs)
library(dygraphs)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(D3TableFilter)
library(data.table)
library(plyr)
library(dplyr)
library(V8)
library(htmlwidgets)
library(reshape2)
library(stringr)
library(DT)
library(scales)
library(plotly)
library(shinyWidgets)
library(DT)
library(nloptr)
library(formattable)

source("./Input_Data.R")

navbarPage(
  title=div(
    tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"logo_cba.png\" alt=\"alt\" style=\"float:right;width:100px;height:60px;padding-top:10px;\"> </a>`</div>');
    console.log(header)")
    )
    , "Media Planning Tool")
  # "Select the Tab:"
           ,id = 'navid',
           tabPanel("Input Data",
                    dashboardPage(
                      dashboardHeader(title="Optimisation Input"),
                      dashboardSidebar(
                        uiOutput("dateRange"),
                        selectizeInput("heir1",
                                       shiny::HTML("<p> <span style='color: black'>Granularity</span></p>"),
                                       choices =Granularity_names, selected = NULL, multiple = T),
                        selectizeInput("heir2", 
                                       shiny::HTML("<p> <span style='color: black'>Date Level</span></p>"),
                                       choices = c("Weekly", "Monthly", "Yearly","Total"), selected = NULL, multiple = F),
                        uiOutput("granulairty_filter1"),
                        uiOutput("granulairty_filter2"),
                        p(actionButton("submitplan","Submit"),actionButton("export_button", "Export/Scenario Planning"))
                      ),
                      
                      
                      
                      dashboardBody(
                        fluidPage( 
                          fluidRow(
                                  column(width=2, valueBoxOutput("Spend_Input_nav", width = "25%")),
                                  column(width=2, valueBoxOutput("Contribution_Input_nav", width = "25%")),
                                  column(width=2, valueBoxOutput("Revenue_Input_nav", width = "25%")),
                                  column(width=3, valueBoxOutput("CPA_Input_nav", width = "25%")),
                                  column(width=3, valueBoxOutput("ROMI_Input_nav", width = "25%")),
                                  #box(title="Contribution means Application Count here for now",status="primary",width = 12,
                                  d3tfOutput('Summary_Table', height = "auto")
                                  #)
                                  # ,
                                  # column(width = 2,
                                  #        h4("Last edits"),
                                  #        tableOutput("edits"))
                                  )
                          ),
                        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #FFCC00;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #FFCC00;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #FFCC00;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #FFCC00;
                                }

                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #FFCC00;
                                }

                                /* body */
                                
                                .content-wrapper, .right-side {
                                background-color: #EBEBEB;
                                }
                                
                                .box.box-solid.box-primary>.box-header {
                                color:#fff;
                                background:#666666
                                
                    }
                                
                        
                           '))))
                      
                    )
           ),
           tabPanel("Scenario Planning",value="Optimisation_Tab",
                    dashboardPage(
                      dashboardHeader(title="Optimisation Dashboard"),
                      dashboardSidebar(
                        sidebarMenu(id = "tabs",
                                    menuItem(text = "Input Assessment", tabName = "data", icon = icon("file-upload")),
                                    uiOutput('ui_sbo'),
                                    uiOutput('ui_gbo')
                                    # menuItem(text="Optimisation",tabName = 'Opti', icon = icon("bolt"))
                        )
                      ),
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "data",
                                  fluidRow(
                                            column(width=2, fileInput(inputId = "base_scenario", label = "Custom Plan:", accept = ".csv")),
                                            column(width=2, valueBoxOutput("Spend", width = "25%")),
                                            column(width=2, valueBoxOutput("Contribution", width = "25%")),
                                            column(width=2, valueBoxOutput("Revenue", width = "25%")),
                                            column(width=2, valueBoxOutput("CPA_Input_Assess", width = "25%")),
                                            column(width=2, valueBoxOutput("ROMI_Input_Assess", width = "25%"))
                                            
                                        ),
                                  fluidRow(
                                    column(width=2,
                                           autonumericInput(
                                             inputId="simulated_spend",
                                             label="Budget:",
                                             value=150000000,
                                             modifyValueOnWheel=T,
                                             wheelOn ="hover",
                                             wheelStep=1000000,
                                             currencySymbol = "$",
                                             currencySymbolPlacement = "p",
                                             decimalPlaces = 2,
                                             digitGroupSeparator = ",",
                                             divisorWhenUnfocused = 1000000,
                                             symbolWhenUnfocused = "M"
                                          )
                                        ),
                                  column(width=2,
                                         autonumericInput(
                                           inputId="Target_Revenue",
                                           label="Target:",
                                           value=0,
                                           modifyValueOnWheel=T,
                                           wheelOn ="hover",
                                           wheelStep=100000,
                                           # currencySymbol = "$",
                                           # currencySymbolPlacement = "p",
                                           decimalPlaces = 2,
                                           digitGroupSeparator = ",",
                                           divisorWhenUnfocused = 1000,
                                           symbolWhenUnfocused = "K"
                                         )
                                        ),
                                  column(width=2,
                                         numericInput(
                                           inputId="lower_bound",
                                           label="Lower Bound:",
                                           value=0.8,
                                           min = 0,
                                           max = 1,
                                           step = 0.05,
                                           width = NULL
                                         )
                                        ),
                                  column(width=2,
                                         numericInput(
                                           inputId="upper_bound",
                                           label="Upper Bound:",
                                           value=1.2,
                                           min = 1,
                                           max = NA,
                                           step = 0.05,
                                           width = NULL
                                         )
                                        ),
                                  column(width=4, align="center",
                                         actionButton("simulate_button",HTML('&emsp;Simulate') , icon("paper-plane"), width = '100%',
                                                      style="color: #fff; background-color: #808080; border-color: #525151"),
                                         actionButton("jumptoopt", HTML('&emsp;Spend Based Optimisation'), icon("fighter-jet"), width = '100%',
                                                      style="color: #fff; background-color: #808080; border-color: #525151"),
                                         actionButton("jumptogbo", HTML('&emsp;Goal Based Optimisation'), icon("fighter-jet"), width = '100%',
                                                      style="color: #fff; background-color: #808080; border-color: #525151")
                                      )
                                  )
                                  ,
                                  fluidRow(
                                    tabsetPanel(type='tabs',
                                                tabPanel("What-If Scenario",
                                                         d3tfOutput("contents",height="auto")),
                                                tabPanel("Response Curves",
                                                         fluidRow(column(4,  offset=4, uiOutput("RC_selector")),
                                                                  column(4,      uiOutput("RC_selector_subset"))),
                                                         fluidRow(plotOutput("RC_plot"), click = "plot_click" ))
                                                )
                                          )
                          ),
                          tabItem(tabName = "Opti_SBO",
                                  fluidRow(
                                        column(width=2, h3("What-If Scenario:")),
                                        column(width=2, valueBoxOutput("Simulated_Spend_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Simulated_Contribution_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Simulated_Revenue_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Simulated_CPA_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Simulated_ROMI_SBO", width = "25%"))
                                          ),
                                  fluidRow(
                                        column(width=2, h3("Optimised Scenario:")),
                                        column(width=2, valueBoxOutput("Uplift_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Optimised_Contribution_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Optimised_Revenue_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Optimised_CPA_SBO", width = "25%")),
                                        column(width=2, valueBoxOutput("Optimised_ROMI_SBO", width = "25%"))
                                          ),
                                  fluidRow(
                                    tabsetPanel(type='tabs',
                                                         tabPanel("Graphs",
                                                         fluidRow(column(4, align="center", offset=4, uiOutput("SBO_Budget_selector"))),
                                                         fluidRow(plotlyOutput("SBO_spend_level", width = "100%", height = "400px")),
                                                         fluidRow(plotlyOutput("SBO_contri_level", width = "100%", height = "400px")),
                                                         fluidRow(plotlyOutput("SBO_rev_level", width = "100%", height = "400px"))),
                                                         tabPanel("SBO Optimisation Results",
                                                         fluidRow(column(4, align="center", offset=4, downloadButton("Export_SBO", "Export SBO Results"))),
                                                         d3tfOutput("SBO_Opti_Granular",height="auto"))
                                               )
                                          )
                                                          # fluidRow(plotlyOutput("opt_rev_plot"))
                                ),
                          tabItem(tabName = "Opti_GBO",
                                 fluidRow(
                                       column(width=2, h3("What-If Scenario:")),
                                       column(width=2, valueBoxOutput("Simulated_Spend_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Simulated_Contribution_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Simulated_Revenue_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Simulated_CPA_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Simulated_ROMI_GBO", width = "25%"))
                                         ),
                                 fluidRow(
                                       column(width=2, h3("Optimised Scenario:")),
                                       column(width=2, valueBoxOutput("Optimised_Spend_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Optimised_Contribution_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Optimised_Revenue_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Optimised_CPA_GBO", width = "25%")),
                                       column(width=2, valueBoxOutput("Optimised_ROMI_GBO", width = "25%"))
                                         ),
                                 fluidRow(
                                       column(width=2, h3("Optimisation Results:")),
                                       column(width=2, valueBoxOutput("Uplift_GBO_Spend", width = "25%")),
                                       column(width=2, valueBoxOutput("Uplift_GBO_Contribution", width = "25%")),
                                       column(width=2, valueBoxOutput("Uplift_GBO_Revenue", width = "25%")),
                                       column(width=4, valueBoxOutput("Target_Contribution_GBO", width = "25%")),
                                       
                                          ),
                                 fluidRow(
                                   tabsetPanel(type='tabs',
                                               tabPanel("Charts",
                                                        fluidRow(column(4, align="center", offset=4, uiOutput("GBO_Budget_selector"))),
                                                        fluidRow(plotlyOutput("GBO_spend_level", width = "100%", height = "400px")),
                                                        fluidRow(plotlyOutput("GBO_contri_level", width = "100%", height = "400px")),
                                                        fluidRow(plotlyOutput("GBO_rev_level", width = "100%", height = "400px"))),
                                               tabPanel("GBO Optimisation Results",
                                                        fluidRow(column(4, align="center", offset=4, downloadButton("Export_GBO", "Export GBO Results"))),
                                                        d3tfOutput("GBO_Opti_Granular",height="auto"))
                                              )
                                         )
                                 )
                          
                            )
                          )
                        )
            )
           
)                 