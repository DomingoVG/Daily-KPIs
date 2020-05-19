## ---------------------------
##
## Script name: ui.R
##
## Purpose of script:  Specifies user interface for Pax-Crossings Dashboard
##
## Author: Domingo Velazquez
##
## Date Created: 2020-04-24
##
## ---------------------------
##
## load up the packages we will need 

library(shiny)
library(tidyselect)
library(plotly)
library(shinyWidgets)
library(repmis)
library(dplyr)
library(shinydashboard)
library(dashboardthemes)
library(scales)

## source files

source("getData.R")

# Load configurations and variables to use

options(scipen = 8)
months <- 1:12
names(months) <- month.abb

# Define UI

shinyUI(dashboardPage(#skin = "purple",
  dashboardHeader(title = "Daily KPIs"),
  
  dashboardSidebar(tags$head(tags$style(HTML("hr {border-top: 1px solid #666666;}"))),
                   hr(),
                   div(img(height = 80,
                           width = 160,
                           src="CBX.png"),
                           style="text-align: center;"),
                   hr(),
                   radioButtons(inputId = "DirectionID",
                                label = "Direction",
                                choices = list("Total" = 1, 
                                               "Northbound" = 2,
                                               "Southbound" = 3),
                                selected = 1),
                   selectizeInput(inputId = "PeriodID",
                                  label = "Year:",
                                  choices = list("2019", 
                                                 "2020"),
                                  selected = "2020"),
                   selectizeInput(inputId = "RecurrencyID",
                                  label = "Recurrence:",
                                  choices = list("Yearly" = 1, 
                                                 "Monthly" = 2), 
                                  selected = 2),
                   conditionalPanel(
                     condition = "input.RecurrencyID == 2",
                     selectizeInput(inputId = "MonthsID",
                                    label = "Month:",
                                    choices = list("January" = 1, 
                                                   "February" = 2,
                                                   "March" = 3,
                                                   "April" = 4, 
                                                   "May" = 5,
                                                   "June" = 6,
                                                   "July" = 7, 
                                                   "August" = 8,
                                                   "September" = 9,
                                                   "October" = 10, 
                                                   "November" = 11,
                                                   "December" = 12),
                                    selected = match(months(Sys.Date()-2), month.name))),
                     checkboxGroupInput(inputId = "ScenarioID",
                                        label = "Scenario:",
                                        choices = list("Budget" = 2,
                                                       "YoY" = 3),
                                        selected = 2)),
  dashboardBody(
    tags$head(tags$style(HTML("/* logo */
                              .skin-blue .main-header .logo {
                              background-color: #3F2A55;
                              }
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #FF3094;
                              }
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #3F2A55;
                              }        
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #666666;
                              }
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #FF3094;
                              }
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              }
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #FF3094;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #FF3094;
                              }"
                              ))),
    tags$head(tags$style(HTML(".small-box {
                              height: 75px;
                              }
                              .fa {
                              font-size: 45px;
                              }
                              .small-box-content {
                              padding-top: 0px; padding-bottom: 0px;
                              }"))),
    tags$style(".small-box.bg-purple {
               background-color: #3F2A55 !important; color: #FFFFFF !important; 
               }"),
    tags$style(".small-box.bg-red {
               background-color: #DB2801 !important; color: #FFFFFF !important; 
               }"),
    tags$style(".small-box.bg-fuchsia {
               background-color: #E05C96 !important; color: #FFFFFF !important; 
               }"),
    tags$style(".small-box.bg-blue {
               background-color: #1D3AA2 !important; color: #FFFFFF !important; 
               }"),
    tags$style(".small-box.bg-yellow {
               background-color: #F9AA23 !important; color: #FFFFFF !important; 
               }"),
    tags$style(".fa-user {
               color:#ab4b7b
               }"),
    tags$style(".fa-users {
               color:#ab4b7b
               }"),
    fluidRow(valueBoxOutput("actualBox",
                            width = 4),
             valueBoxOutput("budgetBox",
                            width = 4),
             valueBoxOutput("yoyBox",
                            width = 4)),
    fluidRow(valueBoxOutput("actualBox2", 
                            width = 3),
             valueBoxOutput("budgetBox2",
                            width = 3),
             valueBoxOutput("yoyBox2", 
                            width = 3),
             valueBoxOutput("deltaBox",
                            width = 3)),
    tags$style(HTML(".box.box-solid.box-primary>.box-header {
                    color:#fff;background:#726C79
                    }
                    .box.box-solid.box-primary {
                    border-bottom-color:#726C79;
                    border-left-color:#726C79;
                    border-right-color:#726C79;
                    border-top-color:#726C79;
                    }")),
    fluidRow(box(title = "Daily (Actual vs Scenario)",
                 solidHeader = TRUE,
                 status="primary", 
                 plotlyOutput("rawPlot")),
             box(title = textOutput("box"),
                 solidHeader = TRUE,
                 status="primary",
                 plotlyOutput("cumPlot"))
    ),
    conditionalPanel(
    condition = "input.ScenarioID != 0 ",
    fluidRow(box(title = "Daily % change vs Projected",
                 solidHeader = TRUE,
                 status="primary", 
                 plotlyOutput("difPlot")),
             box(title = "Acumulative % change vs Projected",
                 solidHeader = TRUE,
                 status="primary", 
                 plotlyOutput("adifPlot")))
    ),
    fluidRow(box(title = "Acummulated Passengers by Day of Week",
                 solidHeader = TRUE,
                 status="primary",
                 plotlyOutput("wdPlot"))
     )  
   )
 )
)
