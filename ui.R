#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#   https://rstudio.github.io/shinydashboard/get_started.html
#

# install.packages("shinydashboard")
# install.packages("openxlsx")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggmap")
# install.packages("data.table")
# install.packages("plotly")
# install.packages("leaflet")
# install.packages("htmltools")
# install.packages("RColorBrewer")
# install.packages("threejs")
# install.packages("DT")
# install.packages("shiny")
# install.packages("tidyverse")
# install.packages('addinslist') 

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Chimbu Account Reporting"),
    dashboardSidebar(
      sidebarMenu(
        # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Company Report", tabName = "CompanyReportview", icon = icon("map-marker")),
        menuItem("Market Analysis", tabName = "MarktAnalysis", icon = icon("balance-scale")),
        menuItem("Circling Amount", tabName = "CirclingAmt", icon = icon("map-marker")),
        menuItem("Overall by Companies", tabName = "DataOverallCompanies", icon = icon("bars")),
        # menuItem("Price Analysis", tabName = "OppAnalysis", icon = icon("inr")),
        menuItem("Data View", tabName = "DataView", icon = icon("th")),
        menuItem("Price Analysis", tabName = "priceAnalysis", icon = icon("inr")),
        menuItem("TOP SELLER", tabName = "UnitAnalysis", icon = icon("bar-chart")),
        menuItem("Missing Data", tabName = "missingSummary", icon = icon("warning"))
       # menuItem("Closed opportunities", tabName = "widgets2", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        ########################################### Market Analysis ###############
        tabItem(tabName = "MarktAnalysis",
                fluidRow(
                 box(selectInput(inputId = "SelYear","Year",ValTransactionYear,multiple = FALSE)),
                
                 box(selectInput(inputId = "SelMonth","Month",ValTransactionMonth,multiple = FALSE))
                 # valueBoxOutput("ClosedSuccess"),

                # valueBoxOutput("ClosedSuccessPer")
                 ),
                 fluidRow(
              
                   shinydashboard::valueBoxOutput("HighIncome",width=6),
               
                   shinydashboard::valueBoxOutput("HighExp",width=6)

                # valueBoxOutput("ClosedSuccessPer")
                 ),
                fluidRow(
                  box(column(6,plotlyOutput("MonthlyTrack",width = 600))),
                  box(column(6,plotlyOutput("MonthlyDayTrack",width = 600)))
                ),
                fluidRow(
                  box(column(6,plotlyOutput("YearlyDayTrack",width = 600)))
                  )
        ),
       
        ########################################### Data View  ###############
        tabItem(tabName = "DataView",
                fluidRow(
                  box(title = "Overall",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      dataTableOutput("tabOverAllCompanies"))
                )
              ),
        # Second tab content
        
        ########################################### Data View Overall Companies  ###############
        # Third tab content starts
        tabItem(tabName = "DataOverallCompanies",
                fluidRow(
                  box(title = "Overall",width=12,status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                      dataTableOutput("tabOverAllMarket"))
        )
        ),# Third tab content ends
        
        
        ########################################### Companies Report View  ###############
        # Fourth tab content starts
        tabItem(tabName = "CompanyReportview",
                fluidRow(
                  box(selectInput(inputId = "ComReportSelYear","Year",ValTransactionYear,multiple = FALSE)),
                  
                  box(selectInput(inputId = "ComReportSelSegment","Segment of Expense",ValSegment,multiple = FALSE))
                ),
                fluidRow(
                  box(selectInput(inputId = "ComReportSelStdComp","Standard Expenses Company", StandardExpenses ,multiple = FALSE)),
                  
                  box(selectInput(inputId = "ComReportSelIrrComp","Irregular Expenses Company",IrregularExpenses,multiple = FALSE))
                ),
                fluidRow(
                  column(6,plotlyOutput("CompanyReport_Std_MonthlyTrack",width = 600)),
                  column(6,plotlyOutput("CompanyReport_Irr_MonthlyTrack",width = 600))
                  # valueBoxOutput("ClosedSuccessPer")
                ),
                fluidRow(
                  column(6,plotlyOutput("CompanyReport_Irr_DailyTrack",width = 600))
                )
        ),# Fourth tab content ends
        
        ########################################### Circling Amount View   ###############
        tabItem(tabName = "CirclingAmt",
                fluidRow(
                  column(6,box(flexdashboard::gaugeOutput("CyberportLaptop"),width=12,title="Cyberport Laptop"))
                )
        ),
        ########################################### Missing Summary View   ###############
        
        tabItem(tabName = "missingSummary",
                fluidRow(
                  box(title = "Missing Data Summary",width=12,status = "warning", solidHeader = TRUE,collapsible = FALSE,collapsed = FALSE,
                      dataTableOutput("tabmissingSummary"))
                ),
                fluidRow(
                  box(title = "Missing Data Details - Segment",width=12,status = "warning", solidHeader = TRUE,collapsible = FALSE,collapsed = FALSE,
                      dataTableOutput("tabmissingSummaryDetails"))
                )
               )
        
        )
      )#dashboard Body Close
  )#dashboard page close
)
