library(dplyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

shinyUI(dashboardPage(
    dashboardHeader(title = "Get bootstraps from a database"),
    dashboardSidebar(sidebarMenu(
        id = "menu",
        menuItem(tabName = "home", text = "Home", icon = icon("home")),
        menuItem(tabName = "results", text = "Results")
    )),
    dashboardBody(
        useShinyjs(),
        #extendShinyjs(),
        tags$head(
            includeCSS("www/styles.css")
        ),
        tabItems(
            tabItem("home",
                    fluidRow(
                        box(title = "Data file loading",
                            fileInput("loadedTable", NULL),
                               helpText("Your column names must include 'cost', 'effect' and 'strategy' (lower case)")),
                        uiOutput("config")
                    ),
            fluidRow(
                uiOutput("showTable")
            )
            ),
            tabItem("results",
                uiOutput("allResults")
            )
        )
    )
))
