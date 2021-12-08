## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(disable = TRUE),

          dashboardBody(includeCSS("css/movies.css"),
              fluidRow(
                  box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to see most popular movies in your favorite genre", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               )
          )
    )
) 