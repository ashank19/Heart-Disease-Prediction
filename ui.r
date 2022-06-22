#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title ="Heart Disease clustering"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Raw Data",tabName = "raw",icon=icon("table")),
        menuItem("Determine clusters",tabName = "clusters",icon=icon("chart-area",lib="font-awesome")),
        menuItem("K-means clustering",icon=icon("users",lib="font-awesome"),startExpanded = TRUE,
         menuSubItem("Clustering",tabName= "kmeans" ),
         menuSubItem("Cluster Plot", tabName= "k-means")),
        menuItem("Hierarchical Clustering",tabName= "Hierarchical-clustering", icon=icon("users",lib="font-awesome")),
        menuItem("Clusterd Data",tabName = "finaldata",icon=icon("table")),
        selectInput('xcol', 'X Variable', var),
        selectInput('ycol', 'Y Variable', var, selected = var[[1]]),
        numericInput('clusters', 'Cluster count', 2, min = 1, max = 9)
      ),
      textOutput("tabres")
      ),
    dashboardBody(
      tabItems(
      tabItem(tabName = "raw",h1("Raw Data"),fluidRow(column(5,tableOutput("rawdata")))),  
      tabItem(tabName = "k-means",h1("Cluster Plot"),
              fluidRow(
                box(plotOutput("plot1")
              ))),
      tabItem(tabName = "clusters",h1("Number of Clusters"),
              fluidRow(
                box(h2("Elbow Method"),plotOutput("method1")),
                box(h2("Average silhouette method (K-Means)"),plotOutput("method2"))),
              fluidRow(  
                box(h2("Average silhouette method (Hierarchical)"),plotOutput("method3"))
              )),
      tabItem(tabName = "kmeans",h1("K-means clustering"),
              fluidRow(
                box(plotOutput("clusterchart")),
                box(sliderInput("clustnum","Number of clusters",1,10,2))
              )),
      tabItem(tabName = "Hierarchical-clustering",h1("Hierarchical clustering"),
              fluidRow(
                box(h2("Eucledian Method"),plotOutput("method5")),
                box(h2("Eucledian Method"),plotOutput("method7"))),
              fluidRow( 
                box(h2("Dendogram of Diana"), plotOutput('method8'))
              )),
      tabItem(tabName = "finaldata",h1("Clustered Data"),fluidRow(column(5,tableOutput("clustdata"))))
    )
  ))   
)