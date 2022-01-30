#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DT)
library(dashboardthemes)
library(shiny)
library(shinydashboard)
library(shiny.fluent)

#### Load Order data ####
# Load dataset order_df
load("orderData.RData")

#### UI ####
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Order Appsi"),
    #### SideBar ####
    dashboardSidebar(
        hr(),
        Persona(
            imageInitials = "RP",
            text = "Raphael Prates",
            secondaryText = "R Developer Candidate",
            presence = 2
        ),
        hr(),
        sidebarMenu(id="tabs",
                    menuItem("Total Order", tabName = "orderTab", icon=icon("chart-bar")),
                    menuItem("Unit tests", tabName = "unitTab", icon=icon("atom"), selected = TRUE),
                    menuItem("My Projects", tabName = "projectsTab", icon=icon("r-project"))
        )
    ),
    #### Body ####
    dashboardBody(shinyDashboardThemes(theme = "grey_light"),
                  tabItems(
                      tabItem(
                          title = "Total Order",
                          tabName = "orderTab",
                          fluidRow(
                              column(1, selectInput(
                                  "currencyInput", "Currency:", c("EUR", "USD")
                              )),
                              column(
                                  width = 5,
                                  offset = 2,
                                  valueBoxOutput("totalPriceOutput")
                              )
                          ),
                          fluidRow(
                              box(
                                  title = "Order",
                                  solidHeader = T,
                                  width = 4,
                                  collapsible = T,
                                  DT::DTOutput("orderOutput")
                              ),
                              box(
                                  solidHeader = T,
                                  width = 4,
                                  collapsible = T,
                                  plotlyOutput("plotOutput")
                              )
                          )
                      ),
                      tabItem(tabName = "unitTab",
                              box(width = NULL, htmlOutput("unitTest")))
                  ))
)

### Server ###
# Define server logic required to draw a histogram
server <- function(input, output) {

    # Reactive value to choose the icon for the currency
    moneyIconReac <- reactive({
        return(ifelse(input$currencyInput == "EUR", "euro-sign", "dollar-sign"))
    })

    plotReac <- reactive({
        currencySelected <- ifelse(input$currencyInput == "EUR","price_EUR", "price_USD")
        plot <- plot_ly(order_df,
                       x = ~product,
                       y = ~order_df[[currencySelected]],
                       type = "bar",
                       color = ~product)

        plot <- plot %>%
            layout(title = "Total order",
                   xaxis = list(title = "Product"),
                   yaxis = list(title = paste0("Order Price in ", input$currencyInput)))
        return(plot)
    })




    output$orderOutput <- DT::renderDataTable(order_df, rownames=F, options = list(pageLength = 5))

    getUnitPage<-function() {
        return(includeHTML("r_candidate_raphael_prates.html"))
    }

    #### Total Order Output ####
    output$totalPriceOutput <- renderValueBox({
        valueBox("1500",
                 "Total Price",
                 icon = icon(moneyIconReac()),
                 color = "green")
    })

    #### Plot output ###
    output$plotOutput <- renderPlotly({plotReac()})

    output$unitTest <- renderUI({getUnitPage()})
}

# Run the application
shinyApp(ui = ui, server = server)
