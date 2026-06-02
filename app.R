#library nécessaire
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

# Chargement du donnée
dt <- read.csv2("data_debt.csv")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Country debt"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global view", tabName = "globaly_view"),
      menuItem("By-year view", tabName = "year_view")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "globaly_view",
        pickerInput(
          inputId = "country_global_view",
          label = "Country search :", 
          choices = c(unique(dt$Country)),
          options = pickerOptions(container = "body", liveSearch = TRUE),
          width = "100%"
        ),
        fluidRow(
          box(
            title = textOutput("ratio_by_growth"),
            status = "info",
            width = 12,
            solidHeader = TRUE,
            plotOutput("plot_3")
          )
        ),
        infoBox(
          title = "Info", 
          width = 12, 
          value = "Certain theories state that once the debt ratio passes a specific threshold, economic growth begins to decline.")
      ),
      
      tabItem(
        tabName = "year_view",
        wellPanel(
          pickerInput(
            inputId = "country_year_view",
            label = "Country search :", 
            choices = c(unique(dt$Country)),
            options = pickerOptions(container = "body", liveSearch = TRUE),
            width = "100%"
          ),
          airYearpickerInput(
            inputId = "range_year",
            label = "Range year :", 
            value = c(unique(dt$Year_input)),
            width = "100%",
            range = TRUE,
            minDate = min(dt$Year_input),
            maxDate = max(dt$Year_input)
          ),
          actionBttn(
            inputId = "voir",
            label = "View",
            style = "jelly", 
            color = "primary"
          )
        ),
       
        fluidRow(
          box(
            title = textOutput("growth"),
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_1")
          ),
          box(
            title = textOutput("ratio"),
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_2")
          )
        ),
        
        fluidRow(
          infoBox(
            title = "Growth", 
            width = 6, 
            value = "Annual GDP growth rate."
          ),
          infoBox(
            title = "Ratio", 
            width = 6, 
            value = "A 110% ratio means that the country owes more than it produces in a single year."
          )
        )
      )
    )
  )
)


# Define server 
server <- function(input, output) {
 
  output$growth <- renderText(
    {
      paste(input$country_year_view, "'s GDP growth over the years", sep = "")
    }
  )
  
  output$ratio <- renderText(
    {
      paste(input$country_year_view, "'s annual debt ratio.", sep = "")
    }
  )
  
  output$ratio_by_growth <- renderText(
    {
      paste("The relationship between debt ratio and growth in", input$country_global_view, ".",sep = " ")
    }
  )
  
  output$plot_1 <- renderPlot(
    {
      req(input$range_year)
      min_date <- year(input$range_year[1])
      max_date <- year(input$range_year[2])
      dt %>% 
        filter(Country == input$country_year_view) %>% 
        filter(Year >= min_date & Year <= max_date) %>% 
        ggplot() +
          aes(x = Year, y = growth) +
          geom_line()
    }
  ) %>% bindEvent(input$voir, ignoreInit = FALSE)
  
  output$plot_2 <- renderPlot(
    {
      req(input$range_year)
      min_date <- year(input$range_year[1])
      max_date <- year(input$range_year[2])
      dt %>% 
        filter(Country == input$country_year_view) %>% 
        filter(Year >= min_date & Year <= max_date) %>% 
        ggplot() +
          aes(x = Year, y = ratio) +
          geom_line()
    }
  )%>% bindEvent(input$voir, ignoreInit = FALSE)
  
  output$plot_3 <- renderPlot(
    {
      dt %>% 
        filter(Country == input$country_global_view) %>% 
        ggplot() +
          aes(x = ratio, y = growth) +
          geom_line()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
