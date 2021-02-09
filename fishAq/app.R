# this is my app
# NOTE: Jess has not sent us our data yet (should be ready next week), so we will use storms for this assignment

library(tidyverse)
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

  navbarPage("Bioeconomic Model: Fish & Protected Space",
             tabPanel("Fish Biomass",
                      sidebarLayout(
                        sidebarPanel("Select Date Range",
                                     sliderInput(inputId = "year_range",
                                                 label = "Year",
                                                 value = c(min(storms$year, na.rm = TRUE),
                                                           max(storms$year, na.rm = TRUE)),
                                                 min = min(storms$year, na.rm = TRUE),
                                                 max = max(storms$year, na.rm = TRUE),
                                                 step = 1L,
                                                 sep = ""
                                     )),
                        mainPanel("Output",
                                  plotOutput("biomass_plot"))
                      )
                      ),
             tabPanel("Biomass per Patch",
                      sidebarLayout(
                        sidebarPanel("Select Patch",
                                     checkboxGroupInput("check_patch",
                                                        label = "Hurricane Type [dummy dataset]",
                                                        choices = list(
                                                          "Patch 1" = "hurricane",
                                                          "Patch 2" = "tropical storm",
                                                          "Patch 3" = "tropical depression"
                                                        ))),
                        mainPanel("Output",
                                     plotOutput("patches")
                                     ))),
             tabPanel("Farm Size"),
             tabPanel("Zones of Influence")

             )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  storms_reactive <- reactive({

    storms %>%
      filter(year %in% input$year_range)

  })

  output$biomass_plot <- renderPlot(
    ggplot(data = storms_reactive(), aes(x = year, y = category)) +
      geom_jitter(aes(color = category))
  )

  patch_reactive <- reactive({

    storms %>%
      filter(status %in% input$check_patch)
  })

  output$patches <- renderPlot(
    ggplot(data = patch_reactive(), aes(x = year, y = category)) +
      geom_jitter(aes(color = status))
  )
}

# Run the application
shinyApp(ui = ui, server = server)
