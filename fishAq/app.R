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
             tabPanel("Farm Size",
                      sidebarLayout(
                        sidebarPanel("Select Farm size",
                                     checkboxGroupInput("check_farm_size",
                                                        label = "Hurricane Category [dummy dataset]",
                                                        choices = list(
                                                          "Small" = "1",
                                                          "Medium" = "3",
                                                          "Large" = "5"
                                                        ))),
                        mainPanel("Output",
                                  plotOutput("farm_size")
                      ))),

             tabPanel("Zones of Influence",
                      sidebarLayout(
                        sidebarPanel("Select fish size range",
                                     sliderInput("select_size",
                                                 inputId = "fish_size_range",
                                                 label = "Mass",
                                                 value = c(min(storms$hu_diameter, na.rm = TRUE),
                                                           max(storms$hu_diameter, na.rm = TRUE)),
                                                 min = min(storms$hu_diameter, na.rm = TRUE),
                                                 max = max(storms$hu_diameter, na.rm = TRUE),
                                                 step = 10,
                                                 ticks = TRUE
                                                  )),
                        mainPanel("Output",
                                  plotOutput("fish_size")
                                  )
                        )))
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

  farm_reactive <-reactive({

    storms %>%
      filter(category %in% input$check_farm_size)
  })

  output$farm_size <- renderPlot(
    ggplot(data = farm_reactive(), aes(x= category, y=ts_diameter))+
      geom_col(aes(color=status))
  )

  fish_reactive <- reactive({

    storms %>%
      filter(hu_diameter %in% input$fish_size_range)

  })

  output$fish_size <- renderPlot(
    ggplot(data = fish_reactive(), aes(x = category, y = hu_diameter)) +
      geom_jitter(aes(color = name))
  )
}

# Run the application
shinyApp(ui = ui, server = server)
