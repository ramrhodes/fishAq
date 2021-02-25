# this is my app

library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)

app_data <- read_csv("appDatCEOA.csv") %>%
  clean_names() %>%
  mutate(frmsz_class = case_when(frm_sz == 2 ~ "small",
                              frm_sz == 5 ~ "medium",
                              frm_sz == 10 ~ "large")) %>%
  mutate(num_farms = ((mgmt_area*100)/frm_sz))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

  navbarPage("Bioeconomic Model: Fish & Protected Space",
             tabPanel("Overview",
                      mainPanel(
                        h3("Introduction"),
                        p("This Shiny application uses data from Jessica Couture's research to explore the impacts of Marine Protected Areas (MPA) and farms on wild fish populations and fishery catches. The application provides a platform for the user to observe how variations in size of MPAs and aquaculture farms relate to biomass of fish catches, fish attraction to the farm, and movement of adult fish."),
                        h3("Overview"),
                        p("The model is a 1-D spatially explicit bioeconomic model that simulates fish movement and fishery catches around a protected space. The model is used to simulate the impacts of an MPA or farm on wild populations (abundance, biomass) and fishery catches (yield amount and yield biomass)."),
                        img(src = "shiny_app_diagram.png"),
                        h3("Summary"),
                        h3("Citation"),
                        p("Model created by Jessica Couture, University of California, Santa Barbara")
                      )
                      ),

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
                                  ))),

            tabPanel("Fish Biomass Over Time",
                     sidebarLayout(
                       sidebarPanel("Select Date Range",
                                    sliderInput(inputId = "year_range",
                                                label = "Year",
                                                value = c(min(app_data$year, na.rm = TRUE),
                                                          max(app_data$year, na.rm = TRUE)),
                                                min = min(app_data$year, na.rm = TRUE),
                                                max = max(app_data$year, na.rm = TRUE),
                                                step = 1L,
                                                sep = ""
                                    )),
                       mainPanel("Output",
                                 plotOutput("biomass_plot")
                                 ))),

            tabPanel("Number of Farms",
                     sidebarLayout(
                       sidebarPanel("Select Number of Farms",
                                    checkboxGroupInput(inputId = "num_farms",
                                                       label = "Select Farm Size",
                                                       choices = list(
                                                         "Small" = 2,
                                                         "Medium" = 5,
                                                         "Large" = 10),
                                                       selected = 2
                                                       )),
                       mainPanel("Output",
                                 plotOutput("numfarms")
                       ))))

)


# Define server logic required to draw a histogram
server <- function(input, output) {

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

  year_reactive <- reactive({

    app_data %>%
      filter(year %in% input$year_range)

  })

  output$biomass_plot <- renderPlot(
    ggplot(data = year_reactive(), aes(x = year, y = tot_bm)) +
      geom_point(aes(color = mgmt, size = frm_sz))
  )

  numfarms_reactive <- reactive({

    app_data %>%
      filter(frm_sz %in% input$num_farms)
  })

  output$numfarms <- renderPlot(
    ggplot(data = numfarms_reactive(), aes(x = year, y = abund)) +
      geom_jitter(aes(color = frmsz_class))+
      facet_wrap(~mgmt) +
      labs(color = "Farm Size", x = "Number of Years", y = "Abundance of Fish")+
      theme_minimal()
  )

}

# Run the application
shinyApp(ui = ui, server = server)
