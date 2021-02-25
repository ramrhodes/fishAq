# this is my app

library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)
library(here)

app_data <- read.csv(here("data","appDatCEOA.csv")) %>%
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
                        p("Total biomass of fish increases as the percent of management area increases. The increase in biomass in greater is constant effort management strategies.
                          The number of fish caught by fisheries is higher with constant effort management strategies and increases as the percent of protected/managed area increases.
                         "),
                        h3("Citation"),
                        p("Model created by Jessica Couture, University of California, Santa Barbara")
                      )
                      ),

             tabPanel("Biomass change with Management Area",
                      sidebarLayout(
                        sidebarPanel("Select Managment Area",
                                     checkboxGroupInput("check_mgmt_size",
                                                        label = "Management Area",
                                                        choices = unique(
                                                          app_data$mgmt_area
                                                        ))),
                        mainPanel("Projected Fish Biomass in Management Area",
                                  plotOutput("biomass_mgmt")
                                  ))),

             tabPanel("Fishery Catch and Managment Area",
                      sidebarLayout(
                        sidebarPanel("Select Managment Area",
                                    radioButtons("select_size",
                                                inputId = "mgmt_area",
                                                label = "Managment Area",
                                                choices =unique(
                                                  app_data$mgmt
                                                    ))),
                        mainPanel("Projected Number of Fish Caught with Different Management Conditions",
                                  plotOutput("catch_mgmt")
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

  biomass_mgmt_reactive <-reactive({

    app_data %>%
      filter(mgmt_area %in% input$check_mgmt_size)
  })

  output$biomass_mgmt <- renderPlot(
    ggplot(data = biomass_mgmt_reactive(), aes(x= mgmt_area, y=tot_bm))+
      geom_col(aes(fill= mgmt))+
      labs(x="Management Area (% protected)", y="Total Biomass of Fish", color="Management Practice")
  )

  catch_mgmt_reactive <- reactive({

    app_data %>%
      filter(mgmt %in% input$mgmt_area)

  })

  output$catch_mgmt <- renderPlot(
    ggplot(data = catch_mgmt_reactive(), aes(mgmt_area, amt_caught)) +
      geom_point(aes(color = mgmt))+
      labs(x="Management Area", y="Number of Fish Caught", color="Managment Practice")
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
