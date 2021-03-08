# this is my app

library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)
library(here)

# Global data for app
## full data set
app_data <- read.csv(here("data","appDatCEOA.csv")) %>%
  clean_names() %>%
  mutate(frmsz_class = case_when(frm_sz == 2 ~ "small",
                              frm_sz == 5 ~ "medium",
                              frm_sz == 10 ~ "large")) %>%
  mutate(num_farms = ((mgmt_area*100)/frm_sz)) %>%
  mutate(mgmt = case_when(mgmt == "OpenAccess" ~ "Open Access",
                          mgmt == "ConstantEffort_MSY" ~ "Constant Effort"))

## biomass by farm size
app_data_biomass <- app_data %>%
  group_by(year) %>%
  count(frmsz_class, mgmt, wt = tot_bm) %>%
  filter(year > 49) %>%
  mutate(year = year - 50) %>%
  rename(tot_bm = n)

## abundance by farm size
app_data_abund <- app_data %>%
  group_by(year) %>%
  count(frmsz_class, mgmt, wt = abund) %>%
  filter(year > 49) %>%
  mutate(year = year - 50) %>%
  rename(abund = n)

## amount caught by farm size
app_data_amt_caught<- app_data %>%
  group_by(year) %>%
  count(frmsz_class, mgmt, wt = amt_caught) %>%
  filter(year > 49) %>%
  mutate(year = year - 50) %>%
  rename(amt_caught = n)

## abundance and amount caught df combined
app_data_comb <- merge(app_data_abund, app_data_amt_caught, by=c("year", "mgmt", "frmsz_class")) %>%
  pivot_longer(cols = c("amt_caught", "abund"),
               names_to = "type",
               values_to = "value")

## abundance

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

  navbarPage("Bioeconomic Model: Fish & Protected Space",
             tabPanel("About",
                      icon = icon("fas fa-book-open"),
                      mainPanel(
                        h3("Introduction"),
                        p("This Shiny application uses data from Jessica Couture's research to explore the impacts of Marine Protected Areas (MPA) and farms on wild fish populations and fishery catches. The application provides a platform for the user to observe how variations in size of MPAs and aquaculture farms relate to biomass of fish catches, fish attraction to the farm, and movement of adult fish."),
                        h3("Overview"),
                        p("The model is a 1-D spatially explicit bioeconomic model that simulates fish movement and fishery catches around a protected space. The model is used to simulate the impacts of an MPA or farm on wild populations (abundance, biomass) and fishery catches (yield amount and yield biomass).
                          Management strategy is either Open Access, where management efforts are not put into place or Constant Effort, where fisheries are limited to number of boats and rules on catches are followed."),
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
                      icon = icon("far fa-chart-bar"),
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
                                                label = "Managment Practice",
                                                choices =unique(
                                                  app_data$mgmt
                                                    ))),
                        mainPanel("Projected Number of Fish Caught with Different Management Conditions",
                                  plotOutput("catch_mgmt")
                                  ))),

            tabPanel("Fish Biomass Over Time",
                     icon = icon("fas fa-hourglass-half"),
                     sidebarLayout(
                       sidebarPanel("Select Date Range",
                                    sliderInput(inputId = "year_range",
                                                label = "Time",
                                                min = 0,
                                                max = 100,
                                                value = 100
                                    )),
                       mainPanel("Fish Biomass Over Time",
                                 plotOutput("biomass_plot")
                                 ))),

            tabPanel("Number of Fish",
                     icon = icon("fas fa-fish"),
                     sidebarLayout(
                       sidebarPanel("Select Number of Farms",
                                    selectInput(inputId = "select_type",
                                                       label = "Select",
                                                       choices = list(
                                                         "Abundance" = "abund",
                                                         "Amount Caught"= "amt_caught"),
                                                selected = "abund"
                                                )),
                       mainPanel("Abundance of Fish vs Fish Caught",
                                 plotOutput("type")
                       ))))

)


# Define server logic required to draw a histogram
server <- function(input, output) {

  biomass_mgmt_reactive <-reactive({

    app_data %>%
      filter(mgmt_area %in% input$check_mgmt_size) %>%
      mutate(tot_mil_bm = tot_bm/1e+06)
  })

  output$biomass_mgmt <- renderPlot(
    ggplot(data = biomass_mgmt_reactive(), aes(x= mgmt_area, y=tot_mil_bm))+
      geom_col(aes(fill= mgmt))+
      labs(x="Management Area (% protected)",
           y="Total Biomass of Fish (million kg)",
           fill="Management Practice")
  )

  catch_mgmt_reactive <- reactive({

    app_data %>%
      filter(mgmt %in% input$mgmt_area) %>%
      mutate(amt_10k_caught= amt_caught/10000)

  })

  output$catch_mgmt <- renderPlot(
    ggplot(data = catch_mgmt_reactive(), aes(mgmt_area, amt_10k_caught)) +
      geom_point(aes(color = mgmt))+
      scale_y_continuous(
        breaks = seq(0, 13, by=1),
        labels = seq(0, 13, by=1),
        limits = c(0,13))+
      labs(x="Management Area",
           y="Number of Fish Caught (10,000s)",
           color="Managment Practice")
  )

  year_reactive <- reactive({

    app_data_biomass%>%
      filter(year < input$year_range)

  })

  output$biomass_plot <- renderPlot(
    ggplot(data = year_reactive(), aes(x = year, y = tot_bm)) +
      geom_line(aes(color = frmsz_class)) +
      facet_wrap(~mgmt, scales = "free") +
      labs(x = "Time (years)", y = "Total Biomass", color = "Farm Size") +
      scale_color_manual(values=c("#003f5c", "#bc5090", "#ffa600"))+
      scale_x_continuous(breaks = seq(0, 100, by=25), labels = seq(0, 100, by=25)) +
      theme_minimal()
  )

  type_reactive <- reactive({

    app_data_comb %>%
      filter(type %in% input$select_type)
  })

  output$type <- renderPlot(
    ggplot(data = type_reactive(), aes(x = year, y = value)) +
      geom_line(aes(color = frmsz_class))+
      facet_wrap(~mgmt) +
      scale_color_manual(values=c("#003f5c", "#bc5090", "#ffa600"))+
      labs(color = "Farm Size", x = "time (years)", y = "Number of Fish")+
      scale_x_continuous(breaks = seq(0, 100, by=25), labels = seq(0, 100, by=25)) +
      theme_minimal()
  )

}

# Run the application
shinyApp(ui = ui, server = server)
