# this is my app

library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)
library(here)
library(DT)

# Global data for app
## full data set
app_data <- read.csv(here("data","appDatCEOA_diffs.csv")) %>%
  clean_names() %>%
  mutate(frmsz_class = case_when(frm_sz == 2 ~ "small",
                              frm_sz == 5 ~ "medium",
                              frm_sz == 10 ~ "large")) %>%
  mutate(num_farms = ((mgmt_area*100)/frm_sz)) %>%
  mutate(mgmt_area = mgmt_area*100) %>%
  mutate(mgmt = case_when(mgmt == "OpenAccess" ~ "Open Access",
                          mgmt == "ConstantEffort_MSY" ~ "Constant Effort"))

## cleaned_up
app_clean <-  app_data %>%
  rename(farm_size = frm_sz,
         attraction = attr,
         patch_carrying_capacity = p_k,
         abundance = abund,
         total_biomass = tot_bm,
         catch_amount = amt_caught,
         catch_biomass = bm_caught,
         mgmt_type = mgmt,
         farm_size_class = frmsz_class,
         density_dep_move = dmm,
         difference_biomass = bm_diff,
         difference_catch_biomass = c_bm_diff
  ) %>%
  select(year, mgmt_type, mgmt_area, farm_size, num_farms, farm_size_class, abundance, total_biomass, catch_amount, catch_biomass, difference_biomass, difference_catch_biomass) %>%
  relocate(num_farms, .after = farm_size) %>%
  relocate(farm_size_class, .after = num_farms) %>%
  relocate(mgmt_type, .after = year)


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


## management and amount caught
app_data_mgmt_amt <- app_data %>%
  filter(year > 49) %>%
  group_by(mgmt) %>%
  count(mgmt_area, mgmt, wt = amt_caught) %>%
  rename(amt_caught = n)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

                #CSS Styles
                tags$style(HTML(
                  paste0(".shiny-input-container {background-color: #f5f5f5; border: 1px solid #e3e3e3;",
                         "padding-left: 10px; padding-right: 10px; border-radius: 3px;}")
                )),

  navbarPage("Bioeconomic Model: Fish & Protected Space",
             tabPanel("About",
                      icon = icon("fas fa-book-open"),
                      mainPanel(
                        h3("Introduction"),
                        p("This Shiny application uses data from Jessica Couture's research to explore the impacts of Marine Protected Areas (MPA) and farms on wild fish populations and fishery catches. The application provides a platform for the user to observe how variations in size of MPAs and aquaculture farms relate to biomass of fish catches, fish attraction to the farm, and movement of adult fish."),
                        h3("Overview"),
                        p("The model is a 1-D spatially explicit bioeconomic model that simulates fish movement and fishery catches around a protected space. The model is used to simulate the impacts of an MPA or farm on wild populations (abundance, biomass) and fishery catches (yield amount and yield biomass).
                          Management strategy is either Open Access, where management efforts are not put into place or Constant Effort, where fisheries are limited to number of boats and rules on catches are followed."),
                        img(src = "fish_diagram.png"),
                        h3("Key Findings"),
                        p("Total biomass of fish increases as the percent of management area increases. The increase in biomass in greater is constant effort management strategies.
                          The number of fish caught by fisheries is higher with constant effort management strategies and increases as the percent of protected/managed area increases.
                         "),
                        h3("Citation"),
                        p("Model created by PhD candidate Jessica Couture, Bren School of Enviornmental Science and Management, University of California, Santa Barbara"),
                        p("App designed by Laurel Wee and Rachel Rhodes, Bren School of Enviornmental Science and Management, University of California, Santa Barbara")
                      )
                      ),

             tabPanel("Data",
                      icon = icon("fas fa-table"),
                      h2("Model Output"),
                      DT::dataTableOutput("mytable")
             ),


             tabPanel("Biomass change with Management Area",
                      icon = icon("far fa-chart-bar"),
                      sidebarLayout(
                        sidebarPanel("Select Managment Area",
                                     checkboxGroupInput("check_mgmt_size",
                                                        label = "Management Area (% Protected)",
                                                        choices = list(10, 20, 30, 40, 50, 60
                                                        ),
                                                        selected = c(10,20)),
                                     h5("Compare the total biomass of fish (million kg) for each management area. The management area refers to the number of patches protected by farms or marine protected areas. This plot is showing the biomass of fish once the model reaches equilibrium.")
                                     ),
                        mainPanel("Projected Fish Biomass in Management Area at Equilibrium",
                                  plotOutput("biomass_mgmt")
                                  ))),

             tabPanel("Fishery Catch by Management Type",
                      icon = icon("fas fa-water"),
                      sidebarLayout(
                        sidebarPanel("Select Management Type",
                                    radioButtons("select_size",
                                                inputId = "mgmt_area",
                                                label = "Management Practice",
                                                choices =unique(
                                                  app_data$mgmt
                                                    )
                                                ),
                                    h5("Compare how the number of fish caught changes depending on the management type. Open access management refers to farms located in fisheries with very little oversight/enforcement and constant effort refers to farms located in fisheries with limits/enforcement. NUmber of fish caught are projected amounts to occur by the model once equilibrium occurs.")),
                        mainPanel("Projected Number of Fish Caught with Different Management Conditions at Equilibrium",
                                  plotOutput("catch_mgmt")
                                  ))),

            tabPanel("Fish Biomass Over Time",
                     icon = icon("fas fa-hourglass-half"),
                     sidebarLayout(
                       sidebarPanel("Select Time Range",
                                    sliderInput(inputId = "year_range",
                                                label = "Number of years",
                                                min = 1,
                                                max = 100,
                                                value = 50
                                    ),
                                    h5("See how fish biomass (million kg) changes in different farm sizes over time. Constant effort (shown on the left) refers to fisheries with limits/enforcement. Open access (shown on the right) refers to fisheries with very little oversight/enforcement. The farm size refers to the number of patches per farm (large farms = 10 patches, medium = 5 patches, small = 2 patches).")),
                       mainPanel("Fish Biomass Over Time",
                                 plotOutput("biomass_plot")
                                 ))),

            tabPanel("Number of Fish",
                     icon = icon("fas fa-fish"),
                     sidebarLayout(
                       sidebarPanel("Abundance vs Fish Catch",
                                    selectInput(inputId = "select_type",
                                                       label = "Select",
                                                       choices = list(
                                                         "Abundance" = "abund",
                                                         "Catch"= "amt_caught"),
                                                selected = "abund"
                                                ),
                                    h5("Compare the impact of farms on the abundance of wild fish populations (number of fish) vsersus the impact on fishery catches (number of fish caught) over time. The farm size refers to the number of patches per farm (large farms = 10 patches, medium = 5 patches, small = 2 patches)")),
                       mainPanel("Abundance of Fish vs Fish Caught",
                                 plotOutput("type")
                       )))

))


# Define server logic required to draw a histogram
server <- function(input, output) {

  biomass_mgmt_reactive <-reactive({

    app_data %>%
      filter(mgmt_area %in% input$check_mgmt_size) %>%
      filter(year== max(year)) %>%
      mutate(tot_mil_bm = tot_bm/1e+06)
  })

  output$biomass_mgmt <- renderPlot(
    ggplot(data = biomass_mgmt_reactive(), aes(x= mgmt_area, y=tot_mil_bm))+
      geom_col(aes(fill= mgmt), position = "dodge")+
      scale_fill_manual(values=c("#004c6d", "#7aa6c2"))+
      scale_x_continuous(
        breaks = seq(0, 60, by=10),
        labels = seq(0, 60, by=10),
        limits = c(0,70))+
      scale_y_continuous(
        breaks = seq(0, .9, by=.1),
        labels = seq(0, .9, by=.1),
        limits = c(0,.85))+
      labs(x="Management Area (% protected)",
           y="Total Biomass of Fish (million kg)",
           fill="Management Practice")
  )

  catch_mgmt_reactive <- reactive({

    app_data %>%
      filter(mgmt %in% input$mgmt_area) %>%
      filter(year == max(year)) %>%
      group_by(mgmt) %>%
      mutate(amt_10k_caught= amt_caught/10000)

  })

  output$catch_mgmt <- renderPlot(
    ggplot(data = catch_mgmt_reactive(), aes(x = mgmt_area, y = amt_10k_caught)) +
      geom_col(aes(fill = mgmt, position = "dodge"))+
      scale_fill_manual(values="#004c6d")+
      labs(x="Management Area (% protected)",
           y="Number of Fish Caught (10,000s)",
           fill="Managment Practice")+
      scale_x_continuous(
        breaks = seq(0, 60, by=10),
        labels = seq(0, 60, by=10),
        limits = c(0,70))+
      scale_y_continuous(
        breaks = seq(0, 1, by=.1),
        labels = seq(0, 1, by=.1),
        limits = c(0,1))
  )

  year_reactive <- reactive({

    app_data_biomass%>%
      filter(year < input$year_range) %>%
      mutate(tot_bm_mil = tot_bm/1e+06)

  })

  output$biomass_plot <- renderPlot(
    ggplot(data = year_reactive(), aes(x = year, y = tot_bm_mil)) +
      geom_line(aes(color = frmsz_class)) +
      facet_wrap(~mgmt) +
      labs(x = "Time (years)", y = "Total Biomass (million kg)", color = "Farm Size") +
      scale_color_manual(values=c("#003f5c", "#bc5090", "#ffa600"))+
      scale_x_continuous(breaks = seq(0, 100, by=25), labels = seq(0, 100, by=25)) +
      scale_y_continuous(breaks = seq(0, 4, by=1),
                         labels = seq(0, 4, by=1),
                         limits = c(0,4))+
      theme_minimal()
  )

  type_reactive <- reactive({

    app_data_comb %>%
      filter(type %in% input$select_type) %>%
      mutate(value_10k=value/1e+04)
  })

  output$type <- renderPlot(
    ggplot(data = type_reactive(), aes(x = year, y = value_10k)) +
      geom_line(aes(color = frmsz_class))+
      facet_wrap(~mgmt, scales = "free") +
      scale_color_manual(values=c("#003f5c", "#bc5090", "#ffa600"))+
      labs(color = "Farm Size", x = "Time (years)", y = "Number of Fish (10,000s)")+
      scale_x_continuous(breaks = seq(0, 100, by=25), labels = seq(0, 100, by=25)) +
      theme_minimal()
  )

  output$mytable = DT::renderDataTable({
    app_clean
  })

}

# Run the application
shinyApp(ui = ui, server = server)
