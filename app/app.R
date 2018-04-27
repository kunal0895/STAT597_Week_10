library(shiny)
library(tidyverse)
library(tidycensus)
library(ggplot2)

ui <- fluidPage(
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
     #Input box to select a US state, where the default selected is NJ
      selectInput(
       inputId = "state",
       label = "state",
       choices = c(
         'Alaska',
         'Alabama',
         'Arkansas',
         'Arizona',
         'California',
         'Colorado',
         'Connecticut',
         'District of Columbia',
         'Delaware',
         'Florida',
         'Georgia',
         'Hawaii',
         'Iowa',
         'Idaho',
         'Illinois',
         'Indiana',
         'Kansas',
         'Kentucky',
         'Louisiana',
         'Massachusetts',
         'Maryland',
         'Maine',
         'Michigan',
         'Minnesota',
         'Missouri',
         'Mississippi',
         'Montana',
         'North Carolina',
         'North Dakota',
         'Nebraska',
         'New Hampshire',
         'New Jersey',
         'New Mexico',
         'Nevada',
         'New York',
         'Ohio',
         'Oklahoma',
         'Oregon',
         'Pennsylvania',
         'Rhode Island',
         'South Carolina',
         'South Dakota',
         'Tennessee',
         'Texas',
         'Utah',
         'Virginia',
         'Vermont',
         'Washington',
         'Wisconsin',
         'West Virginia',
         'Wyoming'
       ),
      selected = "New Jersey"
      ),
      
      #Radio button to select the type of plot
      # The Default value is the "Median Household Income"
      
      radioButtons(
        inputId = "plotType",
        label = "Select what you want to plot out of the following:",
        choices = c(
          "Median Household Income",
          "Median Gross Rent",
          "Ratio of Median Gross Rent to Median Household Income"
        ),
        selected = "Median Household Income"
      )
   ),
   
   mainPanel(plotOutput("main_plot"),
             tableOutput("results"))
),

titlePanel(" "))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$main_plot <- renderPlot({
     state <- input$state
     plot_type <- input$plotType
     if(plot_type == "Median Household Income") {
       
       # Fetching median income data from ACS
       df_median_income <- 
         get_acs(
           geography = "county",
           variables = c(medincome = "B19013_001"),
           state = state,
           geometry = TRUE
         )
       
       #Plotting median income by county
       df_median_income %>%
         ggplot(aes(fill = estimate, color = estimate)) + 
         geom_sf() + scale_fill_gradient(low = "white", high="#004414") +
         scale_color_gradient(low = "white", high = "#004414") + 
         ggtitle("Median Household Income") +
         guides(
           fill = guide_legend(title = "Median Household Income"),
           colour = guide_legend(title = "Median Household Income")
         ) + theme_bw()
     }
     
     else if (plot_type == "Median Gross Rent") {
       
       # Fetching median rent data from ACS
       df_median_rent <- 
         get_acs(
           geography = "county",
           variables = c(medincome = "B25064_001"),
           state = state,
           geometry = TRUE
         )
       
       #Plotting median rent by county
       df_median_rent %>%
         ggplot(aes(fill = estimate, color = estimate)) + 
         geom_sf() + scale_fill_gradient(low = "white", high="#001344") +
         scale_color_gradient(low = "white", high = "#001344") + 
         ggtitle("Median Gross Rent") +
         guides(
           fill = guide_legend(title = "Median Gross Rent"),
           colour = guide_legend(title = "Median Gross Rent")
         ) + theme_bw()
     }
     
     else {
       
       # Fetching median income data from ACS
       df_median_income <-
         get_acs(
           geography = "county",
           variables = c(medincome = "B19013_001"),
           state = state,
           geometry = TRUE
         )
       
       # Fetching median rent data from ACS
       df_median_rent <-
         get_acs(
           geography = "county",
           variables = c(medrent = "B25064_001"),
           state = state,
           geometry = TRUE
         )
       
       #Change the column names for joining
       colnames(df_median_income)[4] <- "Income"
       colnames(df_median_rent)[4] <- "Rent"
       
       #Inner Join
       df_joined <- 
         inner_join(as.data.frame(df_median_income),
                    as.data.frame(df_median_rent),
                    by="GEOID")
       
       #Calculating the ratio of median gross rent to median household income
       df_ratio <- df_joined %>%
         mutate(Ratio = Rent/Income)
       
       #Plot the ratio by county
       df_median_income$Ratio <- df_ratio$Ratio
       df_median_income %>% ggplot(aes(fill = Ratio, color = Ratio)) +
         geom_sf() + scale_fill_gradient(low = "white", high = "#440042") + 
         scale_color_gradient(low = "white", high = "#440042") +
         ggtitle("Ratio of Median Gross Rent to Median Household Income") + 
         guides(
           fill = guide_legend(title = "Ratio of Median Gross Rent to Median Household Income"),
           colour = guide_legend(title = "Ratio of Median Gross Rent to Median Household Income")
         ) + theme_bw()
      }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)