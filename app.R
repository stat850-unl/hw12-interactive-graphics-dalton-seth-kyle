#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv') %>% 
  select(drink, ingredient, measure)
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv') %>%
  select(drink = name, ingredient, measure)

all_drinks <- cocktails %>% 
  bind_rows(boston_cocktails)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Grad School Survival Guide"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          img(src = "sadness.PNG", width = "100%"),
          h6("Grad school got you down? Enter an ingredient to find cocktails containing that ingredient and get the recipe for one of the options!"),
          textInput("ingredient",
                    "Ingredient:",
                    value = "Bourbon"),
          uiOutput("recipe_for")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("n_ingredient_figure"),
          tableOutput("test_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  drinks_with_ingredient <- reactive({
    all_drinks %>% 
      filter(ingredient == input$ingredient) %>% 
      pull(drink)
  })
  
  recipes = reactive({
    all_drinks %>% 
      filter(drink %in% drinks_with_ingredient())
  })
  
  output$recipe_for <- renderUI({
    selectInput(
      "drink_of_choice",
      "Get recipe for:",
      choices = drinks_with_ingredient()
      )
    })
  
  output$n_ingredient_figure <- renderPlot({
    ggplot(recipes() %>% 
             group_by(drink) %>% 
             mutate(n_ingredients = n()), 
           aes(x = reorder(drink, n_ingredients), 
               y = n_ingredients)) + 
      geom_col() + 
      labs(x = "Drink Name", 
           y = "Number of Ingredients") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$test_table <- renderTable({
    
    recipes() %>% 
      filter(drink == input$drink_of_choice)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
