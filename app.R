library(shiny)
library(bslib)
library(DT)
library(httr2)
library(dplyr)
library(tidyr)
library(waiter)

source("get_data.R")
source("log_search.R")

# Modern theme with bslib
my_theme <- bs_theme(
  bg = "#F1F1F1", fg = "#000", primary = "#B86800", secondary = "#FC9838",
  base_font = font_google("Press Start 2P"),
  code_font = font_google("Press Start 2P"),
  "font-size-base" = "0.65rem", "enable-rounded" = TRUE
) 

ui <- fluidPage(
  theme = my_theme,
  autoWaiter(),
  br(),
  titlePanel("Job & Learning Explorer"),
  br(),
  h6("Explore job vacancies, companies, skills, learning tracks, and book recommendations."),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Search data"),
      selectInput("category", "Category:",
                  choices = c("Skills", "Companies", "Vacancies", "Learning Tracks", "Books")),
      numericInput("default_limit", "Number of records:", 
                   value = 10, 
                   min = 1, 
                   max = 1000,
                   step = 5),
      uiOutput("dynamic_filters"),
      actionButton("go", "Search", icon = icon("search")),
      hr(),
      helpText("Select a category and enter a search term to filter the data.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Skills", DTOutput("skills_table")),
        tabPanel("Companies", DTOutput("companies_table")),
        tabPanel("Vacancies", DTOutput("vacancies_table")),
        tabPanel("Learning Tracks", DTOutput("tracks_table")),
        tabPanel("Books", DTOutput("books_table")),
        tabPanel("Log Search",
                 fluidRow(
                   column(6,
                          br(),
                          numericInput("user_id", "User ID:", value = 1, min = 1),
                          textInput("search_query", "Search Query:", value = ""),
                          actionButton("log_button", "Log Search")
                   ),
                   column(6,
                          verbatimTextOutput("log_result")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  #dynamic filters
  output$dynamic_filters <- renderUI({
    req(input$category)
    
    switch(input$category,
           "Vacancies" = tagList(
             numericInput("company_filter", "Filter by Company ID:", value = NA),
             textInput("canton_filter", "Filter by Canton:")
           ),
           "Learning Tracks" = tagList(
             textInput("skill_id_filter", "Filter by Skill ID:")
           ),
           "Books" = tagList(
             textInput("book_skill_filter", "Filter by Skill ID:")
           ),
           NULL # default for Skills and Companies that do not have filters
    )
  })
  
  # Reactive value to store the current data
  current_data <- reactiveVal(NULL)
  
  # Observer for the search button
  observeEvent(input$go, {
    category <- tolower(gsub(" ", "_", input$category))

    
    # Base list of arguments
    args <- list(
      endpoint = category,
      limit = input$default_limit
    )
    
    # Add specific filters according to category
    if (category == "vacancies") {
      if (!is.null(input$company_filter) && !is.na(input$company_filter)) 
        args$company <- input$company_filter
      if (!is.null(input$canton_filter) && nchar(input$canton_filter) > 0) 
        args$canton <- input$canton_filter
    }
    else if (category == "learning_tracks") {
      if (!is.null(input$skill_id_filter) && nchar(input$skill_id_filter) > 0) 
        args$skill_id <- input$skill_id_filter
    }
    else if (category == "books") {
      if (!is.null(input$book_skill_filter) && nchar(input$book_skill_filter) > 0) 
        args$skill <- input$book_skill_filter
    }
    
    # Call the API using do.call
    tryCatch({
      data <- do.call(get_data, args)
      current_data(data)
    }, error = function(e) {
      showNotification(
        paste("Error fetching data:", e$message),
        type = "error"
      )
    })
    waiter_hide()
  })
  
  # Skills table output
  output$skills_table <- renderDT({
    
    # w = Waiter$new() # 2. Initialize
    # w$show() # 3. Program
    req(input$go, input$category == "Skills")

    data <- current_data()


    if (!is.null(data)) {
      datatable(
        data.frame(
          Skill_ID = sapply(data, `[[`, "skill_id"),
          Skill_Label = sapply(data, `[[`, "skill_label")
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        )      
      )
      # w$hide() # 4. Hide
    }
  })
  
  # Companies table output
  output$companies_table <- renderDT({
    req(input$go, input$category == "Companies")
    data <- current_data()
    if (!is.null(data)) {
      datatable(
        data.frame(
          Company_ID = sapply(data, `[[`, "company_id"),
          Name = sapply(data, `[[`, "name"),
          Sector = sapply(data, `[[`, "sector")
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        )
      )
    }
  })
  
  # Vacancies table output
  output$vacancies_table <- renderDT({
    req(input$go, input$category == "Vacancies")
    data <- current_data()
    if (!is.null(data)) {
      datatable(
        data.frame(
          Vacancy_ID = sapply(data, `[[`, "vacancy_id"),
          Company_ID = sapply(data, `[[`, "company_id"),
          Canton = sapply(data, `[[`, "canton"),
          Occupation = sapply(data, `[[`, "occupation"),
          Year = sapply(data, `[[`, "year"),
          Month = sapply(data, `[[`, "month")
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip')
      )
    }
  })
  
  # Learning Tracks table output
  output$tracks_table <- renderDT({
    req(input$go, input$category == "Learning Tracks")
    data <- current_data()
    if (!is.null(data)) {
      datatable(
        data.frame(
          Track_ID = sapply(data, `[[`, "track_id"),
          Title = sapply(data, `[[`, "title"),
          Description = sapply(data, `[[`, "description"),
          URL = sapply(data, `[[`, "url")
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip')
      )
    }
  })
  
  # Book Recommendations table output
  output$books_table <- renderDT({
    req(input$go, input$category == "Books")
    data <- current_data()
    if (!is.null(data)) {
      datatable(
        data.frame(
          Book_ID = sapply(data, `[[`, "book_id"),
          Title = sapply(data, `[[`, "title"),
          Author = sapply(data, `[[`, "author"),
          Skill_ID = sapply(data, `[[`, "skill_id")
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip')
      )
    }
  })
}
w <- Waiter$new(
  id = "skills_table",
  html = spin_fading_circles(),
  color = "black"
)
# Observer for a "Log Search" (POST)
observeEvent(input$log_button, {
  req(input$user_id, input$search_query)

  tryCatch({
    res <- log_search(
      user_id = input$user_id,
      query = input$search_query
    )

    output$log_result <- renderPrint({
      paste("Remaining queries:", res)
    })


  }, error = function(e) {
    showNotification(
      paste("Error logging search:", e$message),
      type = "error"
    )
  })
})


shinyApp(ui, server)