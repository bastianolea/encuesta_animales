# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(dplyr)
library(ggplot2)

# Database setup

# surveydown stores data on a database that you define at https://supabase.com/
# To connect to a database, update the sd_database() function with details
# from your supabase database. For this demo, we set ignore = TRUE, which will
# ignore the settings and won't attempt to connect to the database. This is
# helpful for local testing if you don't want to record testing data in the
# database table. See the documentation for details:
# https://surveydown.org/store-data

db <- sd_database(
    host   = "",
    dbname = "",
    port   = "",
    user   = "",
    table  = "",
    ignore = TRUE
)

# Server setup
server <- function(input, output, session) {

    # Config setup
    config <- sd_config()

    data <- sd_get_data(db, refresh_interval = 5) # Every 5 sec (default)

    # Render the plot
    output$penguin_plot <- renderPlot({
        data() |> # Note the () here, as this is a reactive function
            count(penguins) |>
            mutate(penguins = ifelse(penguins == '', 'No response', penguins)) |>
            ggplot() +
            geom_col(aes(x = n, y = reorder(penguins, n)), width = 0.7) +
            theme_minimal() +
            labs(x = "Count", y = "Penguin Type", title = "Penguin Count")
    })

    # sd_server() initiates your survey - don't change it
    sd_server(
        input   = input,
        output  = output,
        session = session,
        config  = config,
        db      = db
    )

}

# shinyApp() initiates your app - don't change it
shiny::shinyApp(ui = sd_ui(), server = server)
