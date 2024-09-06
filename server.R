################
# config setup #
################

# Use the sd_config() function to customize features in your survey, like
# logic to conditionally display questions or skip to pages based on responses
# to skip other questions in your survey. See documentation for details at
# https://surveydown.org/config

config <- sd_config()

###################################
# Get reactive data and make plot #
###################################

data <- sd_get_data(db, reactive = TRUE, refresh_interval = 5) # Every 5 sec

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
