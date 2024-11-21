# remotes::install_github("surveydown-dev/surveydown", force = TRUE)
library(surveydown)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(scales)
library(purrr)
library(htmltools)

# Database setup

# For this demo, you need to create a database at https://supabase.com/
# and connect to it using the sd_database() function with details
# from your supabase database. See the documentation for details:
# https://surveydown.org/store-data

db <- sd_database(
  host   = "aws-0-us-west-1.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.cfzimauvcjpylqxojaxe",
  table  = "encuesta_shiny_2",
  ignore = FALSE
)

# funciones ----
grafico_torta <- function(data,
                          valores = c("Sí" = "#23b0bd", "No" = "#f72078")) {
  data |>
    ggplot(aes(x = p, y = factor(1), fill = variable, color = variable)) +
    geom_col(width = 1, linewidth = 1.2, color = "white") +
    geom_point(alpha = 0) +
    coord_radial(expand = FALSE) +
    geom_text(aes(y = 1.2, label = percent(p, accuracy = 1)),
              color = "white", fontface = "bold",
              position = position_stack(vjust = 0.5)) +
    theme_void() +
    scale_fill_manual(values = valores,
                      aesthetics = c("color", "fill")) +
    guides(fill = guide_none(),
           color = guide_legend(override.aes = list(alpha = 1, size = 5))) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.margin = margin(l = -20))
}


fondo_grafico <- list(theme(plot.background = element_rect(fill = "#F2F6F9",
                                                           linewidth = 0),
                            panel.background = element_rect(fill = "#F2F6F9",
                                                            linewidth = 0),
                            legend.background = element_rect(fill = "#F2F6F9",
                                                             linewidth = 0)
))



server <- function(input, output, session) {

  # datos ----
  datos <- sd_get_data(db, refresh_interval = 6)



  # gráficos ----

  output$animo <- renderPlot({
    req(nrow(datos() >= 2))

    # datos <- \() tibble(animo = c("Muy mal",
    #                               "Mal",
    #                               "Más o menos",
    #                               "Más o menos",
    #                               "Bien",
    #                               "Bien",
    #                               "Bien",
    #                               "Muy bien"))

    datos() |>
      filter(animo != "") |>
      mutate(animo = case_match(animo,
                                "Muy mal" ~ -2,
                                "Mal" ~ -1,
                                "Más o menos" ~ 0,
                                "Bien" ~ 1,
                                "Muy bien" ~ 2)) |>
      ggplot(aes(x = animo)) +
      geom_density(fill = "#c63584", color = "#c63584",
                   alpha = 0.8, linewidth = 1.1) +
      scale_x_continuous(labels = c("Muy mal", "Mal", "Más o menos", "Bien", "Muy bien"),
                         breaks = c(-2, -1, 0, 1, 2),
                         limits = c(-2, 2),
                         expand = expansion(c(0.1, 0.1))) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold", margin = margin(t = -10, b = 10))) +
      fondo_grafico
  }, bg = "#F2F6F9")


  output$animal <- renderPlot({
    req(nrow(datos() >= 2))

    datos() |>
      count(variable = animal) |>
      filter(variable != "") |>
      mutate(p = n/sum(n)) |>
      grafico_torta(valores = c("Gato" = "#ff8172", "Perro" = "#3a579a")) +
      fondo_grafico
  }, bg = "#F2F6F9")


  output$edad <- renderPlot({
    req(nrow(datos() >= 2))

    # datos <- \() tibble(edad = c(18, 30, 40, 35, 36, 37, 24, 26, 15, 30, 40, 20, 25, 26, 24))

    datos_edad <- datos() |>
      filter(edad != "",
             !is.na(edad)) |>
      mutate(edad = as.numeric(edad))

    datos_edad |>
      ggplot(aes(x = edad)) +
      geom_density(fill = "#31b954", color = "#31b954",
                   alpha = 0.8, linewidth = 1.1) +
      scale_x_continuous(expand = expansion(c(0.1, 0.1)),
                         labels = scales::number_format(accuracy = 1, suffix = " años")) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold", margin = margin(t = -10, b = 10))) +
      fondo_grafico

  }, bg = "#F2F6F9")


  output$genero <- renderPlot({
    req(nrow(datos() >= 2))

    datos() |>
      count(variable = genero) |>
      filter(variable != "") |>
      mutate(p = n/sum(n)) |>
      grafico_torta(valores = c("Femenino" = "#ff6fcf",
                                "Masculino" = "#5460e2",
                                "No binario/Otros" = "#be5fee",
                                "Prefiero no responder" = "#856b91")) +
      fondo_grafico
  }, bg = "#F2F6F9")


  output$uso_r <- renderPlot({
    req(nrow(datos() >= 2))

    datos() |>
      count(variable = uso_r) |>
      filter(variable != "") |>
      mutate(p = n/sum(n)) |>
      grafico_torta() +
      fondo_grafico
  }, bg = "#F2F6F9")



  output$nivel <- renderPlot({
    req(nrow(datos() >= 2))

    datos() |>
      filter(nivel != "") |>
      mutate(nivel = forcats::fct_relevel(nivel,
                                          "Nulo",
                                          "Principiante",
                                          "Intermedio",
                                          "Avanzado",
                                          "Experto")) |>
      count(variable = nivel) |>
      mutate(p = n/sum(n)) |>
      ggplot(aes(x = p, y = variable)) +
      geom_col(width = .6, fill = "#faba61") +
      scale_x_continuous(labels = scales::label_percent(),
                         expand = expansion(c(0, 0.1))) +
      theme_void() +
      theme(axis.text.y = element_text(face = "bold", hjust = 1, margin = margin(r = 8)),
            axis.text.x = element_text(margin = margin(t = -10, b = 10))) +
      fondo_grafico
  }, bg = "#F2F6F9")


  output$uso_shiny <- renderPlot({
    req(nrow(datos() >= 2))

    datos() |>
      count(variable = uso_shiny) |>
      filter(variable != "") |>
      mutate(p = n/sum(n)) |>
      grafico_torta() +
      fondo_grafico
  }, bg = "#F2F6F9")


  output$temas <- renderUI({
    req(nrow(datos() >= 1))

    # datos <- \() tibble(animo = c("Muy mal",
    #                               "Mal",
    #                               "Más o menos",
    #                               "Más o menos",
    #                               "Bien",
    #                               "Bien",
    #                               "Bien",
    #                               "Muy bien"))

    temas <- datos() |>
      filter(temas != "") |>
      pull(temas) |>
      unique() |>
      str_split(pattern = ",") |>
      unlist() |>
      str_trim()

    map(temas, \(texto) {
      div(style = htmltools::css(background = "#F9F9F9",
                                 border = "1px solid #DDDDDD",
                                 border_radius = "4px",
                                 padding = "8px",
                                 margin = "12px",
                                 max_width = "300px"),
          div(style = css(opacity = "70%"),
              em(texto)
          )
      )
    })
  })


  # Database designation and other settings
  sd_server(
    db = db
    # all_questions_required = TRUE
  )

}

shiny::shinyApp(ui = sd_ui(), server = server)
