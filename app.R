library(surveydown)
library(dplyr)
library(ggplot2)
library(scales)


# conectar a la base de datos
db <- sd_database(
  host   = "aws-0-us-west-1.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.cfzimauvcjpylqxojaxe",
  table  = "encuesta_shiny_1",
  ignore = FALSE
)


# funciones ----

color_fondo = "#F2F6F9"

grafico_torta <- function(data,
                          valores = c("Sí" = "#23b0bd", "No" = "#f72078")) {
  data |>
    ggplot(aes(x = p, y = factor(1), fill = variable, color = variable)) +
    geom_col(width = 1, linewidth = 1.2, lineend = "round",
             color = color_fondo) +
    geom_point(alpha = 0) +
    coord_radial(expand = FALSE, clip = "off") +
    geom_text(aes(y = 1.2,
                  label = ifelse(p > 0.05, percent(p, accuracy = 1), "")),
              color = "white", fontface = "bold",
              position = position_stack(vjust = 0.5)) +
    theme_void() +
    scale_fill_manual(values = valores,
                      aesthetics = c("color", "fill")) +
    guides(fill = guide_none(),
           color = guide_legend(override.aes = list(alpha = 1, size = 5))) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 11),
          legend.margin = margin(l = -18))
}


fondo_grafico <- list(theme(plot.background = element_rect(fill = color_fondo,
                                                           linewidth = 0),
                            panel.background = element_rect(fill = color_fondo,
                                                            linewidth = 0),
                            legend.background = element_rect(fill = color_fondo,
                                                             linewidth = 0)
))

espaciado_grafico <- list(theme(plot.margin = unit(rep(6, 4), "mm"))
)



server <- function(input, output, session) {

  # datos ----
  datos <- sd_get_data(db, refresh_interval = 10)

  # datos_base <- sd_get_data(db)
  # datos <- function() {tibble(datos_base)}
  # datos()

  output$respuestas <- renderText({
    datos() |> filter(animal != "") |> tally() |> pull()
  })

  # gráficos ----

  output$animal <- renderPlot({
    req(nrow(datos() >= 2))

    datos() |>
      count(variable = animal) |>
      filter(variable != "") |>
      mutate(p = n/sum(n)) |>
      grafico_torta(valores = c("Gato" = "#ff8172", "Perro" = "#3a579a")) +
      espaciado_grafico +
      fondo_grafico

  }, bg = color_fondo)


  datos_edad <- reactive({
    datos_edad <- datos() |>
      mutate(edad = as.numeric(edad)) |>
      filter(!is.na(edad)) |>
      filter(edad >= 18) |>
      filter(edad < 85)
  })

  output$edad <- renderPlot({
    req(nrow(datos() >= 2))
    # browser()
    # datos <- \() tibble(edad = c(30, 20, 35, 45, 23, 24, 25, 34, 31, 30))

    datos_edad() |>
      # add_row(edad = 85) |>
      # add_row(edad = 18) |>
      ggplot(aes(x = edad)) +
      geom_density(fill = "#c63584", color = "#c63584",
                   alpha = 0.8, linewidth = 1.1,
                   adjust = .7) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold",
                                       margin = margin(t = 0, b = 10))) +
      espaciado_grafico +
      fondo_grafico

  }, bg = color_fondo)



  output$animal_edad <- renderPlot({
    req(nrow(datos() >= 2))
    # browser()
    # datos <- \() tibble(edad = c(30, 20, 35, 45, 23, 24, 25, 34, 31, 30))

    datos_edad() |>
      # add_row(edad = 85) |>
      # add_row(edad = 18) |>
      filter(animal != "") |>
      ggplot(aes(x = edad, fill = animal, color = animal)) +
      geom_density(alpha = 0.5, linewidth = 1.1,
                   adjust = .7) +
      scale_fill_manual(values = c("Gato" = "#ff8172", "Perro" = "#3a579a"),
                        aesthetics = c("fill", "color")) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold",
                                       margin = margin(t = -2, b = 10))) +
      espaciado_grafico +
      fondo_grafico +
      guides(fill = guide_legend(position = "bottom")) +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 11),
            legend.key.spacing.x = unit(7, "mm"),
            legend.margin = margin(l = 4))

  }, bg = color_fondo)



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
      espaciado_grafico +
      fondo_grafico

  }, bg = color_fondo)


  output$animal_genero <- renderPlot({
    req(nrow(datos() >= 2))

    datos_genero_animal <- datos() |>
      count(genero, animal) |>
      filter(genero != "",
             animal != "") |>
      mutate(p = n/sum(n)) |>
      mutate(total = sum(n), .by = animal)

    datos_genero_animal_p <- datos_genero_animal |>
      select(animal, n) |>
      summarize(n = sum(n), .by = animal) |>
      mutate(p_total = n/sum(n)) |>
      select(animal, p_total)

    datos_genero_animal |>
      left_join(datos_genero_animal_p) |>
      ggplot(aes(x = animal, y = n, fill = genero)) +
      geom_col(position = position_stack(), width = .5, color = color_fondo, linewidth = 1.2) +
      geom_text(aes(label = ifelse(p > 0.05, percent(p, accuracy = 1), "")),
                color = "white", fontface = "bold",
                position = position_stack(vjust = 0.5)) +
      geom_text(aes(label = percent(p_total, accuracy = 1), y = total),
                color = "black", fontface = "bold",
                check_overlap = TRUE, vjust = 0,
                nudge_y = mean(datos_genero_animal$n)*0.08) +
      scale_fill_manual(values = c("Femenino" = "#ff6fcf",
                                   "Masculino" = "#5460e2",
                                   "No binario/Otros" = "#be5fee",
                                   "Prefiero no responder" = "#856b91")) +
      theme_void() +
      fondo_grafico +
      espaciado_grafico +
      theme(axis.text.x = element_text(face = "bold",
                                       margin = margin(t = 0, b = 10)),
            legend.title = element_blank(),
            legend.text = element_text(size = 11),
            legend.margin = margin(l = -18))

  }, bg = color_fondo)


  # configuraciones de la base de datos
  sd_server(
    db = db
    # required_questions = "animal"
    # all_questions_required = TRUE
  )

}

shiny::shinyApp(ui = sd_ui(), server = server)
