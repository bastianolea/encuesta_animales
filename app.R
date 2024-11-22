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
    geom_col(width = 1, linewidth = 1.2, color = color_fondo) +
    geom_point(alpha = 0) +
    coord_radial(expand = FALSE, clip = "off") +
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



  output$edad <- renderPlot({
    req(nrow(datos() >= 2))

    # datos <- \() tibble(edad = c(30, 20, 35, 45, 23, 24, 25, 34, 31, 30))

    datos_edad <- datos() |>
      filter(edad != "",
             !is.na(edad),
             edad >= 18,
             edad < 85) |>
      mutate(edad = as.numeric(edad))

    datos_edad |>
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
      # coord_cartesian(clip = "off") +
      #                 xlim = c(18*0.5, max(datos_edad$edad*1.25))
      #                 ) +
      # xlim(18*0.5, max(datos_edad$edad*1.25)) +
      # scale_x_continuous(limits = c(15, 85),
                         # breaks = c(20, 30, 40, 50, 60, 70, 80)) +
      # geom_vline(xintercept = c(20, 30, 40, 50, 60, 70, 80), color = color_fondo, alpha = .2) +
      fondo_grafico

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


  # configuraciones de la base de datos
  sd_server(
    db = db
    # required_questions = "animal"
    # all_questions_required = TRUE
  )

}

shiny::shinyApp(ui = sd_ui(), server = server)
