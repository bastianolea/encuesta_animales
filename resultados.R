library(surveydown)
library(dplyr)
library(lubridate)

db <- sd_database(
  host   = "aws-0-us-west-1.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.cfzimauvcjpylqxojaxe",
  table  = "encuesta_shiny_2",
  ignore = FALSE # si es true, guarda respuestas en un .csv en lugar de en la base
)

# datos <- readr::read_csv("data.csv")

datos <- sd_get_data(db) |>
  tibble() |>
  select(everything(), -starts_with("time"), time_start, time_end)

datos_horario <- datos |>
  filter(!(edad == 31 & genero == "No binario/Otros")) |>
  # select(time_start) |>
  mutate(time_start = as_datetime(time_start)) |>
  filter(time_start > as_datetime("2024-11-21 21:30:00"),
         time_start < as_datetime("2024-11-21 23:59:59"))

datos_horario |>
  count(animal)

datos_horario |>
  count(uso_r)

datos_horario |>
  count(nivel)

datos_horario |>
  count(genero)

datos |>
  count(nivel, uso_shiny)

datos |>
  select(temas) |>
  print(n=Inf)

# cerrar conexión
pool::poolClose(db$db)
