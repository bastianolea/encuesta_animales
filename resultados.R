library(surveydown)
library(dplyr)
library(lubridate)

db <- sd_database(
  host   = "aws-0-us-west-1.pooler.supabase.com",
  dbname = "postgres",
  port   = "6543",
  user   = "postgres.cfzimauvcjpylqxojaxe",
  table  = "encuesta_shiny_1",
  ignore = FALSE # si es true, guarda respuestas en un .csv en lugar de en la base
)

# datos <- readr::read_csv("data.csv")

datos <- sd_get_data(db) |>
  tibble() |>
  select(everything(), -starts_with("time"), time_start, time_end)


# cerrar conexión
pool::poolClose(db$db)
