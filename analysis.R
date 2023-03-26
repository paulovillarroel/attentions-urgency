library(tidyverse)
library(lubridate)
library(janitor)

## Data obtained from DEIS

url <- "https://repositoriodeis.minsal.cl/SistemaAtencionesUrgencia/AtencionesUrgencia2023.zip"

download.file(url, destfile = "data_raw/AtencionesUrgencia2023.zip")

unzip("data_raw/AtencionesUrgencia2023.zip", exdir = "data_raw/")

data <- data.table::fread("data_raw/AtencionesUrgencia2023.csv", encoding = "Latin-1") |>
  clean_names()

respiratory_cause <- data |>
  filter(
    glosacausa == "TOTAL CAUSAS SISTEMA RESPIRATORIO",
    semana != max(semana)
  ) |>
  group_by(semana) |>
  summarise(total = sum(total))

# Plot

respiratory_cause |>
  ggplot(aes(semana, total)) +
  geom_area(fill = "#e0aaff", color = "#7b2cbf", size = 2) +
  geom_point(color = "#724cf9", size = 3) +
  scale_x_continuous(breaks = 1:nrow(respiratory_cause)) +
  scale_y_continuous(labels = scales::comma) +
  theme_grey() +
  labs(
    title = "Atenciones de urgencia por causas respiratorias. Chile - 2023",
    subtitle = "Se incluyen todos los servicios de urgencia del país\nTodas las edades\n",
    x = "\nSemana estadística",
    y = "N° de atenciones",
    caption = "Elaborado por Paulo Villarroel | Fuente: DEIS"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "#e9ecef"),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "#ced4da", linewidth = 0.2),
    panel.grid.minor = element_line(color = "#ced4da", linewidth = 0.2),
    legend.background = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("plots/respiratory_cause_2023.png", height = 17, width = 23, units = "cm")
