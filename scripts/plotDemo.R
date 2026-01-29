library(plotly)

LongPlot <- dataLong |>
  filter(instrument == "DAD") |>
  mutate(sample_id = paste(enzyme, sample, time_point, species, sep = "_")) |>
  ggplot(
    aes(
      x = time,
      y = intensity,
      color = species,
      group = sample_id,
      text = paste(
        "Sample:", sample_id,
        "<br>Time:", round(time, 3),
        "<br>Intensity:", signif(intensity, 4),
        "<br>Species:", species
      )
    )
  ) +
  geom_line()

ggplotly(LongPlot, tooltip = "text")

LongPlot

LongPlot <- pipe1Long |>
  filter(instrument == "DAD") |>
  mutate(sample_id = paste(enzyme, sample, time_point, species, sep = "_")) |>
  ggplot(
    aes(
      x = time,
      y = intensity,
      color = species,
      group = sample_id,
      text = paste(
        "Sample:", sample_id,
        "<br>Time:", round(time, 3),
        "<br>Intensity:", signif(intensity, 4),
        "<br>Species:", species
      )
    )
  ) +
  geom_line()

