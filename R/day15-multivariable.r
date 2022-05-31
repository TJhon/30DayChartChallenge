librarian::shelf(
  tidyverse
  , PeruData
  , lubridate
  # , sysfonts
  # , showtext
)

sysfonts::font_add_google("Ubuntu", "regular")
# showtext_auto()


mlt <- c(
  "https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04704XD/html"
  , "https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04703XD/html"
  , "https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04702XD/html"
  , "https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD04701XD/html"
)

Sys.setlocale("LC_ALL", "Spanish")


pi <- 
  bcrp_get(mlt)



last_p <- 
  pi |> 
  reduce(left_join) |> 
  slice(n():(n() - 300)) |> 
  mutate(
    fecha1 = str_to_lower(fecha)
  )

nn <- 
  last_p |> 
  mutate(
    fecha = str_replace_all(fecha, "Set", "Sep")
    , fecha1 = str_to_lower(fecha)
    , fecha2 = paste(str_sub(fecha1, 1, 5), ".", str_sub(fecha1, 6, -1), sep = "")
    , fecha = dmy(fecha2)
  ) |> 
  # filter(is.na(fecha))
  select(1:5) |> 
  rename(
    oro = 2
    , zinc = 3
    , plata = 4
    , cobre = 5
  ) |> 
  pivot_longer(!fecha) |> 
  mutate(
    value = as.numeric(value)
  )

mi_cl <- 
  c(
    "#ffd700"
    , "#bac4c8"
    , "#b87333"
    , "#5b7582"
  )

nn |> 
  mutate(
    mi = factor(str_to_sentence(name), c("Oro", "Plata", "Cobre", "Zinc"))
  ) |> 
  # filter(name == 'oro') |> 
  ggplot() +
  aes(fecha, value, group = mi, color = mi) +
  # ggalt::geom_xspline() +
  geom_line(size = 1, show.legend = F) +
  facet_wrap(~mi, scales = "free_y") +
  scale_x_date(date_breaks = "18 weeks") +
  labs(
    x = "", y = ""
    , title = "Commodities"
    , subtitle = "Cottizaciones internacionales ($)"
    , caption = "#30DayChartChallenge | Data: BCRP \nDay15: Multivariable | Viz: @JhonKevinFlore1"
    ) +
  scale_color_manual(
    values = mi_cl
  ) +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()+
  theme(
    panel.background = element_rect("white", color = NA)
    , plot.background = element_rect("white")
    , panel.grid.minor = element_blank()
    , plot.margin = margin(.3, 1, .1, .3, "cm")
    , axis.line = element_line()
    , axis.text.x = element_text(hjust = .5, angle = 12)
    , plot.caption = element_text(hjust = .5)
  )

ggsave(
  here::here(
    "plots"
    , "day15.png"
  )
  , height = 8
  , width = 12
  , dpi = 320
)
