librarian::shelf(
  tidyverse
  , PeruData
  , gganimate
  , lubridate
)


PeruData::get_last_igp("data", "igp-2022.csv") 

igp_last <- 
  read_csv(here::here("data", "igp_2022.csv")) |> 
  clean_igp()

eq <- 
  PeruData::igp |> 
  bind_rows(igp_last) |> 
  distinct() |> 
  select(!c(lat, lon, hour, intensi_km)) |> 
  mutate(
    anio = year(date)
    , mes = month(date)
  ) |> 
  group_by(anio, mes, alert) |> 
  summarise(promedio = mean(magn)) |> 
  ungroup() |> 
  mutate(date = ymd(paste(anio, mes, "01", sep = "/"))) |> 
  select(!1:2) |> 
  drop_na() |> 
  mutate(
    alert = factor(alert, c("Red", "Yellow", "Green"))
    , alerta_name = case_when(
      alert == "Red" ~ 'Alerta "Roja"'
      , alert == "Yellow" ~ 'Alerta "Ararilla"'
      , T ~ 'Alerta "Verde"'
    ) |> 
      factor(c('Alerta "Roja"', 'Alerta "Ararilla"', 'Alerta "Verde"'))
  ) |> 
  filter(year(date) < 2023) |> 
  with_groups(
    alert, mutate, mid = mean(promedio, na.rm = T)
  )

pal <- c(
  "#C20008" 
  , "#FFB400"
  # , "#595A52"  
  # , "#13AFEF"
  , "#006320"
)

eq |> 
  ggplot() +
  geom_point(size = 2) +
  aes(date, promedio, color = alert) +
  geom_step(direction = "vh") +
  geom_hline(aes(yintercept = mid, color = alert), linetype = "dashed") +
  facet_wrap(~alerta_name, scales = "free_y", ncol = 1) +
  labs(
    x = "", y = ""
    , title = "Terremotos/Sismos en el Perú"
    , subtitle = "Promedio de la magnitud de los terremotos segun su clasificación"
    , caption = "Data: IGP - Peru | Viz: @JhonKevinFlore1 | #30DayChartChallenge | Day3: Historical"
    
    ) +
  scale_color_manual(
    values = pal
  ) + 
  scale_x_date(
    date_labels = "%m - %Y", date_breaks = "2 years"
    , limits = as.Date(c('1958-05-01','2023-01-01'))
    )  +
  
  theme(
    axis.text.x = element_text(angle = 60, vjust = .1, hjust = .5, size = 10)
    , legend.position = "none"
    , panel.grid.minor = element_blank()
    , panel.grid.major.x = element_line(size = .5, color = "grey70", linetype = "dotted")
    , text = element_text(
      family = "Ubuntu",
      face = 'bold',
      colour = "#004D40"
    ),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA), 
    plot.title = element_text(
      hjust = 0.5,
      size = 40,
      margin = margin(10, 0, 0, 0),
      color = "#c00000"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 36,
      margin = margin(10, 0, 0, 0),
      color = "grey40"
    ),
    plot.caption = element_text(
      hjust = .5,
      margin = margin(20, 0, 2, 0),
      color = "grey50"
      , lineheight = .7
      , size = 13
    ),
    plot.margin = margin(0, 1, 1, 0, unit = "cm")
    , strip.background = element_blank()
    , strip.text = element_text(family = "Ubuntu", size = 17, hjust = 0)
    , strip.background.x = element_blank()
    , axis.line.x.bottom = element_line(linetype = "solid", arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                                                ends = "last", type = "closed"))
    , axis.line.y.left = element_line(linetype = "solid")
  )
  
ggsave(
  here::here("plots", "day3.png")
  , width = 20, height = 12
)
