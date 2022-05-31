librarian::shelf(
  tidyverse
  , PeruData
  , lubridate
  , gganimate
  , gifski
)
tc_i <- "data/bcrp/tc1.xlsx"
bcrp_get("tc", "1/1 /2021", "5/5/2022", tc_i)
tc <- readxl::read_xlsx(here::here(tc_i), skip = 1) |> 
  rename(
    f = 1
    , tc = 2
  )
Sys.setlocale("LC_ALL", "Spanish")
tc1 <-
  tc |> 
  mutate(
    f = str_replace_all(f, "Set", "Sep")
    , f1 = str_to_lower(f)
    , f2 = paste(str_sub(f1, 1, 5), ".", str_sub(f1, 6, -1), sep = "")
    , f = dmy(f2) 
    , anio = year(f)
    , tc = as.numeric(tc)
  ) |> 
  filter(
    f > dmy("31/12/2020")
    , month(f) < month(5)
  ) |>
  arrange(desc(f)) |> 
  drop_na() |> 
  select(!f1:f2) |> 
  # group_by(anio) |> 
  # mutate(f = row_number()) |> 
  mutate(f = ifelse(year(f) == 2021, f + years(1), f)) |>
  pivot_wider(names_from = anio, values_from = tc) |> 
  mutate(f = lubridate::as_date(f)) |> 
  drop_na() |> 
  pivot_longer(!f)
  
  
pa <- 
  tc1 |> 
  # filter(name == "2021") |>
  arrange(f) |> 
  ggplot() +
  aes(f, value, color = name) +
  geom_point(size = 4.3) +
  # geom_line() +
  geom_step(size = 2, direction = "mid") +
  scale_color_manual(values = c("#db6b20", "#265e8e"))+
  cowplot::theme_minimal_hgrid() +
  labs(
    color = ""
    , x = ""
    , y = ""
    , title = "Perú: 2021 - 2022"
    , subtitle = "Movimiento del Tipo de Cambio Interbancario"
    , caption = "#30DayChartChallenge | Day 22: Animation\nData: BCRP |  @JhonKevinFlore1"
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(
        size = 1
      )
    )
  ) +
  # transition_reveal(id = f, along = f) +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%B %d") +
  theme(
    legend.position = c(.45, .8)
    , panel.grid.major = element_line(linetype = 4, color = "gray30")
    , panel.grid.major.x = element_line(linetype = 4, color = "gray60")
    , plot.caption = element_text(hjust = .5, color = "grey50", size = 13)
    , plot.title = element_text(size = 20)
    , plot.subtitle = element_text(size = 17)
    , panel.background = element_rect("#f9f3f2")
    , plot.background = element_rect("#f9f3f2")
    , legend.background = element_blank()
    , legend.text = element_text(size = 16)
    , legend.key.width = unit(22, "mm")
  )  +
  transition_reveal(f)

animate(pa, width = 1200, height = 800, renderer = gifski_renderer(here::here("plots", "day221.gif"))) 

ggsave(
  here::here("plots", "day22-1.png")
  , plot = pa
  , width = 12
  , height = 8
  , units = "in"
)


  
