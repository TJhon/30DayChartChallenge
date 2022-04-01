librarian::shelf(
  PeruData #remotes::install_github("tjhon/perudata")
  , tidyverse
  , janitor
  , ggstream
  , cowplot
)
Sys.setlocale("LC_TIME", "Spanish")

peru_covid <- 
  PeruData::covid_casos_fallecidos

peru_prueba <- 
  peru_covid |> 
  select(fecha, sexo, pcr:total_casos) |> 
  filter(lubridate::year(fecha) > 2020) |> 
  mutate(anio = lubridate::year(fecha), mes = lubridate::month(fecha)) |> 
  select(!fecha) |>
  pivot_longer(!c(anio, mes, sexo, total_casos), names_to = "prb") |> 
  group_by(anio, mes, sexo, prb) |> 
  mutate(across(c(total_casos, value), sum)) |> 
  distinct() |> 
  mutate(
    fecha = paste(anio, mes, "01", sep = "-")
    , fecha = lubridate::ymd(fecha)
    , prb = case_when(
      prb == "pr" ~ "Prueba rápida"
      , prb == "ag" ~ 'Prueba de antigeno'
      , T ~ "PCR"
    )
    ) |> 
  drop_na() |> 
  ungroup()


pal <- c(
  "#595A52",  
  "#C20008", 
  "#13AFEF"
  # "#FFB400", 
  # "#8E038E", 
)



p <- 
  peru_prueba |>
  ggplot() +
  aes(fecha, value, fill = prb) +
  geom_stream() +
  # geom_stream_label(aes(label = prb)) +
  # geom_line(aes(fecha, value, color = prb, group = prb, fill = prb)) +
  facet_wrap(~sexo, ncol = 1) +
  scale_fill_manual(values = pal) +
  labs(fill = "", title = "COVID Perú", subtitle = "Casos positivos según prueba realizada") +
  guides(
    fill = guide_legend(
      direction = "horizontal"
      , title.position = "top"
      , title.hjust = .5
      , label.hjust = .5
      , label.position = "top"
      , keywidth = 2.3
      , keyheight = 1
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank()
    , axis.title = element_blank()
    , legend.position = c(.5, .6)
    , legend.background = element_rect(fill = "white", color = NA)
    , legend.key.height = unit(.25, "mm")
    , panel.background = element_rect(fill = "white", color = NA)
    , plot.background = element_rect(fill = "white", color = NA)
    , plot.title = element_text(
      size = 27
      , face = "bold"
      , hjust = .5
      , margin = 
    )
    , plot.subtitle = element_text(
      size = 20
      , hjust = .5
    )
    , legend.text = element_text(size = 15)
    , axis.text.x = element_text(size = 15)
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.y = element_blank()
    , panel.grid.major.x = element_line(size = 1, linetype = "dashed", color = "darkblue")
    , panel.grid.minor.x = element_blank()
    , strip.background = element_blank()
    , strip.text = element_blank()
  ) +
  scale_x_date(date_labels = "%b - %Y", date_breaks = "3 months") 


p1 <- 
  ggdraw(p) +
  cowplot::draw_label("Femenino", x = .74, y = .75, size = 22, color = "#d17dd8", fontface = "bold") +
  cowplot::draw_label("Masculino", x = .74, y = .30, size = 22, color = "#0b4dbf",fontface = "bold") +
  draw_line(x = c(.34, .72), y = c(.90, .90), size = 28, color = "white") +
  draw_line(x = c(.34, .72), y = c(.485, .485), size = 38, color = "white") +
  draw_label("#30DayChartChallenge\nDay1: Part-to-whole\nViz: @JhonKevinFlore1\nData: MINSA Perú", x = .02, y = .08, hjust = 0)

# p1

ggsave(
  plot = p1
  , here::here("plots", "day1.png")
  , width = 16, height = 12)

# 
# peru_prueba |> 
#   ggplot() +
#   aes(x = fecha, y = value, group = prb) + 
#   geom_line() +
#   facet_wrap(~sexo)
# 
# last_plot() +
#   transition_reveal(fecha)
# 
# 
# geom_area()
# 
# animate(p)
# ## transition_time()
# ## view_follow()
