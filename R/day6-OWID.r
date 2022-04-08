
librarian::shelf(
  tidyverse
  , gganimate
  , gapminder
  , ggtext
  # , showtext
  # , sysfonts
)

# showtext_auto()
# sysfonts::font_families_google()
# sysfonts::font_add_google("Ubuntu", "Ubuntu")


ref <- 
  dir(recursive = T, pattern = "csv")

# ref

point_rg <- c(4, 18)

hm_cr <- 
  read_csv(ref[1]) |> 
  rename(
    pais = 1
    , anio = 3
    , hm = 4
    , corrupcion = 5
    , pop = 6
  ) |> 
  drop_na(hm, corrupcion) |> 
  select(!Continent) |> 
  mutate(
    anio = as.integer(anio)
    , pop_sz =  BBmisc::normalize(pop, "range", point_rg)
    )



hm_cr_co <- 
  gapminder  |> 
  select(pais = country, continent) |> 
  distinct() |> 
  right_join(hm_cr) |> 
  drop_na(continent)

co_pal <- 
  c(
    "#d3491b"
    , "steelblue"
    , "#227519"
    , "#13afef"
    , "#ffb400"
  )

p <- 
  hm_cr_co |> 
  ggplot() +
  aes(corrupcion, hm,  color = continent) +
  geom_jitter(alpha = .7, show.legend = F, aes(size = pop_sz)) +
  scale_size(range = c(1, 19)) +
  ggtitle(
    '<span style="color:#595a52">Índice de Desarrollo Humano</span>
    **vs** <span style="color:#C20008">Índice de Percepción de la Corrupción</span>'
    ) +
  scale_y_continuous(breaks = seq(.3, .9, by = .1)) +
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  geom_text(
    data = filter(hm_cr_co, continent == "Americas")
    , aes(corrupcion, hm, label = pais)
    , show.legend = F
    , nudge_x = 3
    , nudge_y = -.01
  ) +
  geom_text(aes(x = 70, y = .6), label = "África", color = co_pal[1], size = 5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 70, y = .55), label = "América", color = co_pal[2], size =5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 70, y = .5), label = "Asia", color = co_pal[3], size = 5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 70, y = .45), label = "Europa", color = co_pal[4], size = 5, hjust = 0, vjust = 0) +
  geom_text(aes(x = 70, y = .40), label = "Oceania", color = co_pal[5], size = 5, hjust = 0, vjust = 0) +
  labs(
    caption = "Data: OurWorldInData.org | Viz: @JhonKevinFlore1\n#30DayChartChallenge | Day6: Our World in Data"
    , x = "Índice de Percepción de la Corrupción"
    , y = "Índice de Desarollo Humano"
  ) +
  scale_color_manual(
    values = co_pal
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm")
    , axis.line = element_blank()
    , axis.title.y = element_text(hjust = .5)
    , axis.title.x = element_text(hjust = 1)
    # , axis.title = element_text(size = 30)
    , panel.grid.minor = element_blank()
    , plot.title = element_markdown(
      hjust = .5
      , size = 15
      # , face = "bold"
    )
    , plot.caption.position = "panel"
    , panel.grid.major = element_line(
      size = .2
      , color = "grey50"
      , linetype = 2
    )
    , plot.caption = element_text(hjust = 0, size = 8)
  ) 

p1 <- 
  p +
  labs(subtitle = "Año: {frame_time}") +
  transition_time(anio) +
  ease_aes("linear")



ggsave(
  here::here("plots", "day6.png")
  , height = 6
  , width = 8.4
)


w = 8.4 / 6
gganimate::animate(
  p1
  , heigh = 860
  , width = 860*w
  # , fig.width = 600
  # , fig.height = 600*w
  # , units = "cm"
  , duration = 10
  , fps = 60
  # , dpi = 300
)


anim_save(
  here::here("Plots", "day6.gif")
)
