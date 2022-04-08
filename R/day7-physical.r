librarian::shelf(
  tidyverse
  , ggtext
  , tidytuesdayR
  , ggside
)

# tt_available()

tt <- tt_load("2022-03-29")

sports <- tt$sports
glimpse(sports)


unique(sports$sports)

sports |> 
  summarise(total = total_)

options(scipen = 999)

sport_diff <- 
  sports |> 
  select(contains(c("rev", "exp"))) |> 
  select(!contains("total")) |> 
  gather() |> 
  separate(key, c("tipo", "sex"), sep = "_") |> 
  drop_na(value) 

sport_w <- 
  sport_diff |> 
  group_by(tipo) |> 
  mutate(id = row_number()) |> 
  pivot_wider(names_from = tipo, values_from = value) |> 
  ungroup() |> 
  select(!id) |> 
  mutate(sex = ifelse(sex == "men", "Hombres", "Mujeres"))
pal_col <- c(
  "#008e5f"
  , "#f1007d"
)
sport_w |> 
  group_by(sex) |> 
  slice(1:2000) |> 
  mutate(across(c(rev, exp), log)) |> 
  ggplot() +
  aes(exp, rev, color = sex) +
  geom_point(alpha = .3) +
  scale_color_manual(
    values = pal_col
  ) +
  scale_x_continuous(breaks = seq(5, 20, by = 5)) +
  scale_y_continuous(breaks = seq(5, 20, by = 5)) +
  geom_xsidedensity(aes(y = stat(density), xfill = sex), alpha = .2) +
  scale_xfill_manual(values = pal_col) +
  geom_ysidedensity(aes(x = stat(density), yfill = sex), alpha = .2) +
  scale_yfill_manual(values = pal_col) +
  ggside(x.pos = "bottom", y.pos = "right") +
  # '<span style="color:#595a52">Índice de Desarrollo Humano</span>
  ggtitle(
    '<span style="color:#db3a04">**Gastos**</span> y <span style="color:#305c99">**Retornos**</span> generados($)'
    , "Equipos deportivos de Estados Unidos (Escala logarítmica)"
  ) +
  labs(
    x = "Gastos"
    , y = "Retornos"
    , caption = "Data: Equily in Athletics - Tidytuesday {2022-03-29} | Viz: @JhonKevinFlore1\n#30DayChartChallenge | Day7: Physical"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(.7, .2)
    , plot.margin = margin(1, 1, .5, 1, "cm")
    , panel.grid.minor = element_blank()
    , panel.grid.major = element_line(color = "white")
    , panel.background = element_rect(fill="grey95", color = "white")
    , plot.background = element_rect(fill="grey85", color = "white")
    , plot.caption = element_text(hjust = .5)
    , plot.title = element_markdown(hjust = .5)
    , axis.title.x = element_text(color = "#db3a04", face = "bold")
    , axis.title.y = element_text(color = "#305c99", face = "bold")
    , legend.background = element_blank()
    , legend.key = element_rect(fill = "white", color = NA)
    , legend.text = element_text(color ="gray50", size = 13)
    , legend.key.width = unit(12, "mm")
    , legend.title = element_blank()
    
  )

ggsave(
  here::here("plots", "day7.png")
  , width = 12
  , height = 8
)
