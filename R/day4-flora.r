librarian::shelf(
  tidyverse
  , sf
  , PeruData
  , cowplot
)


sf_file <- 
  dir(here::here("data"), full.names = T, recursive = T, pattern = ".shp$")

fona <- 
  sf_file[2] |> 
  read_sf() |> 
  janitor::clean_names() |> 
  drop_na(descrip)

bsq <- "Bosque"

zona <- 
  fona |> 
  filter(
    str_detect(descrip, "bosque")
  ) |>
  mutate(
    tipo = 
      case_when(
        str_detect(descrip, "muy seco") ~ paste(bsq, "muy seco")
        , str_detect(descrip, "seco") ~ paste(bsq, "seco")
        , str_detect(descrip, "pluvial") ~ paste(bsq, "pluvial")
        , str_detect(descrip, "muy humedo") ~ paste(bsq, "muy humedo")
        , str_detect(descrip, "humedo") ~ paste(bsq, "humedo")
        , T ~ paste(bsq, "espino")
      ) |> 
      factor(
        c("Bosque muy humedo", "Bosque humedo", "Bosque pluvial", "Bosque seco", "Bosque muy seco", "Bosque espino")
      )
  ) |> 
  select(!count)  
  
unique(zona$descrip)

zona_per <- 
  zona |> 
  group_by(tipo) |> 
  summarise(km2 = sum(sum_km2)) |> 
  mutate(per = km2/sum(km2) * 100) |> 
  arrange(per)
bosque <- unique(zona_per$tipo)
# seco, pluvia, muy humedo, espino

plot_bsq <- function(bosque, color = "green"){
  zona_plot <- 
    zona |> 
    filter(tipo == bosque) |> 
    ggplot() +
    geom_sf(
      aes(geometry = geometry), 
      fill = color, color = "gray70", size = .1) +
    theme_void() +
    theme(
      axis.text = element_blank()
      , legend.position = "none"
    )
  print(zona_plot)
}

colores <- 
  c(
    "espino" = "#a38165"
    , "seco" = "#bc5c0d"
    , "muy seco" = "#967d4e"
    , "pluvial" = "#0c8fad"
    , "humedo" = "#0a440f"
    , "muy humedo" = "#02820d"
  )

p1 <- plot_bsq(bosque[1], colores[1])
p2 <- plot_bsq(bosque[2], colores[2])
p3 <- plot_bsq(bosque[3], colores[3])
p4 <- plot_bsq(bosque[4], colores[4])
p5 <- plot_bsq(bosque[5], colores[5])
p6 <- plot_bsq(bosque[6], colores[6])

ggplot(zona) +
  geom_sf(
    aes(
      geometry = geometry
      , fill = tipo
      )
  ) +
  theme_void() +
  scale_fill_manual(
    values = colores
  )

all <- 
  ggplot() +
  geom_sf(aes(geometry = geometry), data = filter(zona, tipo == bosque[1]), fill = colores[1], color = NA) +
  geom_sf(aes(geometry = geometry), data = filter(zona, tipo == bosque[2]), fill = colores[2], color = NA) +
  geom_sf(aes(geometry = geometry), data = filter(zona, tipo == bosque[3]), fill = colores[3], color = NA) +
  geom_sf(aes(geometry = geometry), data = filter(zona, tipo == bosque[4]), fill = colores[4], color = NA) +
  geom_sf(aes(geometry = geometry), data = filter(zona, tipo == bosque[5]), fill = colores[5], color = NA) +
  geom_sf(aes(geometry = geometry), data = filter(zona, tipo == bosque[6]), fill = colores[6], color = NA) +
  theme_bw() +
  theme(
    axis.text = element_blank()
    , panel.grid.major = element_blank()
    , panel.background = element_rect(fill = NA)
    , plot.background = element_rect(fill = NA)
  )


cl <- "gray50"

ggdraw(xlim = c(1, 4), ylim = c(0, 3)) +
  draw_plot(p1, height = .5, x = 2.2, y = .6) +
  draw_plot(p2, height = .5, x = 1.6, y = .6) +
  draw_plot(p3, width = 1.7, x = .6, y = .3) +
  
  draw_plot(p4, width = 1.7, x = 1.8, y = 1.5) +
  draw_plot(p5, width = 1.7, x = 1.2, y = 1.5) +
  draw_plot(p6, width = 1.7, x = .6, y = 1.5) +
  
  draw_plot(all, height = 3, x = 2.95, y = 0) +
  
  draw_label(
    "Muy Húmedo", x = 1.40, y = 2.5
    , size = 40
    , hjust = 0.5
    , color = cl
    ) +
  draw_label(
    "Húmedo", x = 2.05, y = 2.5
    , size = 40
    , hjust = 0.5
    , color = cl
  ) +
  draw_label(
    "Pluvial", x = 2.65, y = 2.5
    , size = 40
    , hjust = 0.5
    , color = cl
  ) +
  
  
  draw_label(
    "Seco", x = 1.45, y = .2
    , size = 40
    , hjust = 0.5
    , color = cl
  ) +
  draw_label(
    "Muy seco", x = 2.1, y = .3
    , size = 40
    , hjust = 0.5
    , color = cl
  ) +
  draw_label(
    "Espino", x = 2.68, y = .3
    , size = 40
    , hjust = 0.5
    , color = cl
  ) +
  
  draw_label(
    "Bosques del Perú"
    , size = 60
    , color = "gray30"
    , x = 2.5
    , y = 2.85
    , fontface = "bold"
  ) +
  
  draw_label(
    "Data: MINAM - Peru\nViz: @JhonKevinFlore1\n#30DayChartChallenge\nDay4: Flora"
    , size = 25
    , hjust = 1
    , color = "black"
    , x = 3.9, y = .15
  ) +
  
  theme(
    panel.grid.major.x = element_line(color = "black", size = 20)
    , panel.background = element_rect(
      fill = "grey90"
    )
    # , panel.grid.major = element_line(color = "black")
  )


ggsave(
  here::here(
    "plots", "day4.png"
  )
  , width = 70, height = 50, units = "cm"
)

