librarian::shelf(
  tidyverse
  , rvest
  , ggtext
)


oecd <- #
  read_csv(here::here("data", "oecd", "healt_pubord.csv")) |> 
  janitor::clean_names() |> 
  select(!c(flag_codes, measure, indicator, frequency))



# cc <- read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv") |>
#   janitor::clean_names() |>
#   select(1, 2) |>
#   rename(code = 2)

cc1 <- "https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm"

cc <- 
  rvest::read_html(cc1) |> 
  rvest::html_table() %>%
  .[[1]] |> 
  rename(
    country = 1, code = 2, code_c = 3
  ) |> 
  slice(3:n())


oecd_all <-
  oecd |> 
  mutate(subject = PeruData::tidy_text(subject)) |> 
  left_join(cc, by = c("location" = 'code')) |> 
  # filter(is.na(country)) |> 
  # pull(location) |> 
  # unique()
  
  pivot_wider(names_from = subject, values_from = value) |>
  filter(time == 2019) |> 
  mutate(
    country = case_when(
      location == "OEU" ~ "OECD - Europe"
      , location == "OECD" ~ "OECD - Total"
      , location == "OAVG" ~ "OECD - Average"
      , location == "ROU" ~ "Romania"
      , T ~ country
    )
    , country1 = ifelse(str_detect(country, "OE"), paste0('<span style="color:#51719c">**', country, "**</span>"), country)
    , total = health + pubord
    # , diff = 
  ) |> 
  dplyr::select(country, health:total) |> 
  pivot_longer(c(health:pubord)) |>
  mutate(
    name = ifelse(name == "health", "Health", "Public order\nand safety")
    , country = fct_reorder(country, total)
    , country1 = fct_reorder(country1, total)
    , value = value / 100
    )

    


oecd_mark <- 
  filter(oecd_all, str_detect(country, "OE")) 
  
oecd_cl <- "#0e3a72"
s_l <- 1.1
oecd_all |> 
  ggplot() +
  geom_line(aes(group = country1), color = "grey", size = s_l, alpha = .6) +
  geom_line(data = oecd_mark, aes(group = country1), color = oecd_cl, size = s_l, alpha = .7) +
  geom_segment(
    size = .7, alpha = .2, linetype = "dashed"
    ,data = oecd_mark, aes(x = 0, xend = value, y = country1, yend = country1), color = oecd_cl) +
  # geom_point(data = oecd_mark, color = "yellow") +
  geom_point(size = 2.4) +
  aes(value, country1, shape = name, color = name) +
  scale_shape_manual(values = c(18, 17)) +
  scale_color_manual(values = c("#ff7d01", "#51719c")) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, .3, by = .06), limits = c(0, .31)) +
  theme_bw() +
  labs(
    x = "", y = ""
    , color = ""
    , shape = ""
    , title = '<span style="color:#6c97b1">**OECD 2019**</span>: Central government spending'
    , caption = "#30DayChartChallenge | Data: OECD\nDay18: OECD | Viz: @JhonKevinFlore1"
    ) +
    # title = 'Perú - <span style="color:#0e23af">**INCORE**</span>'
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  theme(
    legend.position = c(.7, .5)
    , panel.grid = element_blank()
    , panel.grid.major.x = element_line(linetype = 2, color = "grey70", size = .2)
    , legend.background = element_blank()
    , legend.text = element_text(size = 9)
    , plot.title = element_markdown(hjust = -.4)
    , plot.caption = element_text(hjust = .5, color = "gray35")
    , axis.text.y = element_markdown()
    , plot.background = element_rect("#f7f6ef")
    # , legend.key.width = unit(22, "mm")
    # , 
    # , legend.
    # , legend.key.size = unit(42, "mm")
  ) 

  # geom_point(data = oecd_mark, color = "grey", size = 1) 
  # scale_color_manual(values = c("green", "yellow")) 
  

ggsave(
  here::here("plots", "day18.png")
)  
