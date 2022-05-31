library(tidyverse)
library(glue)
library(showtext)
library(ggtext)
library(geomtextpath)

# Load data

data <- tidytuesdayR::tt_load("2022-04-12")
indoor <- data$indoor_pollution
death_ts <- data$death_timeseries
death_fuel <- data$death_fuel
death_source <- data$death_source
fuel_access <- data$fuel_access
fuel_gdp <- data$fuel_gdp
rm(data)

names(death_source)[4] <- "rate"

names(fuel_gdp)[c(4,6)] <- c("perc", "pop")

change <- death_source %>%
  filter(Year %in% c(1990, 2019) & str_detect(Entity, "World Bank region")) %>%
  mutate(lab = str_remove(Entity, " - World Bank region"),
         lab = factor(lab, levels = c("Sub-Saharan Africa",
                                      "South Asia",
                                      "East Asia & Pacific",
                                      "Latin America & Caribbean",
                                      "Europe & Central Asia"))) 

# Palette
colors <- c(
  "#E5121A", # red
  "#6AA0BB",
  "#0E6AA0",
  "#758C9A",
  "#07A2C2"
)

font_add_google("Mukta", "c")
showtext_opts(dpi = 200)
showtext_auto()
change
change %>%
  ggplot(aes(Year, rate, group = lab, label = lab, color = lab)) +
  # geom_segment(data = change %>%
  #                filter(Year == 1990),
  #              aes(y = min(rate), yend = max(rate)),
  #              x = 1990, xend = 1990, color = "grey90") +
  # geom_segment(data = change %>%
  #                filter(Year == 2019),
  #              aes(y = min(rate), yend = max(rate)),
  #              x = 2019, xend = 2019, color = "grey90") +
  geom_textline(straight = TRUE, linewidth = 1, text_smoothing = 100,
                family = "c") #+
  # geom_point(size = 3) +
  geom_text(aes(label = round(rate), family = "c",
                x = ifelse(Year == 2019, Year + .5, Year - .5),
                y = ifelse(lab == "South Asia", rate + 3, rate),
                hjust = ifelse(Year == 2019, 0, 1))) #+
  annotate(geom = "text", x = c(1990, 2019), y = c(-7, -7),
           label = c("1990", "2019"),
           family = "c") +
  annotate(geom = "text", x = c(1988, 2021), y = c(-30, -30),
           hjust = c(0, 1), color = "grey70",
           label = c("Source: Our World in Data",
                     "Spencer Schien (@MrPecners)"),
           family = "c") +
  scale_color_manual(values = colors) +
  coord_cartesian(clip = "off", expand = 0) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = colors[1], size = 24, face = "bold",
                                  margin = margin(b = 25), family = "c"),
        plot.subtitle = element_textbox(size = 14, face = "bold", family = "c",
                                        margin = margin(b = 20)),
        plot.margin = margin(t = 10, r = 10)) +
  labs(y = "", x = "",
       title = "South Asia has shown the largest decrease\nin deaths from household air pollution",
       subtitle = "Regional death rate, deaths per 100k")

  ### dumbell
  ### 
  ### 
  librarian::shelf(
  tidyverse
  , tidytuesdayR
  , PeruData
  )

tt <- tidytuesdayR::tt_load("2022-03-08")
erasmus <-
  tt$erasmus |>
  janitor::clean_names()

cc <- read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv") |>
  janitor::clean_names() |>
  select(1, 2) |>
  rename(code = 2)

country <-
  erasmus |>
  filter(
    academic_year %in% c("2018-2019", "2019-2020")
  ) |>
  select(
    participant_nationality
    , participant_gender
    , participant_age
    , receiving_country_code
    , receiving_city
    , activity_mob
  ) |>  #uk
  mutate(
    across(
      c(receiving_country_code, participant_nationality)
      , ~ifelse(. == "UK", "GB", .)
    )
  ) |>
  count(
    receiving_country_code, participant_gender
  ) |>
  filter(participant_gender != "Undefined") |>
  with_groups(1, ~mutate(., diff = max(n) - min(n), total = sum(n)), BSM) |>
  mutate(diff_ap = BBmisc::normalize(diff, method = "range", range = c(.1, 1)))

country

erasmus_20 <-
  right_join(cc, country, by = c("code" = "receiving_country_code")) |>
  drop_na(country) |>
  mutate(
    country = ifelse(str_detect(country, "Macedonia"), "Macedonia", country)
    , country = forcats::fct_reorder(country, total))
erasmus_20

r_l <-
  erasmus_20 |>
  group_by(country) |>
  arrange(desc(n)) |>
  top_n(1, n)
l_l <-
  erasmus_20 |>
  group_by(country) |>
  arrange(desc(n)) |>
  slice(2)
erasmus_20 |>
  ggplot() +
  aes(country, n) +
  geom_line(aes(group = country, alpha = diff_ap), size = 1) +
  geom_point(aes(color = participant_gender), size = 1.5) +
  # geom_text(
  #   data = r_l
  #   , aes(color = participant_gender, label = n)
  #   , size = 3, hjust = -.5
  # ) +
  # geom_text(
  #   data = l_l
  #   , aes(color = participant_gender, label = n)
  #   , size = 3, hjust = 1.5
  # ) +
  ylim(0, 3400) +
  scale_color_manual(values = c("#c87bd1", "#2f4c7f")) +
  coord_flip() +
  labs(
    x = ""
    , y = ""
    , title = "Participation in THE ERASMUS"
    , subtitle = "2018 - 2020"
    , caption = "#TidyTuesday-22/10 \n VIZ: @jhonkevinflore1"
    , alpha = "Diference (%)"
    , color = "Gender"
  ) +
  # theme_roboto()
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "steelblue2", linetype = "dashed"),
    
    plot.title = element_text(size = 20, hjust = .5, color = "steelblue4", face = "bold"),
    plot.subtitle = element_text(size = 14, color = "firebrick4", hjust = 0.5),
    plot.caption = element_text(color = "#2f4c7f", hjust=.5),
    
    axis.line.x =  element_line(color = "steelblue2", size = 0.5),
    axis.line.y =  element_line(color = "steelblue2", size = 0.5),
    axis.text = element_text(size = 10, color = "steelblue4", face = "bold"),
    axis.title = element_text(color = "steelblue4", face = "bold"),
    
    # legend.title = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(.54, .25),
    legend.background = element_blank(),
    legend.direction="horizontal"
  )


#### animate
#### 
#### library(tidyverse)
devtools::install_github("BillPetti/baseballr")
library(baseballr)
devtools::install_github("thomasp85/gganimate")
library(gganimate)
library(lubridate)
library(fuzzyjoin) # for position graph

savant_daily <- function(data){
  output <- data %>% 
    filter(!is.na(events), events != "caught_stealing_2b") %>% 
    mutate(
      is_ab = if_else(
        events %in% c("strikeout", "field_out", "single", "double", "force_out", 
                      "triple", "home_run", "double_play", "field_error", "grounded_into_double_play",
                      "strikeout_double_play", "fielders_choice_out"),
        TRUE,
        FALSE
      ),
      is_hit = if_else(
        events %in% c("single", "double", "triple", "home_run"), TRUE, FALSE
      ),
      bases = case_when(
        events == "single" ~ 1,
        events == "double" ~ 2,
        events == "triple" ~ 3,
        events == "home_run" ~ 4,
        TRUE ~ 0
      ),
      event_custom = case_when(
        events %in% c("single", "double", "triple", "home_run") ~ events, 
        str_detect(events, "sac") ~ "sacrifice",
        events %in% c("walk", "hit_by_pitch") ~ NA_character_,
        events == "field_error" ~ "error",
        TRUE ~ "out"
      )
    ) %>% 
    group_by(game_date) %>% 
    summarize(
      pa = length(unique(at_bat_number)),
      ab = sum(is_ab),
      hits = sum(is_hit),
      doubles = sum(events == "double"),
      triples = sum(events == "triples"),
      home_runs = sum(events == "home_run"),
      bb = sum(events == "walk"),
      hbp = sum(events == "hit_by_pitch"),
      so = sum(events %in% c("strikeout", "strikeout_double_play")),
      bases = sum(bases)
    ) %>% 
    arrange(game_date) %>% 
    mutate(
      ba = round(hits/ab, 3),
      obp = round((hits + bb + hbp)/(ab + bb + hbp), 3),
      slg = round(bases/ab, 3),
      ops = obp + slg,
      hits_to_date = cumsum(hits),
      bb_to_date = cumsum(bb),
      hbp_to_date = cumsum(hbp),
      ab_to_date = cumsum(ab),
      bases_to_date = cumsum(bases),
      ba_to_date = round(hits_to_date/ab_to_date, 3),
      obp_to_date = round(
        (hits_to_date + bb_to_date + hbp_to_date)/(ab_to_date + bb_to_date + hbp_to_date), 3
      ),
      slg_to_date = round(bases_to_date/ab_to_date, 3),
      ops_to_date = obp_to_date + slg_to_date
    )
  
  return(output)
}

joey_bsvnt <- scrape_statcast_savant_batter(start_date = "2018-03-29", end_date = Sys.Date(), batterid = "621563")
joey_bsvnt_daily <- savant_daily(joey_bsvnt)


joey_rbi <- 
  joey_bsvnt_daily %>% 
  select(game_date, ba_to_date) |> 
  ggplot(aes(game_date, ba_to_date)) + 
  geom_step(size = 1.5) + 
  theme_bw() + 
  labs(
    y = "Batting Average",
    title = "Joey Wendle's batting average during the 2018 season",
    caption = "Data source: baseballsavant.com\n"
  ) + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d, %Y") + 
  scale_y_continuous(breaks = seq(0, 0.35, 0.05)) + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold")
  ) +
  transition_reveal(id = game_date, along = game_date) 

animate(joey_rbi, width = 800, height = 600) 

tc1

### Matching
### 

librarian::shelf(RecordLinkage, tidyverse)



levenshteinSim("12", "12")

ive <- 
  tibble::tribble(
                     ~Base.Form, ~`Past.Simple.(V2)`, ~`Past.Participle.(V3)`,
                        "arise",             "arose",                "arisen",
                        "awake",             "awoke",                "awoken",
                           "be",          "was/were",                  "been",
                         "bear",              "bore",               "born(e)",
                         "beat",              "beat",                "beaten",
                       "become",            "became",                "become",
                        "begin",             "began",                 "begun",
                         "bend",              "bent",                  "bent",
                          "bet",               "bet",                   "bet",
                         "bind",             "bound",                 "bound",
                         "bite",               "bit",                "bitten",
                        "bleed",              "bled",                  "bled",
                         "blow",              "blew",                 "blown",
                        "break",             "broke",                "broken",
                        "breed",              "bred",                  "bred",
                        "bring",           "brought",               "brought",
                    "broadcast",         "broadcast",             "broadcast",
                        "build",             "built",                 "built",
                         "burn",      "burnt/burned",          "burnt/burned",
                        "burst",             "burst",                 "burst",
                          "buy",            "bought",                "bought",
                          "can",             "could",         "… (been able)",
                        "catch",            "caught",                "caught",
                       "choose",             "chose",                "chosen",
                        "cling",             "clung",                 "clung",
                         "come",              "came",                  "come",
                         "cost",              "cost",                  "cost",
                        "creep",             "crept",                 "crept",
                          "cut",               "cut",                   "cut",
                         "deal",             "dealt",                 "dealt",
                          "dig",               "dug",                   "dug",
                           "do",               "did",                  "done",
                         "draw",              "drew",                 "drawn",
                        "dream",    "dreamt/dreamed",        "dreamt/dreamed",
                        "drink",             "drank",                 "drunk",
                        "drive",             "drove",                "driven",
                          "eat",               "ate",                 "eaten",
                         "fall",              "fell",                "fallen",
                         "feed",               "fed",                   "fed",
                         "feel",              "felt",                  "felt",
                        "fight",            "fought",                "fought",
                         "find",             "found",                 "found",
                          "fly",              "flew",                 "flown",
                       "forbid",           "forbade",             "forbidden",
                       "forget",            "forgot",             "forgotten",
                      "forgive",           "forgave",              "forgiven",
                       "freeze",             "froze",                "frozen",
                          "get",               "got",                   "got",
                         "give",              "gave",                 "given",
                           "go",              "went",                  "gone",
                        "grind",            "ground",                "ground",
                         "grow",              "grew",                 "grown",
                         "hang",              "hung",                  "hung",
                         "have",               "had",                   "had",
                         "hear",             "heard",                 "heard",
                         "hide",               "hid",                "hidden",
                          "hit",               "hit",                   "hit",
                         "hold",              "held",                  "held",
                         "hurt",              "hurt",                  "hurt",
                         "keep",              "kept",                  "kept",
                        "kneel",             "knelt",                 "knelt",
                         "know",              "knew",                 "known",
                          "lay",              "laid",                  "laid",
                         "lead",               "led",                   "led",
                         "lean",      "leant/leaned",          "leant/leaned",
                        "learn",    "learnt/learned",        "learnt/learned",
                        "leave",              "left",                  "left",
                         "lend",              "lent",                  "lent",
                 "lie (in bed)",               "lay",                  "lain",
  "lie (to not tell the truth)",              "lied",                  "lied",
                        "light",       "lit/lighted",           "lit/lighted",
                         "lose",              "lost",                  "lost",
                         "make",              "made",                  "made",
                          "may",             "might",                     "…",
                         "mean",             "meant",                 "meant",
                         "meet",               "met",                   "met",
                          "mow",             "mowed",            "mown/mowed",
                         "must",            "had to",                     "…",
                     "overtake",          "overtook",             "overtaken",
                          "pay",              "paid",                  "paid",
                          "put",               "put",                   "put",
                         "read",              "read",                  "read",
                         "ride",              "rode",                "ridden",
                         "ring",              "rang",                  "rung",
                         "rise",              "rose",                 "risen",
                          "run",               "ran",                   "run",
                          "saw",             "sawed",            "sawn/sawed",
                          "say",              "said",                  "said",
                          "see",               "saw",                  "seen",
                         "sell",              "sold",                  "sold",
                         "send",              "sent",                  "sent",
                          "set",               "set",                   "set",
                          "sew",             "sewed",            "sewn/sewed",
                        "shake",             "shook",                "shaken",
                        "shall",            "should",                     "…",
                         "shed",              "shed",                  "shed",
                        "shine",             "shone",                 "shone",
                        "shoot",              "shot",                  "shot",
                         "show",            "showed",                 "shown",
                       "shrink",            "shrank",                "shrunk",
                         "shut",              "shut",                  "shut",
                         "sing",              "sang",                  "sung",
                         "sink",              "sank",                  "sunk",
                          "sit",               "sat",                   "sat",
                        "sleep",             "slept",                 "slept",
                        "slide",              "slid",                  "slid",
                        "smell",             "smelt",                 "smelt",
                          "sow",             "sowed",            "sown/sowed",
                        "speak",             "spoke",                "spoken",
                        "spell",     "spelt/spelled",         "spelt/spelled",
                        "spend",             "spent",                 "spent",
                        "spill",     "spilt/spilled",         "spilt/spilled",
                         "spit",              "spat",                  "spat",
                       "spread",            "spread",                "spread",
                        "stand",             "stood",                 "stood",
                        "steal",             "stole",                "stolen",
                        "stick",             "stuck",                 "stuck",
                        "sting",             "stung",                 "stung",
                        "stink",             "stank",                 "stunk",
                       "strike",            "struck",                "struck",
                        "swear",             "swore",                 "sworn",
                        "sweep",             "swept",                 "swept",
                        "swell",           "swelled",       "swollen/swelled",
                         "swim",              "swam",                  "swum",
                        "swing",             "swung",                 "swung",
                         "take",              "took",                 "taken",
                        "teach",            "taught",                "taught",
                         "tear",              "tore",                  "torn",
                         "tell",              "told",                  "told",
                        "think",           "thought",               "thought",
                        "throw",             "threw",                "thrown",
                   "understand",        "understood",            "understood",
                         "wake",              "woke",                 "woken",
                         "wear",              "wore",                  "worn",
                         "weep",              "wept",                  "wept",
                         "will",             "would",                     "…",
                          "win",               "won",                   "won",
                         "wind",             "wound",                 "wound",
                        "write",             "wrote",               "written"
  )

ive |> 
  janitor::clean_names() |> 
  separate_rows(past_simple_v2, past_participle_v3, sep = "/") |> 
  mutate(
    b_ps = levenshteinSim(base_form, past_simple_v2)
    , b_pt = levenshteinSim(base_form, past_participle_v3)
      ) |> 
  arrange(-b_ps, -b_pt) |> 
  view()
