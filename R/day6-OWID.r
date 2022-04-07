
librarian::shelf(
  tidyverse
  , gganimate
  , gapminder
)

ref <- 
  dir(recursive = T, pattern = "csv")

# ref

point_rg <- c(1, 8)

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
  right_join(hm_cr) 
hm_cr_co |> 
  ggplot() +
  aes(corrupcion, hm, size = pop_sz, color = continent) +
  geom_point()
