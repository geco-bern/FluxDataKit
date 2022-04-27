library(tidyverse)
library(rnaturalearth)

coast <- rnaturalearth::ne_coastline(returnclass = "sf")
df <- readRDS("data/p_model_drivers/site_based_drivers.rds")

df <- df %>%
  unnest(
    cols = c("sitename","site_info")
  )

p <- ggplot(df) +
  geom_point(
    aes(
      lon,
      lat
    ),
    colour = "red"
  ) +
  geom_sf(
    data = coast
  ) +
  theme_minimal()

print(p)
