library(tidyverse)

df <- readRDS("data/rsofun_driver_data.rds")

# screen for missing gpp data
screening <- df |>
  group_by(sitename) |>
  unnest(forcing) |>
  summarize(
    keep = ifelse(all(is.na(gpp)), FALSE, TRUE)
  )

df <- left_join(df, screening)
df <- df |>
  filter(
    keep
  )

# screen manually, drop funky sites

screen <- readODS::read_ods("data/rsofun_sites.ods") |>
  filter(
    drop
  )

df <- df |>
  filter(
    !(sitename %in% screen$sitename)
  )

# screen manually, drop funky years
screen <- readODS::read_ods("data/rsofun_sites.ods") |>
  filter(
    is.na(drop)
  )

data_fix_years <- df |>
  filter(sitename %in% screen$sitename) |>
  group_by(sitename) |>
  unnest(forcing) |>
  mutate(
    year = as.numeric(format(date, "%Y"))
  ) |>
  filter(
    (year >= screen$start[which(tolower(screen$sitename) %in% tolower(sitename))] &
       year <= screen$end[which(tolower(screen$sitename) %in% tolower(sitename))])
  ) |>
  select(
    -params_siml,
    -site_info,
    -keep,
    -year
  ) |>
  nest() |>
  rename(
    forcing = data
  )

df1 <- df |>
  filter(sitename %in% screen$sitename) |>
  select(
    -forcing
  ) |>
  left_join(data_fix_years)

df2 <- df |>
  filter(!(sitename %in% screen$sitename))

df <- bind_rows(df1, df2) |>
  select(
    -keep
  ) |>
  filter(
    sitename == "AR-SLu"
  )

df |>
  group_by(sitename) |>
  do({

    tmp <- .$forcing[[1]] |>
      tidyr::pivot_longer(
        col = !contains("date"),
        names_to = "measurement",
        values_to = "value"
      )

    sitename <- .$sitename[1]

    l <- seq(as.Date("1990/1/1"), as.Date("2023/1/1"), "years")

    p <- ggplot(data = tmp) +
      geom_line(
        aes(
          date,
          value
        ),
        colour = "red"
      ) +
      geom_vline(xintercept = l) +
      labs(
        title = sitename
      ) +
      theme_bw() +
      theme(panel.grid.minor = element_line()
      ) +
      facet_grid(
        measurement ~ .,
        scales = "free"
        )

    ggsave(
      paste0("manuscript/",sitename,".png"),
      width = 12,
      height = 14,
      dpi = 175
    )

  })
