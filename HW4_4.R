library(dplyr)
library(ggplot2)
library(lubridate)

load("/Users/kristenpechin/Library/CloudStorage/GoogleDrive-carterkj@umich.edu/.shortcut-targets-by-id/1ehWwunuAo7CE1Vk2JYkUnQMmxh5pph3C/DATA/preprint_growth.rda")

preprint_growth %>% 
  filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth

preprints <- preprint_growth %>%
  filter(archive %in% c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%
  filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

preprints <- na.omit(preprints)

y_max <- max(preprints$count, na.rm = TRUE)
x_min <- ymd("2013-01-01")
x_max <- ymd("2017-01-01")

preprints_final <- filter(preprints, date == ymd("2017-01-01"))

ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, y_max), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis(
      breaks = preprints_final$count,
      labels = levels(preprints$archive),
      name = NULL
    )
  ) +
  scale_x_date(
    name = "year",
    limits = c(x_min, x_max)
  ) +
  scale_color_manual(
    values = c("yellow", "pink", "blue"),
    name = NULL
  ) +
  theme(legend.position = "none")

#Changed the colors to yellow, pink, blue
#Changed the start date to 2013