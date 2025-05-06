library(readxl)
library(dplyr)
library(ggplot2)
library(aod)
library(stargazer)
library(haven)
library(geofacet)
library(tidyr)
library(scales)
library(ggthemes)
library(stringr)
library(xtable)
library(usdata)


# heat map of env bills ---------------------------------------------------

state_sum_raw = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/EnvLegislation/sum_bills_state.csv")
state_stats = data.frame(state_stats)
state_sum = merge(state_sum_raw, state_stats, by.x = "abbrev", by.y = "abbr")

state_sum$num_bills_normalized = round((state_sum$numbills / state_sum$pop2010) * 100000, 1)

states = map_data("state")
geo_summed = merge(states, state_sum, by.x = "region", by.y = "name_lower")

snames <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
snames <- merge(snames, geo_summed, by="region")

ggplot(geo_summed, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill = num_bills_normalized)) + 
  geom_text(data=snames, aes(long.x, lat.x, label=num_bills_normalized), colour="azure3",
            size = 10) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(fill = "# of Bills / 100,000 Pop.") + 
  theme(text = element_text(size = 15))
  
  
ggsave("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Figures/state_bill_count.jpg")
  
  
  
state_sum_for_table = state_sum %>% select(abbrev, numbills, num_bills_normalized)
state_sum_for_table = state_sum_for_table[order(state_sum_for_table$num_bills_normalized, decreasing = TRUE),] 
print(xtable(state_sum_for_table, digits = 1), include.rownames = FALSE)


filtered_bills = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/EnvLegislation/post_filter_bills.csv")
filtered_bills = merge(filtered_bills, state_stats, by.x = "abbrev", by.y = "abbr")
filtered_bills$num_bills_normalized = round((filtered_bills$numbills / filtered_bills$pop2010) * 100000, 1)

states = map_data("state")

geo_summed_filtered = merge(states, filtered_bills, by.x = "region", by.y = "name_lower")

snamesf <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
snamesf <- merge(snamesf, geo_summed_filtered, by="region")

ggplot(geo_summed_filtered, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill = num_bills_normalized)) +
  geom_text(data=snamesf, aes(long.x, lat.x, label = num_bills_normalized), colour="azure3",
            size = 10) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(fill = "# of Bills / 100,000 Pop.") + 
  theme(text = element_text(size = 15))

ggsave("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Figures/state_bill_count_post_filter.jpg")
# outcome trends ----------------------------------------------------------

outcome = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/EnvLegislation/outcome_finalized.csv")
outcome$year_abbrev = as.numeric(str_sub(outcome$year, -2, -1))

outcome_for_table = outcome %>% select(state, year, perc)
outcome_for_table$percentage = outcome_for_table$perc * 100
outcome_for_table = outcome_for_table %>% select(state, year, percentage)

outcome_for_table_wide = outcome_for_table %>%
  pivot_wider(names_from = year, values_from = percentage)

print(xtable(outcome_for_table_wide, digits = 1), include.rownames = FALSE)


ggplot(outcome, aes(x = year_abbrev, y = perc)) +
  geom_line(color = "darkgray") +
  geom_smooth(method = "loess", level = 0.95) + 
  facet_geo(~ state, grid = "us_state_grid2") +  # includes DC
  #theme_minimal() +
  labs() +
  theme_tufte() +
  xlab("Year") + 
  ylab("% of Bills with Finalized Outcomes") +
 theme(text = element_text(size = 22)) +
  ylim(0, 1)

#width 874, height 1018
w = 874
h = 1018
ggsave("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Figures/outcome_finalized.jpg")

ggsave(filename = "/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Figures/outcome_finalized.jpg",width = w, height = h, unit = "mm", dpi = 300)

disasters = read.csv("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/EnvData/disaster_count_by_year.csv")

ggplot(disasters, aes(x = start_year, y = to_sum)) +
  geom_line(color = "darkgrey") +
  geom_smooth(method = "loess", level = 0.95) + 
  facet_geo(~ abbrev, grid = "us_state_grid2") + 
  #theme_minimal() +
  labs() +
  theme_tufte() +
  xlab("Year") + 
  ylab("%") +
  theme(text = element_text(size = 22)) +
  ylim(0, 1)

ggsave("/Users/eclin/Desktop/MA Thesis/02 MA Thesis Current Project/Figures/disasters_by_state.jpg")





disasters_voting = left_join(outcome, disasters, by=c('state'='abbrev', 'year'='start_year'))

dvl <- disasters_voting %>%
  group_by(year) 


ggplot(dvl, aes(x = year, y = value, color = trend)) +
  geom_line() +
  facet_geo(~ state, grid = "us_state_grid2") +
  scale_color_manual(
    values = c("perc" = "steelblue", "disasters_scaled" = "firebrick"),
    labels = c("Trend 1 (original scale)", "Trend 2 (rescaled 0–1)"),
    name = NULL
  ) +
  scale_y_continuous(
    name = "Trend 1",
    sec.axis = sec_axis(~ ., name = "Trend 2 (normalized)")
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    axis.title.y.right = element_text(color = "firebrick"),
    axis.title.y.left = element_text(color = "steelblue"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Two Trends per State (Dual Axes)",
    x = "Year", y = NULL
  )