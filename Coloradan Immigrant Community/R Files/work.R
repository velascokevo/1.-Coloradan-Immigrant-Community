############################################################
### Loading libraries
############################################################

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forcats) 
library(treemapify)
library(ggplotify)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)
library(webshot2)

############################################################
### Setting up your working directory
############################################################

setwd("YOUR DIRECTORY GOES HERE")

############################################################
### Loading in data
############################################################

raw_data <- read_csv("Data/raw_data.csv") |> 
  mutate(metfips = as.numeric(metfips)) |> 
  rename(msa = metfips)

############################################################
### Data cleaning
############################################################

data <- raw_data |> 
  mutate(
    year = as.numeric(year),
    status = case_when(
      citizen %in% c("born in u.s", "born abroad of american parents", "born in u.s. outlying") ~ "Native-born",
      citizen == "naturalized citizen" ~ "Naturalized",
      citizen == "not a citizen" ~ "Non-citizen"))

############################################################
### Population proportions: Overall
############################################################

fig1 <- data |> 
  filter(year != 2014) |> # removing 2014 due to statistical discrepancies with survey design
  group_by(year, status) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE)) |> 
  filter(year > 2003 & year < 2025) |> 
  ggplot() + 
  geom_line(mapping = aes(color = status, 
                          x = year, 
                          y = sum),
            size = 0.75) + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, 
                                                   suffix = "m",
                                                   accuracy = 0.1)) +
  geom_point(mapping = aes(color = status, 
                           x = year, 
                           y = sum, 
                           shape = status),
             size = 1.5) + 
  scale_color_manual(values = c(
    "Native-born" = "darkgoldenrod2",
    "Naturalized" = "red2",
    "Non-citizen" = "navyblue")) + 
  scale_x_continuous(breaks = seq(2004, 2024, by = 4)) + 
  labs(title = "1. Colorado's population makeup",
       subtitle = "A ballooning native-born population, and booming foreign-born growth.",
       x = "Year",
       y = "",
       color = "",
       shape = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.backgrond = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig1.png", fig1, width = 10, height = 8, units = "in", dpi = 300)

data |> 
  group_by(year) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE)) |> 
  filter(year > 2003 & year < 2025) |> 
  print(n = 30)

data |> 
  group_by(year, status) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE)) |> 
  filter(year == 2004 | year == 2024)

############################################################
### Population proportions: Foreign-born
############################################################

prop_foreign <- data |> 
  filter(year != 2014) |> # removing 2014 due to statistical discrepancies with survey design
  group_by(year, status) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE)) |> 
  filter(status != "Native-born") |> 
  group_by(year) |> 
  mutate(pct = sum / sum(sum),
         round_pct = paste0(round(pct * 100), "%"),
         vert_pct = sapply(strsplit(round_pct, ""), function(x) paste(x, collapse = "\n")))

fig2 <- prop_foreign |> 
  filter(year > 2003) |> 
  ggplot() + 
  geom_bar(mapping = aes(x = year, 
                         y = sum, 
                         fill = status),
           position = "stack",
           stat = "identity",
           width = 1,
           color = "white") +
  geom_text(mapping = aes(x = year, 
                          y = sum, 
                          fill = status, 
                          label = vert_pct),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            angle = 0,
            size = 4.5,
            lineheight = 0.8) + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, 
                                                   suffix = "k", 
                                                   accuracy = 1)) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 4)) + 
  scale_fill_manual(values = c(
    "Naturalized" = "red2",
    "Non-citizen" = "navyblue")) + 
  labs(title = "2. Proportion of non-citizen to naturalized immigrants",
       subtitle = "Declining proportion of non-citizen immigrants, relative to their naturalized counterparts.",
       fill = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.backgrond = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig2.png", width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Nation of birth
############################################################

# 2004
top_bpl_2004 <- data |>
  filter(year == 2004, status != "Native-born") |>
  group_by(bpl) |>
  summarize(sum = sum(asecwt, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(sum)) |>
  slice_head(n = 10) |>
  pull(bpl)

fig3 <- data |>
  filter(year == 2004, status != "Native-born") |>
  mutate(bpl_lumped = if_else(bpl %in% top_bpl_2004, bpl, "Other")) |>
  group_by(bpl_lumped) |>
  mutate(bpl_lumped = fct_recode(bpl_lumped,
                                 "Mexico" = "mexico",
                                 "Venezuela" = "venezuela",
                                 "Guatemala" = "guatemala",
                                 "Ethiopia" = "ethiopia",
                                 "India" = "india",
                                 "El Salvador" = "el salvador",
                                 "Brazil" = "brazil",
                                 "UK" = "united kingdom, n.s",
                                 "Colombia" = "colombia",
                                 "Canada" = "canada",
                                 "Germany" = "germany",
                                 "Vietnam" = "vietnam",
                                 "South Korea" = "south korea", 
                                 "The Philippines" = "philippines",
                                 "Russia/USSR" = "other ussr/russia")) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE), .groups = "drop") |>
  mutate(share = sum / sum(sum),
         pct_label = paste0("(", round(share * 100), "%", ")"),
         sum_label = paste0(round(sum / 100) * 100 / 1000, "k"),
         label = paste0(bpl_lumped, "\n", sum_label, "\n", pct_label),
         bpl_lumped = fct_reorder(bpl_lumped, sum, .desc = TRUE),
         is_other = bpl_lumped == "Other") |> 
  arrange(is_other, desc(sum)) |> 
  mutate(bpl_lumped = forcats::fct_relevel(bpl_lumped, "Other", after = Inf)) |> 
  ggplot() + 
  geom_treemap(mapping = aes(area = sum, 
                             fill = bpl_lumped),
               color = "white") +
  geom_treemap_text(mapping = aes(area = sum, 
                                  label = label),
                    color = "white",
                    place = "center",
                    reflow = TRUE,
                    size = 16,
                    fontface = "bold") + 
  scale_fill_manual(values = c(
    "Other" = "navyblue",
    "Mexico" = "red2",
    "Venezuela" = "darkgoldenrod2",
    "Guatemala" = "chartreuse4",
    "Ethiopia" = "deepskyblue2",
    "India" = "darkorange2",
    "El Salvador" = "tan2",
    "Brazil" = "orangered3",
    "Canada" = "darkgreen",
    "Colombia" = "gold3",
    "UK" = "darkslateblue",
    "Germany" = "grey35",
    "Vietnam" = "darkseagreen4",
    "South Korea" = "steelblue",
    "The Philippines" = "tomato4",
    "Russia/USSR" = "darkgoldenrod4"
  )) +
  labs(title = "3. Coloradan immigrants by place of birth, 2004",
       subtitle = "Over half of Coloradan immigrants derived from Mexico.") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.backgrond = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        text = element_text(size = 20))

ggsave("Graphs/fig3.png", fig3, width = 10, height = 8, units = "in", dpi = 300)

# 2024
top_bpl_2024 <- data |>
  filter(year == 2024, status != "Native-born") |>
  group_by(bpl) |>
  summarize(sum = sum(asecwt, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(sum)) |>
  slice_head(n = 10) |>
  pull(bpl)

fig4 <- data |>
  filter(year == 2024, status != "Native-born") |>
  mutate(bpl_lumped = if_else(bpl %in% top_bpl_2024, bpl, "Other")) |>
  group_by(bpl_lumped) |>
  mutate(bpl_lumped = fct_recode(bpl_lumped,
                                 "Mexico" = "mexico",
                                 "Venezuela" = "venezuela",
                                 "Guatemala" = "guatemala",
                                 "Ethiopia" = "ethiopia",
                                 "India" = "india",
                                 "El Salvador" = "el salvador",
                                 "Brazil" = "brazil",
                                 "UK" = "united kingdom, n.s",
                                 "Colombia" = "colombia",
                                 "Canada" = "canada")) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE), .groups = "drop") |>
  mutate(share = sum / sum(sum),
         pct_label = paste0("(", round(share * 100), "%", ")"),
         sum_label = paste0(round(sum / 100) * 100 / 1000, "k"),
         label = paste0(bpl_lumped, "\n", sum_label, "\n", pct_label),
         bpl_lumped = fct_reorder(bpl_lumped, sum, .desc = TRUE),
         is_other = bpl_lumped == "Other") |> 
  arrange(is_other, desc(sum)) |> 
  mutate(bpl_lumped = forcats::fct_relevel(bpl_lumped, "Other", after = Inf)) |> 
  ggplot() + 
  geom_treemap(mapping = aes(area = sum, 
                             fill = bpl_lumped),
               color = "white") +
  geom_treemap_text(mapping = aes(area = sum, 
                                  label = label),
                    color = "white",
                    place = "center",
                    reflow = TRUE,
                    size = 16,
                    fontface = "bold") + 
  scale_fill_manual(values = c(
    "Other" = "navyblue",
    "Mexico" = "red2",
    "Venezuela" = "darkgoldenrod2",
    "Guatemala" = "chartreuse4",
    "Ethiopia" = "deepskyblue2",
    "India" = "darkorange2",
    "El Salvador" = "tan2",
    "Brazil" = "orangered3",
    "Canada" = "darkgreen",
    "Colombia" = "gold3",
    "UK" = "darkslateblue"
  )) +
  labs(title = "4. Coloradan immigrants by place of birth, 2024",
       subtitle = "By 2024, there was a greater proportion of non-Mexican immigrants.") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "none",
        text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.backgrond = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig4.png", fig4, width = 10, height = 8, units = "in", dpi = 300)


############################################################
### Arrival horizons 
############################################################

# 2004
fig5 <- data |> 
  filter(year == 2004) |> 
  mutate(
    arrival_horizon = case_when(
      yrimmig %in% c("2002-2004", "2000-2001 (2001 cps: 1998-2001)") ~ "Less than 5 years ago",
      yrimmig %in% c("1998-1999 (1999 cps: 1996-1999)", "1996-1997", "1994-1995") ~ "5-10 years ago",
      yrimmig %in% c("1992-1993", "1990-1991", "1988-1989") ~ "11-16 years ago",
      yrimmig == "niu" ~ "Native-born",
      !yrimmig %in% c ("2002-2004", "2000-2001 (2001 cps: 1998-2001)", "1998-1999 (1999 cps: 1996-1999)", "1996-1997", "1994-1995",
                       "1992-1993", "1990-1991", "1988-1989") ~ "More than 16 years ago")
  ) |> 
  group_by(arrival_horizon) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE)) |> 
  mutate(arrival_horizon = factor(
    arrival_horizon,
    levels = c("Less than 5 years ago",
               "5-10 years ago",
               "11-16 years ago",
               "More than 16 years ago"))) |>
  filter(arrival_horizon != "Native-born") |> 
  arrange(desc(arrival_horizon)) |> 
  mutate(cumulative = cumsum(sum),
         midpoint = cumulative - sum / 2,
         prop = paste0(round(sum / sum(sum) * 100), "%")) |> 
  ggplot() + 
  geom_bar(mapping = aes(x = 1, 
                         y = sum, 
                         fill = arrival_horizon),
           stat = "identity", 
           width = 1,
           color = "white") +
  coord_polar(theta = "y", start = 0) + 
  scale_fill_manual(values = c(
    "Less than 5 years ago" = "chartreuse4",
    "5-10 years ago" = "darkgoldenrod2",
    "11-16 years ago" = "red2",
    "More than 16 years ago" = "navyblue")) + 
  geom_text(mapping = aes(x = 1.25, 
                          y = midpoint, 
                          label = prop), 
            color = "white", 
            size = 3.5, 
            fontface = "bold") + 
  labs(title = "5. Coloradan immigrants by year of arrival, 2004",
       subtitle = "",
       fill = "Year of arrival") + 
  theme_fivethirtyeight() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        text = element_text(size = 16,
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig5.png", fig5, width = 10, height = 8, units = "in", dpi = 300)

# 2024
fig6 <- data |> 
  filter(year == 2024) |> 
  mutate(
    arrival_horizon = case_when(
      yrimmig %in% c("2022-2024", "2020-2022") ~ "Less than 5 years ago",
      yrimmig %in% c("2018-2021 (2022 cps forward: 2018-2019)", "2014-2017 (2019 cps forward: 2016-2017)", "2012-2015 (2018 cps forward: 2014-2015)") ~ "5-10 years ago",
      yrimmig %in% c("2010-2013 (2016 cps forward: 2012-2013)", "2008-2011 (2014 cps forward: 2010-2011)", "2006-2009 (2012 cps forward: 2008-2009)") ~ "11-16 years ago",
      yrimmig == "niu" ~ "Native-born",
      !yrimmig %in% c("2022-2024", "2020-2022", "2018-2021 (2022 cps forward: 2018-2019)", "2014-2017 (2019 cps forward: 2016-2017)",
                      "2012-2015 (2018 cps forward: 2014-2015)", "2010-2013 (2016 cps forward: 2012-2013)", "2008-2011 (2014 cps forward: 2010-2011)",
                      "2006-2009 (2012 cps forward: 2008-2009)") ~ "More than 16 years ago")
  ) |> 
  group_by(arrival_horizon) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE)) |> 
  mutate(arrival_horizon = factor(
    arrival_horizon,
    levels = c("Less than 5 years ago",
          "5-10 years ago",
          "11-16 years ago",
          "More than 16 years ago"))) |> 
  filter(arrival_horizon != "Native-born") |> 
  arrange(desc(arrival_horizon)) |> 
  mutate(cumulative = cumsum(sum),
         midpoint = cumulative - sum / 2,
         prop = paste0(round(sum / sum(sum) * 100), "%")) |> 
  ggplot() + 
  geom_bar(mapping = aes(x = 1, 
                         y = sum, 
                         fill = arrival_horizon),
           stat = "identity", 
           width = 1,
           color = "white") +
  coord_polar(theta = "y", start = 0) + 
  scale_fill_manual(values = c(
    "Less than 5 years ago" = "chartreuse4",
    "5-10 years ago" = "darkgoldenrod2",
    "11-16 years ago" = "red2",
    "More than 16 years ago" = "navyblue")) + 
  geom_text(mapping = aes(x = 1.25, 
                          y = midpoint, 
                          label = prop), 
            color = "white", 
            size = 3.5, 
            fontface = "bold") + 
  labs(title = "6. Coloradan immigrants by year of arrival, 2024",
       subtitle = "A growing proportion of immigrants who arrived over a decade ago.",
       fill = "Year of arrival") + 
  theme_fivethirtyeight() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig6.png", fig6, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Age pyramid
############################################################

options(scipen = 999)  

native_pyramid <- data |> 
  filter(year == 2024,
         status == "Native-born") |> 
  mutate(age = as.numeric(age),
         age = floor(age / 5) * 5) |> 
  group_by(status, age) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE),
            status = "Native-born") 

foreign_pyramid <- data |> 
  filter(year == 2024,
         status != "Native-born") |> 
  mutate(age = as.numeric(age),
         age = floor(age / 5) * 5) |> 
  group_by(age) |> 
  summarize(sum = sum(asecwt, na.rm = TRUE)) |>
  mutate(sum = -sum,
         status = "Foreign-born")

combined_pyramid <- bind_rows(native_pyramid, foreign_pyramid)

pop_range <- range(combined_pyramid$sum)
pop_range_breaks <- pretty(pop_range, n = 8)
abs_pop_range_breaks <- abs(pop_range_breaks)

fig7 <- combined_pyramid |>
  filter(!is.na(age)) |> 
  ggplot() + 
  geom_col(mapping = aes(x = sum, 
                         y = factor(age), 
                         fill = status),
           width = 1,
           color = "white") +
  scale_x_continuous(breaks = pop_range_breaks,
                     labels = function(x) paste0(abs(x / 1000), "k")) + 
  scale_fill_manual(values = c(
    "Native-born" = "red2",
    "Foreign-born" = "navyblue")) + 
  labs(title = "7. Colorado population pyramids by native- and foreign-born residents",
       fill = "",
       subtitle = "Foreign-born residents could assist increasingly aging native-born Coloradans.",
       y = "Age") + 
  theme_fivethirtyeight() + 
  theme(axis.title.y = element_text(size = 14),
        text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.backgrond = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig7.png", fig7, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Occupation
############################################################

sum_industry <- data |> 
  filter(year == 2024,
         ind != 0) |> 
  group_by(ind) |> 
  summarize(sum_industry = sum(asecwt, na.rm = TRUE))

sum_foreign_born <- data |> 
  filter(year == 2024,
         status != "Native-born",
         ind != 0) |> 
  group_by(ind) |> 
  summarize(sum_foreign_born = sum(asecwt, na.rm = TRUE))

combined_industry <- sum_industry |> 
  left_join(y = sum_foreign_born, by = "ind") |> 
  mutate(prop_foreign_born = sum_foreign_born / sum_industry,
         prop_foreign_born = if_else(is.na(prop_foreign_born), 0, prop_foreign_born)) |>
  arrange(desc(sum_foreign_born)) |> 
  slice_head(n = 15)

combined_industry |> 
  mutate(label = percent(prop_foreign_born, accuracy = 1),
         sum_industry = comma(round(sum_industry, -2)),
         sum_foreign_born = comma(round(sum_foreign_born, -2)),
         ind = case_when(
           ind == 770 ~ "Construction",
           ind == 8680 ~ "Food services",
           ind == 7690 ~ "Building maintenance and cleaning",
           ind == 7380 ~ "Computer systems",
           ind == 4971 ~ "Grocery retailers",
           ind == 8191 ~ "General medical and surgical hospitals",
           ind == 6170 ~ "Truck transportation",
           ind == 4470 ~ "Grocery merchant wholesaler",
           ind == 6870 ~ "Banking and related activities",
           ind == 7390 ~ "Management and scientific consulting services",
           ind == 1180 ~ "Animal slaughtering and processing",
           ind == 7770 ~ "Landscaping",
           ind == 6680 ~ "Wired telecommunications",
           ind == 7860 ~ "Elementary and secondary schools",
           ind == 9070 ~ "Drycleaning")) |>
  rename(`Pct. in industry` = prop_foreign_born,
         `Industry` = ind,
         `Total workers` = sum_industry,
         `Foreign-born workers` = sum_foreign_born,
         `%` = label) |> 
  gt() |> 
  tab_header(
    title = md("**8. Occupational industry in Colorado by number of foreign-born workers**"),
    subtitle = md("Construction, food services, and building maintenance and cleaning are the most popular.")) |> 
  cols_move_to_end(columns = c(`Total workers`)) |> 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) |> 
  gt_plt_bar(column = `Pct. in industry`, 
             scaled = TRUE,
             labels = FALSE,
             color = "navyblue") |> 
  text_transform(locations = cells_body(columns = c(`Pct. in industry`)),
                 fn = function(x) x) |>
  opt_align_table_header(align = "left") |> 
  opt_vertical_padding(scale = 0.75) |> 
  opt_table_font(font = list(google_font("Lato"),
                             default_fonts())) |> 
  tab_style(style = cell_text(size = px(26),
                              weight = "bold"),
            locations = cells_title(groups = "title")) |> 
  tab_style(style = cell_text(size = px(20)),
            locations = cells_title(groups = "subtitle")) |> 
  gt::gtsave("Graphs/fig8.png") 


