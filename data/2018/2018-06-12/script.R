# Load libraries ---------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
# Import data ------------------------------------------------------------------
week11 <- readr::read_csv("week11_fifa_audience.csv")

# Analyze data structure
glimpse(week11)
lapply(week11, function(x) any(is.na(x)))

# some plots
fifa2010 <- week11 %>% 
  select(-X1) %>% 
  mutate(share_difference = tv_audience_share - population_share) %>% 
  filter(tv_audience_share > 0, 
         population_share > 0, 
         confederation != "OFC")

top_3 <- fifa2010 %>%
  filter(share_difference != 0) %>% 
  group_by(confederation) %>% 
  arrange(desc(share_difference)) %>% 
  slice(c(1:3)) %>% 
  mutate(share_class = "top") 

bottom_3 <- fifa2010 %>%
  filter(share_difference != 0) %>%
  group_by(confederation) %>% 
  arrange(desc(share_difference)) %>% 
  slice((n()-2):n()) %>% 
  mutate(share_class = "bottom") 

top_bottom3 <- bind_rows(top_3, bottom_3) %>% 
  mutate(country_diff = paste(country, share_difference))

ggplot(fifa2010, aes(x = population_share, y = tv_audience_share)) + 
  geom_point(aes(size = gdp_weighted_share, fill = confederation),
             shape = 21,
             color = "black",
             alpha = .7) + 
  geom_label_repel(data = top_bottom3, 
                   aes(label = country_diff
                       ),
                   size = 2.5,
                   min.segment.length = 0,
                   force = 10,
                   alpha = .8,
                   color = ifelse(top_bottom3$share_class == "top", "darkgreen", "darkred"),
                   segment.color = "black",
                   show.legend = FALSE) +
  facet_wrap(~ confederation, scales = "free")


