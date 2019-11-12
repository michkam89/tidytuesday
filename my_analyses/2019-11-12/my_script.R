library(dplyr)
library(ggplot2)
library(ggrepel)
cran <- readr::read_csv("loc_cran_packages.csv")
glimpse(cran)

pkg_lang <- cran %>% 
  group_by(pkg_name, version) %>% 
  summarise(n_lang = n()) %>% 
  arrange(pkg_name, version)

cran <- cran %>% 
  filter(language != "Markdown") %>% 
  mutate(
    language = case_when(
      language %in% c("C", "C/C++ Header", "C#", "C++") ~ "C family",
      language %in% c("Fortran 77", "Fortran 90", "Fortran 95") ~ "Fortran",
      TRUE ~ language
      )
    ) 
top10_lang <- cran %>% 
  group_by(language) %>% 
  summarise(n_pkg = n()) %>% 
  arrange(desc(n_pkg)) %>% 
  slice(1:10)

cran_top <- cran %>% 
  filter(language %in% top10_lang$language, 
         code != 0, 
         comment != 0,
         blank != 0)
comment_code <- ggplot(data = cran_top, 
       aes(x = log(code, base = 2), 
           y = log(comment, base = 2) 
           )) +
  geom_point(aes(color = language),
             alpha = .1) +
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, color = "black")+
  facet_wrap( ~ language)


high_blank <- filter(cran_top, blank > code)
blank_code <- ggplot(data = cran_top, 
                    aes(x = log(code, base = 2), 
                        y = log(blank, base = 2) 
                    )) +
  geom_point(aes(color = language),
             alpha = .1) +
  facet_wrap( ~ language)+
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, color = "black")
  
blank_comment <- ggplot(data = cran_top, 
                     aes(x = log(comment, base = 2), 
                         y = log(blank, base = 2) 
                     )) +
  geom_point(aes(color = language),
             alpha = .5) +
  facet_wrap( ~ language)+
  geom_smooth(method = "lm") + 
  geom_abline(intercept = 0, color = "black")
