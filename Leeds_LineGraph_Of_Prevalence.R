library(tidyverse)
library(fingertipsR)
library(ggrepel)
library(plotly)

# Extract data ------------------------------------------------------------

profiles <- profiles()
NCMP_profiles <- profiles %>% filter(DomainID == "8000011")
NCMP_profiles


inds <- indicators(DomainID = NCMP_profiles$DomainID) %>%
     select(IndicatorID) %>% pull() %>% as.character() %>%
     dput()

area_types(AreaTypeID = 101)

NCMP_data <- fingertips_data(IndicatorID = inds, AreaTypeID = 102)



# Prepare data ------------------------------------------------------------
single_indicators <- c(90316, 90320, 90317, 90321,  92464, 92465, 90319, 90323) # May not be ordered properly
single_indicators_names <- c("Reception: Prevalence of underweight", 
                             "Reception: Prevalence of healthy weight", 
                             "Reception: Prevalence of obesity (including severe obesity)", 
                             "Reception: Prevalence of overweight", 
                             "Year 6: Prevalence of underweight", 
                             "Year 6: Prevalence of healthy weight", 
                             "Year 6: Prevalence of obesity (including severe obesity)", 
                             "Year 6: Prevalence of overweight")

NCMP_data_Leeds <- NCMP_data %>% filter(AreaName == "Leeds") %>%
     filter(IndicatorID %in% single_indicators) %>%
     mutate(IndicatorID = factor(IndicatorID, ordered = TRUE, 
                                 levels = single_indicators)) %>%
     mutate(IndicatorName = factor(IndicatorName, ordered = TRUE, 
                                   levels = single_indicators_names)) %>%
     mutate(label = if_else(Timeperiod == max(Timeperiod), as.character(IndicatorName), NA_character_))

# Explore data ------------------------------------------------------------
head(NCMP_data)
tail(NCMP_data)

# GGplot Graph ------------------------------------------------------------
NCMP_data_Leeds %>%
  #group_by(IndicatorName, Timeperiod) %>%
  ggplot(aes(x = Timeperiod, y = Value)) +
  geom_label_repel(aes(label = label), nudge_x = 1, nudge_y = 3,
                   na.rm = TRUE) +
  geom_line(aes(group = IndicatorName, color = IndicatorName, linetype = IndicatorName),
            size = 3, alpha = 0.8) + 
  geom_point(size = 2, alpha = 0.5) +
  scale_color_discrete(name = "Groups by Color",
                       labels = single_indicators_names) +
  scale_linetype_discrete(guide = "none") +
  ggtitle("NCMP data for Leeds")




# Plotly Graph ------------------------------------------------------------
ggplot_leeds_prevalence <- NCMP_data_Leeds %>%
  #group_by(IndicatorName, Timeperiod) %>%
  ggplot(aes(x = Timeperiod, y = Value)) +
  # geom_label_repel(aes(label = label), nudge_x = 1, nudge_y = 3,
  #                 na.rm = TRUE) +
  geom_line(aes(group = IndicatorName, color = IndicatorName, linetype = IndicatorName),
            size = 3, alpha = 0.8) + 
  geom_point(size = 2, alpha = 0.5) +
  scale_color_discrete(name = "Groups by Color",
                       labels = single_indicators_names) +
  scale_linetype_discrete(guide = "none") +
  ggtitle("NCMP data for Leeds") 

ggplotly(p = ggplot_leeds_prevalence)
