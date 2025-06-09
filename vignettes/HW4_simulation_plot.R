
rm(list=ls())
load("C:/Users/jrjor/Documents/UW/BIOST561/UWBiost561/vignettes/HW4_simulation.RData")
library(ggplot2)
library(cowplot)
library(tidyverse)


# 1. Summarise: count how many 1’s per (alpha, implementation)
df_summary <- data  %>%
  group_by(alpha, implementation) %>%
  summarise(
    total = sum(outcome)
  )

# 2. Plot
plot_ = ggplot(df_summary, aes(x = factor(implementation), y = total)) +
  geom_col() +                            # geom_col() uses your precomputed 'count'
  facet_wrap(~ alpha, scales = "free_x") +# one panel per alpha
  labs(
    x = "Implementation",
    y = "Number success in 13 trials",
    title = "Count of Positive Outcomes by Implementation and Alpha"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("C:/Users/jrjor/Documents/UW/BIOST561/UWBiost561/vignettes/HW4_simulation.png",
       plot = plot_,
       width = 10,          # fixed width
       height = 7.3, # dynamic height
       dpi = 400)

