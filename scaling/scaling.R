library(tidyverse)
library(patchwork)

setwd("~/zzz_SLIM/scaling")

original <- read_csv('./original/sum_K1000_burnin100_EN0_aIoC50_IM1_I0.5_S100_U100_AGV0_MS0.01_me0.01_mr0_WoFF0.1_1977.csv')  %>%
  filter(Population != 6) %>%
  mutate(Population = as.factor(Population),
         GsW = Generation -200) 
         
scaled <- read_csv('./scaled/sum_K1000_burnin100_EN0_aIoC50_IM1_I0.5_S100_U100_AGV0_MS0.01_me0.1_mr0_WoFF1_1977.csv') %>%
  filter(Population != 6) %>%
  mutate(Population = as.factor(Population),
         GsW = Generation -200) 

centered <- read_csv('./centered/sum_K1000_burnin100_EN0_aIoC50_IM1_I0.5_S100_U100_AGV0_MS0.01_me0.1_mr0_WoFF1_1977.csv') %>%
  filter(Population != 6) %>%
  mutate(Population = as.factor(Population),
         GsW = Generation -200) 

original

(pO <- ggplot(original, aes(GsW, Phenotype, color = Population)) +
  geom_line(alpha = 0.8) +
  theme_bw() +
  labs(color = 'Population', y = 'Mean population phenotype', x = '',
       title = 'Original model parameters') +
  scale_x_continuous(limits = c(-20, 150)))

(pS <- ggplot(scaled, aes(GsW, Phenotype, color = Population)) +
  geom_line(alpha = 0.8) +
  theme_bw() +
  labs(color = 'Population', y = 'Mean population phenotype', x = '',
  title = 'Re-scaled model parameters by a factor of 10') +
  scale_x_continuous(limits = c(-20, 150)))

(pC <- ggplot(centered, aes(GsW, Phenotype, color = Population)) +
    geom_line(alpha = 0.8) +
    theme_bw() +
    labs(color = 'Population', y = 'Mean population phenotype', x = 'Generations since warming',
        title = 'Re-scaled and re-centered model parameters') +
    scale_x_continuous(limits = c(-20, 150)))

pO / pS / pC + plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'A')

ggsave('scaling.png', dpi = 300, height = 8, width = 6)
