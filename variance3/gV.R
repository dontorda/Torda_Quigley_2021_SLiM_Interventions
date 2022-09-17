library(tidyverse)
library(patchwork)

# This script demonstrates the interaction of the number of loci (L), mutation effect (a), 
# heterozygosity (He = 2p(1-p), where p is the frequency of the derived allele),
# and genetic variance (Gv = a^2 * He summed over all loci).

# We explore the following scenarios. 
# constant mutation effect (a), constant He
df1 <- tibble(
  L = c(60, 70, 80, 90, 100, 120, 200),
  a = rep(0.01, 7),
  La = L*a,
  p = rep(0.5, 7),
  He = 2*p*(1-p),
  Gv = L*(a^2)*He
)

# constant phenotype (L*a), constant He
df2 <- tibble(
  L = c(60, 70, 80, 90, 100, 120, 200),
  La = 1,
  a = La/L,
  p = rep(0.5, 7),
  He = 2*p*(1-p),
  Gv = L*a^2*He
)

# constant genetic variance (Gv), constant He
df3 <- tibble(
  L = c(60, 70, 80, 90, 100, 120, 200),
  Gv = 0.005,
  p = rep(0.5, 7),
  He = 2*p*(1-p),
  a = sqrt(Gv/(L*He)),
  La = L*a,
)


df <- bind_rows(list(df1, df2, df3), .id = 'id') %>%
  pivot_longer(cols = c('a', 'La', 'Gv', 'He'), names_to = 'metric', values_to = 'value') %>%
  mutate(id = factor(id, levels = c(1, 2, 3), 
                     labels = c('constant: a and He', 
                                'constant: L*a and He',
                                'constant: Gv and He')),
         metric = factor(metric, levels = c('a', 'La', 'Gv', 'He'),
                         labels = c('Mutation effect (a)', 'Phenotye (L*a)',
                                    'Genetic variance (Gv)', 'Heterozygosity (He)')))

df %>%
  ggplot(aes(L, value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = 'Number of loci (L)', breaks = unique(df1$L)) +
  scale_y_continuous(name = '') +
  theme_bw() +
  theme(legend.position = 'none', legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90))+
  facet_grid(metric~id, scales = 'free_y')

ggsave('a_La_Gv_He.png', dpi = 300, height = 6, width = 6)