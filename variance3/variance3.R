# This script is demonstrating the effect of mutation effect and gene number on 
# genetic diversity and genetic variance. The corresponding models were modified from the original,
# and now heterozygosity and the frequency of the derived allele are outputs in the summary files of the 
# slim models. The input of this script is the summary files of the models, run with varying values of mutation
# effect and numbe rof QTL.

library(tidyverse)
library(patchwork)

#The first input folder contains the summary files from the model that was run by constant mutation effect,
#and afterwards by constant phenotypic range (nQTL*me)

setwd("~/zzz_SLIM/variance/M5v4P4b_var")
filenames <- Sys.glob('./sum_*.csv')
ls <- list()
for (f in filenames) {
  df <- read_csv(paste0("./", f), trim_ws = T)  %>% 
    separate(id, sep = "_", into = c("K", "b", "EN", "aIo", "IM", 'I', 'S', 'U', 'AGV', 'MS', 'me', 'mr', 'W', 'no'), remove = T) %>%
    select(-c("K", "b", "EN", "aIo", "IM", 'I', 'S', 'AGV', 'MS', 'mr', 'W')) %>%
    mutate(U = (as.numeric(parse_number(U))), me = as.factor(parse_number(me)),
           Population = as.factor(Population),
           no = as.factor(as.numeric(no)))
  
  ls[[f]] <- df
}

df <- bind_rows(ls) %>%
  na.omit() %>%
  mutate(no = as.factor(as.numeric(no))) %>%
  distinct()
summary(df)

#There are two datasets embedded, one where me = 0.01 for all U values, and one where me changes with U
# In the former, phenotypes would vary at the start
df1 <- df %>%
  filter(me == 0.01) 

summary(df1)

#Check if the models ran well by plotting Phenotype over Generation
df1 %>%
  ggplot(aes(Generation, Phenotype, color = Population)) +
  geom_line() +
  facet_grid(no~U)

df2 <- df %>%
  filter(me != 0.01 | (me == 0.01 & U ==100)) 

df2 %>%
  ggplot(aes(Generation, Phenotype, color = Population)) +
  geom_line() +
  facet_grid(no~U)

#Nice. So the models ran ok.

# The second input folder contains teh summary files from model runs where we kept inital genetic variation constant
setwd("~/zzz_SLIM/variance/M5v4P4b_varGv")
filenames <- Sys.glob('./sum_*.csv')
lsGv <- list()
for (f in filenames) {
  df <- read_csv(paste0("./", f), trim_ws = T)  %>% 
    separate(id, sep = "_", into = c("K", "b", "EN", "aIo", "IM", 'I', 'S', 'U', 'AGV', 'MS', 'me', 'mr', 'W', 'no'), remove = T) %>%
    select(-c("K", "b", "EN", "aIo", "IM", 'I', 'S', 'AGV', 'MS', 'mr', 'W')) %>%
    mutate(U = (as.numeric(parse_number(U))), me = as.factor(parse_number(me)),
           Population = as.factor(Population),
           no = as.factor(as.numeric(no)))
  
  lsGv[[f]] <- df
}

dfGv <- bind_rows(lsGv) %>%
  na.omit() %>%
  mutate(no = as.factor(as.numeric(no))) %>%
  distinct()
summary(dfGv)


#Let's combine all df files

HeMaster <- bind_rows(list(df1=df1, df2 = df2, dfGv=dfGv), .id = 'id')

#It would be smart to tear it apart and put it back together so that U = 100, me = 0.01
# appears in every contrast

He1 <-  HeMaster %>%
  filter(id == 'df1', me == 0.01)
He2 <- HeMaster %>%
  filter(id == 'df2', me != 0.01 | (me == 0.01 & U ==100))
He3 <- HeMaster %>%
  filter(id == 'dfGv' | (me == 0.01 & U ==100 & no == 1))

HeMaster2 <- bind_rows(list(aHe=He1, LaHe=He2, GvHe=He3), .id = 'id') %>%
  mutate(id = factor(id, levels = c("aHe", "LaHe", "GvHe"),
                     labels = c('constant: a and He', 
                                'constant: L*a and He',
                                'constant: Gv and He')))

setwd("~/zzz_SLIM/variance")
# save(HeMaster, HeMaster2, file = 'HeMaster.RData')
summary(HeMaster2)

#pivot long and plot the results
HeMaster2 %>%
  mutate(me = as.numeric(as.character(me)), 
         Ume = as.numeric(as.character(U))*me,
         Gv = me^2*Fr*U) %>%
  select(id, no, Generation, Population, He, U, me, Ume, Gv) %>%
  group_by(id, Generation, no, U) %>%
  summarize(He = mean(He), me = mean(me), Ume = mean(Ume), Gv = mean(Gv)) %>%
  ungroup() %>%
  pivot_longer(cols = c('me', 'Ume', 'Gv', 'He'), 
               names_to = 'metric', values_to = 'value') %>%
  mutate(metric = factor(metric, levels = c('me', 'Ume', 'Gv', 'He'),
                         labels = c('Mutation effect (a)', 'Phenotye (L*a)',
                                    'Genetic variance (Gv)', 'Heterozygosity (He)'))) %>%
  ggplot(aes(Generation, value, color = as.factor(U), group = interaction(U, no))) +
  # geom_point() +
  geom_line(alpha = 0.2) +
  labs(x = 'Generations of burn-in',
       y= '', color = 'Number of loci (L)') +
  facet_grid(metric~id, scales = 'free_y') +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 0)) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

ggsave('Realised a_La_Gv_He - 2a.png', dpi = 300, height = 7, width = 7)

HeMaster2 %>%
  filter(Generation %in% c(1, 100)) %>%
  mutate(me = as.numeric(as.character(me)), 
         Ume = as.numeric(as.character(U))*me,
         Gv = me^2*Fr*U) %>%
  select(id, no, Generation, Population, He, U, me, Ume, Gv) %>%
  group_by(id, Generation, no, U) %>%
  summarize(He = mean(He), me = mean(me), Ume = mean(Ume), Gv = mean(Gv)) %>%
  ungroup() %>%
  pivot_longer(cols = c('me', 'Ume', 'Gv', 'He'), 
               names_to = 'metric', values_to = 'value') %>%
  mutate(metric = factor(metric, levels = c('me', 'Ume', 'Gv', 'He'),
                         labels = c('Mutation effect (a)', 'Phenotye (L*a)',
                                    'Genetic variance (Gv)', 'Heterozygosity (He)'))) %>%
  ggplot(aes(U, value, color = as.factor(Generation), group = interaction(as.factor(Generation), no))) +
  # geom_point() +
  geom_line(alpha = 0.2) +
  scale_x_continuous(name = 'Number of loci (L)', breaks = unique(df1$U), minor_breaks = NULL) +
  labs(x = 'Number of loci (L)',
       y= '', color = 'Generations of burn-in') +
  facet_grid(metric~id, scales = 'free_y') +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90)) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

ggsave('Realised a_La_Gv_He - 3a.png', dpi = 300, height = 7, width = 7)


