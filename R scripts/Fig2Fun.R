Fig2 <- function(dataset, P) {
ss1 <- dataset %>%
  filter(K == 10000, I == 0.5, S == 100, U == 100, me == 0.01, mr == 0, aIoC == 50, Population != 6, WoFF == 0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         MS = as.numeric(MS),
         IM = as.factor(IM),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", "adding new GV")),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100),
         MS = as.factor(paste0(MS *100, '%'))) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, MS, EN, AGV2, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness) 

FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBcon <- FB %>%
  select(EN, AGV2, GenNorm, Population, MS, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

# Let's summarize the influence in an object that we plot with other parameters later
dfCon <- FBcon %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'Connectivity') %>%
  rename(Values = MS)

### Effect of genetic diversity

ss1 <- dataset %>%
  filter(K == 10000, I == 0.5, S == 100, !U %in% c(50, 150, 200), MS == 0.01, me == 0.01, mr == 0, aIoC == 50, Population != 6, WoFF == 0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         U = as.numeric(U),
         IM = as.factor(IM),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", "adding new GV")),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, AGV2, HeR, U, EN, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness)
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBdiv <- FB %>%
  select(EN, AGV2, GenNorm, Population, HeR, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

dfDiv <- FBdiv %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'Diversity') %>%
  rename(Values = HeR)

### Effect of environmental tolerance
ss1 <- dataset %>%
  filter(K == 10000, I == 0.5, S == 100, U == 100, me == 0.01, mr == 0, aIoC == 50, Population != 6, MS == 0.01, WoFF != 0.01) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         MS = as.numeric(MS),
         IM = as.factor(IM),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", "adding new GV")),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, WoFF, EN, AGV2, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness)
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBwoff <- FB %>%
  select(EN, AGV2, GenNorm, Population, WoFF, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

dfWoFF <- FBwoff %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'Environmental tolerance') %>%
  rename(Values = WoFF)

##Effect of inoculum size
ss1 <- dataset %>%
  filter(K == 10000, U == 100, S == 100, MS == 0.01, me == 0.01, mr == 0, aIoC == 50, Population != 6, WoFF == 0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         I = as.numeric(I),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", "adding new GV")),
         IM = as.factor(IM),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()


#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, I, AGV2, EN, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness)
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBino <- FB %>%
  select(EN, AGV2, GenNorm, Population, I, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

dfIno <- FBino %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'Inoculum') %>%
  rename(Values = I)

### Effect of population size
ss1 <- dataset %>%
  filter(I == 0.5, U == 100, S == 100, MS == 0.01, me == 0.01, mr == 0, aIoC == 50, Population != 6, WoFF == 0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         K = as.numeric(K),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", "adding new GV")),
         IM = as.factor(IM),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, K, AGV2, EN, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness)
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBpop <- FB %>%
  select(EN, AGV2, GenNorm, Population, K, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))
# when I run it on multiple no, I will be able to create boxplot per generation per MS

dfPop <- FBpop %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'Population Size') %>%
  rename(Values = K)

#Effect of the number of QTLs
ss1 <- dataset %>%
  filter(K == 10000, S == 100, U %in% c(1000,  500,  200,  100,   50), MS == 0.01, I == 0.5, mr == 0, aIoC == 50, Population != 6, WoFF == 0.1) %>%
  #filter(K == 10000, U %in% c(800, 400, 160, 80, 40), S == 100, MS == 0.01, I == 0.5, mr == 0, aIoC == 50, Population != 6, WoFF == 0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         me = as.numeric(me),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', rep('Scenario 3', 5))),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", rep("adding new GV", 5))),
         IM = as.factor(IM),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

ss2 <- ss1 %>% filter(me == 0.01, U == 200)
ss1<- ss1 %>% filter(!id %in% ss2$id)

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, U, AGV2, EN, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness)
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBme <- FB %>%
  select(EN, AGV2, GenNorm, Population, U, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

dfMe <- FBme %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'Number of genes') %>%
  rename(Values = U)

### Effect of mutation rate
ss1 <- dataset %>%
  filter(K == 10000, U == 100, S == 100, MS == 0.01, me == 0.01, I == 0.5, Population != 6, aIoC == 50, mr != 0.001, WoFF ==0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         mr = as.numeric(mr),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", "adding new GV")),
         IM = as.factor(IM),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, mr, AGV2, EN, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness)
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBmr <- FB %>%
  select(EN, AGV2, GenNorm, Population, mr, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

dfMr <- FBmr %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'Mutation rate') %>%
  rename(Values = mr)

### Effect of lab stock size
ss1 <- dataset %>%
  filter(K == 10000, U == 100, MS == 0.01, me == 0.01, AGV == -1, I == 0.5, Population != 6, mr == 0, aIoC == 50, WoFF == 0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         S = as.numeric(S),
         AGV2 = factor(AGV, labels = c('Scenario 1')), #, 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes")), # "reshuffling SGV", "adding new GV")),
         IM = as.factor(IM),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, S, AGV2, EN, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness)
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBS <- FB %>%
  select(EN, AGV2, GenNorm, Population, S, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

dfS <- FBS %>%
  filter(GenNorm == 1,
         EN == 'with El Nino',
         AGV2 == 'Scenario 1',
         Population == P)%>%
  mutate(Parameter = 'Stock size') %>%
  rename(Values = S)

### Effect of El Nino-like oscillations
ss1 <- dataset %>%
  filter(K == 10000, U == 100, S == 100, MS == 0.01, me == 0.01, I == 0.5, Population != 6, mr == 0, WoFF == 0.1) %>%
  mutate(Generation = as.numeric(Generation),
         Population = as.factor(Population),
         aIoC = as.numeric(aIoC),
         AGV2 = factor(AGV, labels = c('Scenario 1', 'Scenario 2', 'Scenario 3')),
         AGV = factor(AGV, labels = c("crossing extremes", "reshuffling SGV", "adding new GV")),
         IM = as.factor(IM),
         EN = factor(EN, labels = c("with El Nino", "without El Nino")),
         GenNorm = Generation-(aIoC+burnin+100)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(GenNorm %in% c(-2:60)) %>%
  droplevels()

#Ok, now let's calculate the fitness effect
FB <- ss1 %>%
  select(GenNorm, Population, Fitness, IM, aIoC, AGV2, EN, no) %>%
  pivot_wider(names_from = IM, names_prefix = 'IM', values_from = Fitness) %>%
  mutate(ENSO = aIoC-48,
         ENSO2 = paste('Phase', ENSO))
FB$FB <- ifelse(is.na(FB$IM1),  FB$IM2-FB$IM0, FB$IM1-FB$IM0)

# create objects on the relative importance of teh parameter for final compound figure
FBaIoC <- FB %>%
  select(EN, AGV2, GenNorm, Population, aIoC, FB, no) %>%
  filter(GenNorm %in% c(0, 1, 2, 3, 4, 5, 10))

dfaIoC <- FBaIoC %>%
  filter(GenNorm == 1,
         AGV2 == 'Scenario 1',
         EN == 'with El Nino',
         Population == P)%>%
  mutate(Parameter = 'El Nino phase') %>%
  rename(Values = 5)

FBlist <- list(FBcon, FBdiv, FBwoff, FBino, FBme, FBmr, FBpop, FBaIoC, FBS)
names(FBlist) <- c('FBcon', 'FBdiv', 'FBwoff', 'FBino', 'FBme', 'FBmr', 'FBpop', 'FBaIoC', 'FBS')
e <- 'with El Nino'
ls <- list()
ls<-lapply(seq_along(FBlist), FUN = function(x) {
  n <- names(FBlist)[[x]]
  y <- FBlist[[x]] %>% 
    filter(GenNorm == 1,
           #AGV == 'crossing extremes',
           EN == e,
           Population == P) %>%
    mutate(Parameter = n) %>%
    rename(Values = 5) %>%
    mutate(Values = as.factor(Values))
  y
})

df <- do.call(rbind, ls)

dfSum <- df %>%
  group_by(Parameter, AGV2, no) %>%
  summarise(Range = max(FB)-min(FB), SD = sd(FB)) %>%
  mutate(Parameter = as.factor(Parameter))
levels(dfSum$Parameter)
dfSum$Parameter <- factor(dfSum$Parameter, labels = c("El Nino phase", "Connectivity",  "Genetic diversity",  "Inoculum ratio",  "Mutation effect",   "Mutation rate",   "Population size",  "Broodstock size", "Environmental tolerance"))
return(dfSum)
}