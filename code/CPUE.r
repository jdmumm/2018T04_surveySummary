## ###################
#    CPUE           ##
######################
# Calc CPUE, including expanding subsamples
# Used here mostly for quick look at Pollock, Tomcod, and Sablefish. 

## LOAD ----
library (tidyverse)
library(plotrix)
events <- read.csv('./data/events.csv')
events %>% filter (USED_IN_ESTIMATE == 'YES', PROJECT_CODE == 'T04') %>%
  select( Event = EVENT_ID,
          year = YEAR,
          Project = PROJECT_CODE, 
          length=TOW_LENGTH_DESIGNATED,
          totCatch = CATCH_WEIGHT) -> event

catch <- read.csv('./data/catchComp.csv')
catch %>% filter (FK_PROJECT_CODE == 'T04') %>%
select(Project = FK_PROJECT_CODE, Event = EVENT_ID, species=SPECIES_CODE, 
                 count=COUNT,
                 wt=SAMPLE_WT_KG, 
                 sample_type = SAMPLE_TYPE) %>% filter(Event %in% event$Event) -> catch

## ESTIMATE CATCH BY TOW, then CPUE, and aggregate by Bed ----

s <- '250' # Species of interest
# aggregate sampled catch and species of interest 
catch %>% group_by(Event) %>% 
  summarise(#t1 = sum(sample_wt[species == 99997 & sample_type == 1]),
            t2 = sum(wt[sample_type == 2], na.rm = T),
            s1 = sum(wt[species == s & sample_type == 1], na.rm = T), 
            s2 = sum(wt[species == s & sample_type == 2], na.rm = T),
            totSamp = sum(wt), na.rm = T) %>% 
  # join to event for totCatch,length and set any events with no catch to 0. 
  right_join (select( event, Event,year, totCatch, length)) %>%
  mutate (t1 = totCatch -totSamp) %>%
  mutate (t1 = if_else( t1 < 0, 0, t1)) %>%  # Bandaid to set tows with neg remaining catch to 0
  replace_na (list(t1 = 0, t2 = 0, s1 = 0, s2 = 0)) %>%
  # expand any T2's, sum components of catch, and calc CPUE   
  mutate ( sTot = s1 + s2 + if_else(t2>0, (t1*s2/t2), 0), #expanded catch of species s by event.  Con to prevent div by 0.
           sCPM = sTot/length)  -> cpm_bytow_250          # CPUE cnt/nmi 
  # aggregate by year     
  cpm_bytow_250 -> group_by(year) %>% summarise (tows =  n(),
                                cpue_mean = mean(sCPM), 
                                cpue_sd = sqrt(var(sCPM)), 
                                cpue_cv = 100 * cpue_sd/cpue_mean,
                                totCatch = sum(sTot)) ->  cpm_byYear # append species code to output manually 
cpm_byYear # CPUE (kg/nmi) with sd, cv and total expanded catch (cnt) by bed, all for sp. s.     

write.csv(cpm_byYear_250, './output/cpm_byYear_250.csv')
write.csv(cpm_byYear_710, './output/cpm_byYear_710.csv')
write.csv(cpm_byYear_270, './output/cpm_byYear_270.csv')

write.csv(cpm_bytow_250, './output/cpm_byTow_250.csv')
write.csv(cpm_bytow_710, './output/cpm_byTow_710.csv')
write.csv(cpm_bytow_270, './output/cpm_byTow_270.csv')

############################################################################## #
## PLOTS ##

## Plot cpm mean and SD ---- 
par(mfrow = c(3,1))
par(mar=c(3.1,4.1,2,1))
par(mgp = c(2, 1,0))

dat <- cpm_byYear_250
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$cpue_sd,
       li = ifelse((dat$cpue_mean - dat$cpue_sd) > 0 , (dat$cpue_mean - dat$cpue_sd), 0),
       xlim = c(1998,2017), 
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Pacific Tomcod')
    
dat <- cpm_byYear_710
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$cpue_sd,
       li = ifelse((dat$cpue_mean - dat$cpue_sd) > 0 , (dat$cpue_mean - dat$cpue_sd), 0),
       xlim = c(1998,2017), 
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Sablefish')

dat <- cpm_byYear_270
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$cpue_sd,
       li = ifelse((dat$cpue_mean - dat$cpue_sd) > 0 , (dat$cpue_mean - dat$cpue_sd), 0),
       xlim = c(1998,2017),
       ylim = c (0,800),
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Walleye Pollock')

## Plot cpm mean and SE ---- 
read.csv('./output/cpm_byYear_250.csv')%>%
  filter (year > 1997) %>% mutate (se = cpue_sd/(tows^.5)) -> cpm_byYear_250
read.csv('./output/cpm_byYear_710.csv')%>%
  filter (year > 1997) %>% mutate (se = cpue_sd/(tows^.5)) -> cpm_byYear_710
read.csv('./output/cpm_byYear_270.csv')%>%
  filter (year > 1997) %>% mutate (se = cpue_sd/(tows^.5)) -> cpm_byYear_270

par(mfrow = c(3,1))
par(mar=c(3.1,4.1,2,1))
par(mgp = c(2, 1,0))

dat <- cpm_byYear_250
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$se,
       li = dat$cpue_mean - dat$se,
       xlim = c(1998,2017), 
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Pacific Tomcod')

dat <- cpm_byYear_710
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$se,
       li = dat$cpue_mean - dat$se,
       xlim = c(1998,2017), 
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Sablefish')

dat <- cpm_byYear_270
plotCI(dat$year, dat$cpue_mean, col= "black", lwd=1,  pch= 19, cex = 1.0,
       ui = dat$cpue_mean + dat$se,
       li = dat$cpue_mean - dat$se,
       xlim = c(1998,2017),
       ylab = 'CPUE (kg/nmi)', xlab = 'Year', main = 'Walleye Pollock')

## PLOT cpm BOX PLOTS ----

read.csv('./output/cpm_byTow_250.csv')%>%
  filter (year > 1997) %>% mutate (Year = as.factor(year)) -> cpm_bytow_250
read.csv('./output/cpm_byTow_710.csv') %>% 
  filter (year > 1997) %>% mutate (Year = as.factor(year))-> cpm_bytow_710
read.csv('./output/cpm_byTow_270.csv') %>% 
  filter (year > 1997) %>% mutate (Year = as.factor(year))-> cpm_bytow_270

  ## with outliers 
  ggplot (data = cpm_bytow_250, aes(x=Year, y = sCPM),  ylim = c(0, 150)) %>%
    + geom_boxplot( ) 
  
  ggplot (data = cpm_bytow_710, aes(x=Year, y = sCPM),  ylim = c(0, 150)) %>%
    + geom_boxplot() 
  
  ggplot (data = cpm_bytow_270, aes(x=Year, y = sCPM),  ylim = c(0, 150)) %>%
    + geom_boxplot() 
  
  ## w/out outliers
  ggplot (data = cpm_bytow_250, aes(x=Year, y = sCPM),  ylim = c(0, 150)) %>%
    + geom_boxplot( outlier.shape = NA) %>%
    + ylim (c(0,150))
  
  ggplot (data = cpm_bytow_710, aes(x=Year, y = sCPM),  ylim = c(0, 150)) %>%
    + geom_boxplot( outlier.shape = NA) %>%
    + ylim (c(0,20))
  
  ggplot (data = cpm_bytow_270, aes(x=Year, y = sCPM),  ylim = c(0, 150)) %>%
    + geom_boxplot( outlier.shape = NA) %>%
    + ylim (c(0,500))


