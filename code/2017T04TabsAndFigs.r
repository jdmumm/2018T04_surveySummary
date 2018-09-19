#Format tables for 2017 Kachemak Survey SUmmary, mstly from 931_popEstAndCPUE_161101.R used for 2016 report

#LOAD ----
library(tidyverse)
library(stats)
library(plotrix)
library (Hmisc)
options (scipen = 10)
dat_17 <- read.csv('./data/qP_simp_17_170916.csv') # using new size classes from 2017 SD op-plan
dat_old <- read.csv('./data/qP_simp_oldSCs_170916.csv')# using the old pre-2017 size classes 
events <- read.csv('./data/events.csv')
events %>% filter (PROJECT_CODE == 'T04', GEAR_PERFORMANCE_CODE == '1') %>%
  select( Event = EVENT_ID,
          year = YEAR,
          Project = PROJECT_CODE, 
          length=TOW_LENGTH_DESIGNATED,
          totCatch = CATCH_WEIGHT) -> event
awl <- read.csv('./data/AWLshellfish_2017T04_180201.csv')

## Males Main ----

  # New Size Classes 
  m <- dat_17[dat_17$PROJECT_CODE == "T04", c(1:7,16,18,20,22,24,26)] 
  #combine news and olds for P1s and P2s
  m$P1 <- m$MT5_P_ + m$MT6_P_
  m$P2 <- m$MT7_P_ + m$MT8_P_
  names(m)[c(12,13)] <- c('P3','P4')      #"MT9_P_"  "MT10_P_"
  m <- m[,-(8:11)] # remove columns with new old split
  #reorder columns
  m <- m[,c(1,2,3,9,8,11,10,4:7)]
  m
  #write.csv(m,'./output/931PopMales_Main_17.csv')
  
  # Old Size Classes  - samee as above, just differnt file names. 
  m <- dat_old[dat_old$PROJECT_CODE == "T04", c(1:7,16,18,20,22,24,26)] 
  #combine news and olds for P1s and P2s
  m$P1 <- m$MT5_P_ + m$MT6_P_
  m$P2 <- m$MT7_P_ + m$MT8_P_
  names(m)[c(12,13)] <- c('P3','P4')      #"MT9_P_"  "MT10_P_"
  m <- m[,-(8:11)] # remove columns with new old split
  #reorder columns
  m <- m[,c(1,2,3,9,8,11,10,4:7)]
  m
 # write.csv(m,'./output/931PopMales_Main_old.csv')

## Females Main ----
dat_17 %>% filter (PROJECT_CODE == 'T04') %>% select(year = YEAR, tows = n,
                    FT11_P_, FT11_P_CI_, MF_P_, MF_P_CI_, TF_P_, TF_P_CI_) -> f
  
  write.csv(f,'./output/931PopFems_Main.csv')

##  Catch by Station (per Carol request) ----
read.csv('./data/C_17_170921.csv') %>%
  right_join(event, by = c('EVENT_ID' = 'Event')) %>% # limited to good tows at top
  filter (YEAR == 2017) %>% transmute(
    Station = STATION_ID,
    'Pre4' = MT10_T, 
    'Pre3' = MT9_T,
    'Pre2' = MT7_T + MT8_T,
    'Pre1' = MT5_T + MT6_T,
    Recruit = MT1_T + MT2_T,
    Post = MT3_T +MT4_T,
    TotMales = TM_T,
    JuvFems = FT11_T,
    MatFems = FT12_T +FT13_T,
    TotFems = TF_T) %>% arrange(Station) %>%
  mutate_if(is.numeric, funs(as.character(formatC(round(., 1),1,format = "f")))) -> c

  write.csv(c,'./output/2017T04_931CatchByStation_17sc.csv') 
  # previously a version of this from SQL, emailed to KG.  

## CPUE by station (per KG request 180130, not incorporated to 2017 rmd) ----
  read.csv('./data/C_17_170921.csv') %>%  right_join(event, by = c('EVENT_ID' = 'Event')) %>% # limited to good tows at top
    filter  (YEAR == 2017) %>% transmute(
    Station = STATION_ID, 
    length = length, 
    Sublegal = (MT5_T + MT6_T + MT7_T + MT8_T + MT9_T + MT10_T)/length, 
    Legal = LM_T/length, 
    Tot_males = TM_T/length, 
    Juvenile_fems = FT11_T/length, 
    Mature_fems = (FT12_T + FT13_T)/length, 
    Tot_fems = TF_T/length) %>% arrange (Station) -> cpm 
  
  write.csv(cpm ,'./output/2017T04_931CPUEByStation_17sc.csv') 
  
  # calc ranges and cv for KG 
  events %>% filter (PROJECT_CODE == 'T04', YEAR == 2017, USED_IN_ESTIMATE == 'YES') %>% left_join(cpm, by= c("STATION_ID" = "Station")) %>% # exclude 139
  group_by(YEAR) %>% summarise( n= n(),
                                LM_mean = mean(Legal), 
                                LM_min = min(Legal), 
                                LM_max = max(Legal),
                                LM_CV = (var(Legal)^.5)/LM_mean, 
                                LM_SEM = (var(Legal)^.5)/(n^.5), 
                                SM_mean = mean(Sublegal), 
                                SM_min = min(Sublegal), 
                                SM_max = max(Sublegal),
                                SM_CV = (var(Sublegal)^.5)/SM_mean, 
                                SM_SEM = (var(Sublegal)^.5)/(n^.5), 
                                JF_mean = mean(Juvenile_fems),
                                JF_MIN = min(Juvenile_fems), 
                                JF_MAX = max(Juvenile_fems), 
                                JF_CV = ((var(Juvenile_fems)^.5))/JF_mean, 
                                MF_mean = mean(Mature_fems), 
                                MF_min = min(Mature_fems),
                                MF_max = max(Mature_fems), 
                                MF_CV = ((var(Mature_fems)^.5))/MF_mean) %>% select (JF_mean, JF_CV, MF_mean, MF_CV)
      
  
##Plot LM ---- 
  
  dat <- dat_17 
  # dat <- dat_old 
  
  #Convert to thousands of crabs
  dat$LM_P <- dat$LM_P_/1000
  dat$LM_P_CI <- dat$LM_P_CI_/1000
  
  dat <- select(dat, "proj" = PROJECT_CODE, "yr" = YEAR,  LM_P, LM_P_CI)
  
  t04 <- dat[dat$proj == "T04",]
  
  #T04
  par(mfrow=c(1,1), las=1, mgp=c(3.2, 1, 0), mar=c(3.8, 4.2, 1, 1), family = "serif", font = 1,
      cex = 1, ps = 12, cex.lab = 1.16, bty = "l")
  
  #LM
  dat <- t04
  plotCI(dat$yr, dat$LM_P, col= "black", lwd=1,  pch= 19, cex = 1.0,
         ui = dat$LM_P + dat$LM_P_CI,
         li = ifelse((dat$LM_P - dat$LM_P_CI) > 0 , (dat$LM_P - dat$LM_P_CI), 0),
         ylab= "Thousands of Crab", xlab = "", xaxt='n', yaxt='n', las = '1',font.lab = 2,
         ylim = c(0,2950))
  mtext("Year", side = 1, line = 2,font=2, cex= 1.16)
  axis(side=1, at=seq(1990, 2017, by=1, las = '1', font = 2))
  axis(side=2, at = seq(0, 2950, by=500, font = 2),
       labels=formatC(seq(0, 2950, by=500),"d", big.mark=','))
  minor.tick(ny = 5, nx = 0 )
  # THRESHOLDS , KG asked not to include thresholds ----
    # lines(x= c(2002,2017), y = c(500,500), lty = 5, col = "black", lwd =2)
    # lines(x= c(2002,2017), y = c(100,100), lty = 3, col = "dimgrey")
    # lines(x= c(2002,2017), y = c(50,50), lty = 5, col = "dimgrey")
    # 
    # detach("package:dplyr", unload=TRUE)
    # library(stats)
    # rec<- dat[dat$yr > 1997,]
    # f5 <- rep(1/5, 5)
    # y_lag <- filter(rec$LM_P, f5, sides=1)
    # points(rec$yr, y_lag, col="dimgrey", cex = 1.0)
    # 
    # legend ("topright", inset = .1,
    #         legend = c("Commercial MSST","Noncommercial 5-year threshold", "Noncommercial 1-year threshold", "5-Year average"),
    #         col = c("black","dimgrey","dimgrey", "dimgrey"),
    #         lty = c(5,3,5, NA), pch = c(NA,NA,NA,1), lwd = c(2,1,1,NA), bty = "n", y.intersp =1.2)

## KING and DUNGY CPUE    ##   from 161104 ----
 

  ##DUNGIES##
  dat <- read.csv("./data/qP_910_170920.csv")
  
  dat <-  dat[c("PROJECT_CODE", "YEAR","n","SM_CBar","SM_varC", "LM_CBar","LM_varC","TM_CBar","TM_varC",
                "TF_CBar","TF_varC")]
  
  ##Calc sample SDs
  dat$SM_SD <- (dat$SM_varC^.5)
  dat$LM_SD <- (dat$LM_varC^.5) 
  dat$TM_SD <- (dat$TM_varC^.5)
  dat$TF_SD <- (dat$TF_varC^.5)
  
  #remove extra cols, reorder and rename 
  dung_pm <- select(dat,"Proj" = PROJECT_CODE, "Year" = YEAR, n,
                    SM_CBar, SM_SD, LM_CBar, LM_SD, TM_CBar, TM_SD, TF_CBar, TF_SD)
  
  # order by project and year 
  dung_pm <- arrange(dung_pm, Proj, Year)
  dung_pm
  write.csv(dung_pm, "./output/910_pm_170920.csv")

## Tanner CH vs CW plot ## ----
  
read.csv ("./data/931_CHCW_T04T05T06_usedInEst_90to17.csv") %>% 
filter(PROJECT_CODE == "T04", YEAR > 2007) -> awl # 2006 had CH too, but excluding to fit on 6 panel plot

yrs <- unique(awl$YEAR)  

par(mfcol=c(3,2))
par(mar=c(3.1,4.1,1,1))
par(mgp = c(2, 1,0))
for (i in yrs)
  {
  awl %>% filter (YEAR == i, SEX_CODE == '1') %>%  
    select(cw = BIOLOGICAL_WIDTH_MM, ch = CHELA_HEIGHT_MM, sc17 = CRAB_SIZE_CLASS_CODE_17) %>% 
    mutate (cw_ln = log(cw), ch_ln = log(ch), rat = ch_ln/cw_ln) -> len
  
  #len %>% filter(cw_ln > 3.8 & cw_ln < 5.2) -> len # exclude few outliers
  
  #t <- .62
  plot (ch_ln ~ cw_ln, data =len, 
        ylim = c(1.25,3.75),
        xlim = c(3.75,5.1),
        cex = .9,
        #col = 'gray20',
        #col = ifelse(rat < t,'gray60','black'),
        #pch = ifelse(rat < t,1,4),
        #cex = ifelse(rat < t,.8,.5),
        xlab = 'ln(carapace width)', ylab = 'ln(chela height)' )        
  abline( v = log(114), lwd = 3, col = 'gray20')
  abline( v = log(140), lwd = 3, lty ="dotted", col = 'gray20')
  
  # lm(ch_ln ~ cw_ln, len, rat < t) -> lm_s 
  # lm(ch_ln ~ cw_ln, len, rat > t) -> lm_l
  # abline(lm_s, col = 'gray60', lty = 'dashed' )
  # abline(lm_l, col = 'black', lty = 'dashed')

  
  legend( x=3.8, y=3.7 , bty = 'n', legend = c('114mm CW', '140mm CW'),
          lwd = c(3,3), lty = c('solid','dotted'), col =c('gray20','gray20'))
  
  #legend(x = 3.8, y = 3.5, bty = 'n', legend = c('large-claw: ln(ch)/ln(cw) > 0.62', 'small-claw: ln(ch)/ln(cw) < 0.62'),
        #col = c("black", 'gray60'), pch = c(1,4), pt.cex = c(.8,.5) )       
  legend('bottomleft', legend = i)
  }  

  # picking rat threshold    
  ggplot(aes(rat))+geom_density(alpha=.2)
    
  # above plot with only raw data
  plot (ch_ln ~ cw_ln, data =len, 
        xlim = c(3.75,5.1),
        xlab = 'ln(carapace width)', ylab = 'ln(chela height)' )        
  abline( v = log(114), lwd = 3)
  abline( v = log(140), lwd = 3, lty ="dotted")
  -
    -      legend( x=3.8, y=3.7 , bty = 'n', legend = c('114mm CW', '140mm CW'),
                   -              lwd = c(3,3), lty = c('solid','dotted'))
  
# Clutch Tables ----  # SUPER SLOPPY, NEEDS DELETING/CLEANING. this was added to ad-hoc as more requested
    awl %>% left_join (events) %>% filter (USED_IN_ESTIMATE == 'YES', YEAR == 2017, PROJECT_CODE == 'T04',
                                           SPECIES_CODE == '931', SEX_CODE == 2, CRAB_EGG_DEVELOMENT_CODE != '3') %>%  #excluding juveniles 
        select (EVENT_ID, num = CRAB_NUM, cw = BIOLOGICAL_WIDTH_MM,
                full = FULLNESS_PERCENT, cc = CLUTCH_CONDITION_CODE,ed = CRAB_EGG_DEVELOMENT_CODE ) -> clutch
    
      #freq table for each variable   
      eyed <- table(clutch$ed)
      full <- table(clutch$full)
      dead <- table(clutch$cc)
      
      #prop tables 
      eyed.p <- as.data.frame(round(prop.table(eyed)*100,2))
      full.p <- as.data.frame(round(prop.table(full)*100,2))
      dead.p <- as.data.frame(round(prop.table(dead)*100,2))
      
      # calc n, trouble binding to prop tables, manualy add to tables for now.  
      eyed.n <- margin.table(eyed)
      full.n <- margin.table(full)
      dead.n <- margin.table(dead)
      
      # write
      write_csv(eyed.p, 'output/eyed.csv') 
      write_csv(full.p, 'output/full.csv')
      write_csv(dead.p, 'output/dead.csv')
    
    # To match report table select matures rather than non 3s. 
      awl %>% left_join (events) %>% filter (USED_IN_ESTIMATE == 'YES', YEAR == 2017, PROJECT_CODE == 'T04',
                                             SPECIES_CODE == '931', SEX_CODE == 2, CRAB_EGG_DEVELOMENT_CODE, 
                                             MAT_CLASS == 'MAT') %>%  #excluding juveniles 
        select (EVENT_ID, num = CRAB_NUM, cw = BIOLOGICAL_WIDTH_MM,
                full = FULLNESS_PERCENT, cc = CLUTCH_CONDITION_CODE,ed = CRAB_EGG_DEVELOMENT_CODE ) -> clutch
    
    clutch %>% filter(!is.na(full)) %>% mutate %>% 
      mutate (f = ifelse(full == 0, "b",(ifelse(full >= 90, "f","p")))) -> cc# exclude nulls  
    
    table(cc$f)  
    margin.table(table(cc$f)) # n 
    100 * prop.table(table(cc$f)) -> clutch_full
    write.csv(clutch_full, './output/clutchfull_2017t04.csv')
      
    # Other clutch chracteristics for all years ---- 
    read.csv ('data/T04931_awlClutch_thru2017.csv') %>% left_join (events) %>%  #all years included here
      filter (USED_IN_ESTIMATE == 'YES', PROJECT_CODE == 'T04',
          SPECIES_CODE == '931', SEX_CODE == 2, CRAB_EGG_DEVELOMENT_CODE, 
           MAT_CLASS == 'MAT') %>%  #excluding juveniles 
    select (YEAR, EVENT_ID, num = CRAB_NUM, cw = BIOLOGICAL_WIDTH_MM,
              full = FULLNESS_PERCENT, cc = CLUTCH_CONDITION_CODE,ed = CRAB_EGG_DEVELOMENT_CODE ) -> dat
    
    # clutch condition
    dat %>% filter (!is.na(dat$cc)) -> clutch
    byYr <- table(clutch$YEAR, clutch$cc)
    propByYr<- prop.table(byYr,1)
    n <- margin.table(byYr,1)  
    
    t <- as.data.frame.matrix(propByYr)
    n <- as.data.frame(table(clutch$YEAR))
    
    cc <- merge.data.frame(t,n,by.x = "row.names", by.y = "Var1")
    cc %>% select (Year = Row.names, n = Freq, 'Dead eggs not apparent' ='1', 'Dead eggs < 20%' = '2', 'Dead eggs >20%' ='3',
                  'Barren with clean silky setae' = '4' , 'Barren with matted setae' = '5') -> cc
    write.csv(cc, './output/T04clutchCondition06to17.csv')

    #egg development 
    dat %>% filter (!is.na(dat$ed)) -> clutch
    byYr <- table(clutch$YEAR, clutch$ed)
    propByYr<- prop.table(byYr,1)
    n <- margin.table(byYr,1)  
    
    t <- as.data.frame.matrix(propByYr)
    n <- as.data.frame(table(clutch$YEAR))
    
    ed <- merge.data.frame(t,n,by.x = "row.names", by.y = "Var1")
    ed %>% select (Year = Row.names, n = Freq, 'Uneyed' = '1', 'Eyed' = '2', 'None or remnant eggs' ='4') -> ed
    write.csv(ed, './output/T04eggDev90to17.csv')

# 931 abundance table with new and old as requested by Carol 180301.  #Code Copied from catch up report, should be improved. 
  dat <- read.csv("./data/qP_simp_17_170916.csv")
    dat %>% filter (PROJECT_CODE == 'T04') -> dat
    str(dat)
    names(dat)
    
    ma <- dat[c(1,2,26,27,24,25,20:23,16:19,8:15)]
    ma
    str(ma)
    names(ma)
    newNames <- c("PROJECT_CODE","YEAR", "Pre-4", "Pre-4_CI","Pre-3", "Pre-3_CI",
                  "Pre-2n", "Pre-2n_CI", "Pre-2o", "Pre-2o_CI",
                  "Pre-1n", "Pre-1n_CI", "Pre-1o", "Pre-1o_CI",
                  "Rn", "Rn_CI", "Ro", "Ro_CI",
                  "PRn", "PRn_CI", "PRo", "PRo_CI")
    
    cbind(names(ma), newNames) # check that classes are relabled properly. Yup
    
    names(ma) <- newNames
    str(ma)
    write.csv(ma,'./output/931PopMales_Apx_17.csv')    
