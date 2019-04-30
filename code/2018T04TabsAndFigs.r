# modified for 2018 kachemak survey summary, final 190305
#Format tables for 2017 Kachemak Survey SUmmary, mstly from 931_popEstAndCPUE_161101.R used for 2016 report

#LOAD ----
library(tidyverse)
library(stats)
library(plotrix)
library (Hmisc)  ## summarize conflicts with dplyr 
library (scales)
library (gridExtra)
options (scipen = 10)
library(extrafont)
font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())+ 
            theme(legend.title = element_blank()))
dat_17 <- read.csv('./data/qP_simp_17_190301.csv') # using new size classes from 2017 SD op-plan
dat_old <- read.csv('./data/qP_simp_oldSCs_190301.csv')# using the old pre-2017 size classes 
events <- read.csv('./data/events_190304.csv')
events %>% filter (PROJECT_CODE == 'T04', GEAR_PERFORMANCE_CODE == '1') %>%
  select( Event = EVENT_ID,
          year = YEAR,
          Project = PROJECT_CODE, 
          length=TOW_LENGTH_DESIGNATED,
          totCatch = CATCH_WEIGHT) -> event
awl <- read.csv('./data/awl_shellfish_190301.csv') 

## Males Main ----
# New Size Classes
  dat_17 %>% filter (PROJECT_CODE == "T04") %>% 
    transmute(Year = YEAR,Tows = n,
              'Pre-4' =  MT10_P_,
              'Pre-3' =  MT9_P_,  
              'Pre-2' = MT7_P_ + MT8_P_, 
              'Pre-1' = MT5_P_ + MT6_P_, 
              'LM'    = LM_P_, 'LM_CI' = LM_P_CI_,  
              'TM'    = TM_P_, 'TM_CI' = TM_P_CI_) -> m_17
  #write.csv(m_17,'./output/931PopMales_Main_17.csv') 
#Old Size Classes
  dat_old %>% filter (PROJECT_CODE == "T04") %>% 
    transmute(Year = YEAR,Tows = n,
              'Pre-4' =  MT10_P_,
              'Pre-3' =  MT9_P_,  
              'Pre-2' = MT7_P_ + MT8_P_, 
              'Pre-1' = MT5_P_ + MT6_P_, 
              'LM'    = LM_P_, 'LM_CI' = LM_P_CI_,  
              'TM'    = TM_P_, 'TM_CI' = TM_P_CI_) -> m_old
  #write.csv(m_old,'./output/931PopMales_Main_old.csv') 
# Composite table 
m_17  %>% left_join (m_old %>% select (Year, LM_140 = LM, LM_140_CI = LM_CI)) %>% # join LM from old SC to abund table using new SCs 
  select(-c(TM,TM_CI), everything())->m_comp # move totals to end 
#write.csv(m_comp, './output/931PopMales_Main_comp.csv')  # this uses new size classes for all except LM_140. 

## Females Main ----
dat_17 %>% filter (PROJECT_CODE == 'T04') %>% select(year = YEAR, tows = n,
                    FT11_P_, FT11_P_CI_, MF_P_, MF_P_CI_, TF_P_, TF_P_CI_) -> f
  
  #write.csv(f,'./output/931PopFems_Main.csv')

##  Catch by Station (per Carol request) ----
read.csv('./data/C_17_190301.csv') %>%
  right_join(event, by = c('EVENT_ID' = 'Event')) %>% # limited to good tows at top
  filter (YEAR == 2018) %>% transmute(
    Station = STATION_ID,
    nmi = length,
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
  mutate_if(is.numeric, funs(as.character(formatC(round(., 2),2,format = "f")))) -> c

  write.csv(c,'./output/2018T04_931CatchByStation_17sc.csv') 
  # previously a version of this from SQL, emailed to KG.  

## CPUE by station (per KG request 180130, not incorporated to 2017 rmd) ----
  read.csv('./data/C_17_190301.csv') %>%  right_join(event, by = c('EVENT_ID' = 'Event')) %>% # limited to good tows at top
    filter  (YEAR == 2018) %>% transmute(
    Station = STATION_ID, 
    length = length, 
    Sublegal = (MT5_T + MT6_T + MT7_T + MT8_T + MT9_T + MT10_T)/length, 
    Legal = LM_T/length, 
    Tot_males = TM_T/length, 
    Juvenile_fems = FT11_T/length, 
    Mature_fems = (FT12_T + FT13_T)/length, 
    Tot_fems = TF_T/length) %>% arrange (Station) -> cpm 
  
  write.csv(cpm ,'./output/2018T04_931CPUEByStation_17sc.csv') 
  
  # calc ranges and cv for KG 
  events %>% filter (PROJECT_CODE == 'T04', YEAR == 2018, USED_IN_ESTIMATE == 'YES') %>% left_join(cpm, by= c("STATION_ID" = "Station")) %>% # exclude 139
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
# NEW size classes   
  dat_17 %>% filter (PROJECT_CODE == "T04") %>%
    transmute ("proj" = PROJECT_CODE, "yr" = YEAR,
           "LM_P" = LM_P_ / 1000, # convert to thousands of crab 
           "LM_P_CI" = LM_P_CI_ / 1000) %>%
  
  ggplot (aes(x = yr, y = LM_P,
              ymin = ifelse((LM_P - LM_P_CI) > 0 , (LM_P - LM_P_CI), 0),
              ymax = LM_P + LM_P_CI)) + 
          geom_pointrange() + 
          scale_x_continuous(breaks = seq(1990,2018,1)) + 
          scale_y_continuous(label = comma, breaks = seq(0,3000,1000), limits = c(0,3100)) + 
          labs( x = "Year", y = "Thousands of crab") + 
          theme( axis.text.x = element_text(angle=90, vjust= 0)) -> L17
    
  L17 %>% ggsave(file = './figs/T04LM_2017SCs.png', dpi=300, height=8.5, width=6.5, units="in" )

# OLD size classes   
  dat_old %>% filter (PROJECT_CODE == "T04") %>%
    transmute ("proj" = PROJECT_CODE, "yr" = YEAR,
               "LM_P" = LM_P_ / 1000, # convert to thousands of crab 
               "LM_P_CI" = LM_P_CI_ / 1000) %>%
    
    ggplot (aes(x = yr, y = LM_P,
                ymin = ifelse((LM_P - LM_P_CI) > 0 , (LM_P - LM_P_CI), 0),
                ymax = LM_P + LM_P_CI)) + 
    geom_pointrange() +  
    scale_x_continuous(breaks = seq(1990,2018,1)) + 
    scale_y_continuous(label = comma, breaks = seq(0,3000,500)) + 
    labs( x = "Year", y = "Thousands of crab") + 
    theme( axis.text.x = element_text(angle=90, vjust= 0)) -> Lold
  
  Lold %>% ggsave(file = './figs/T04LM_OldSCs.png', dpi=300, height=8.5, width=6.5, units="in" )
  

## DUNGY CPUE ----
  dat <- read.csv("./data/qP_910_190301.csv")
  
  dat %>% select ("PROJECT_CODE", "YEAR","n","SM_CBar","SM_varC", "LM_CBar","LM_varC","TM_CBar","TM_varC",
                "TF_CBar","TF_varC") -> dat
  
  ##Calc sample SDs and SEM
  dat %>% mutate(
    SM_SD = SM_varC^.5,
    LM_SD = LM_varC^.5,
    TM_SD = TM_varC^.5,
    TF_SD = TF_varC^.5, 
    SM_SE = SM_SD/(n^.5),
    LM_SE = LM_SD/(n^.5),
    TM_SE = TM_SD/(n^.5),
    TF_SE = TF_SD/(n^.5)) %>%
  
  #remove extra cols, reorder and rename 
  select("Proj" = PROJECT_CODE, "Year" = YEAR, n,
                    SM_CBar, SM_SE, LM_CBar, LM_SE, TM_CBar, TM_SE, TF_CBar, TF_SE) %>%
  arrange(Proj, Year) %>% 
  filter (Proj == "T04") -> dung_pm 
  write.csv(dung_pm, "./output/910_cpm_190301.csv") # note previously included SD instead of SE
  # Plot TM
  dung_pm %>%  
  ggplot (aes(x = Year, y = TM_CBar,
              ymin = ifelse((TM_CBar - TM_SE) > 0, (TM_CBar - TM_SE), 0),
              ymax = TM_CBar + TM_SE)) + 
    geom_pointrange() + 
    scale_x_continuous(breaks = seq(1990,2018,1)) + 
    labs( x = "Year", y = "CPUE (crab/nmi)", title = 'Total Males') + 
    theme( axis.text.x = element_text(angle=90, vjust= 0)) -> D_TM
    D_TM
  # Plot LM
    dung_pm %>%   
      ggplot (aes(x = Year, y = LM_CBar,
                  ymin = ifelse((LM_CBar - LM_SE) > 0, (LM_CBar - LM_SE), 0),
                  ymax = LM_CBar + LM_SE)) + 
      geom_pointrange() + 
      scale_x_continuous(breaks = seq(1990,2018,1)) + 
      labs( x = "Year", y = "CPUE (crab/nmi)", title = 'Legal Males') + 
      theme( axis.text.x = element_text(angle=90, vjust= 0)) -> D_LM
    D_LM
  # Plot TF
    dung_pm %>%  
      ggplot (aes(x = Year, y = TF_CBar,
                  ymin = ifelse((TF_CBar - TF_SE) > 0, (TF_CBar - TF_SE), 0),
                  ymax = TF_CBar + TF_SE)) + 
      geom_pointrange() + 
      scale_x_continuous(breaks = seq(1990,2018,1)) + 
      labs( x = "Year", y = "CPUE (crab/nmi)", title = 'Total Females') + 
      theme( axis.text.x = element_text(angle=90, vjust= 0)) -> D_TF
    D_TF 
    dungyCPM <-arrangeGrob(D_TM,D_LM,D_TF, ncol=1)
    dungyCPM %>% ggsave(file = "./figs/T04_910_cpm.png", dpi=300, height=8.5, width=6.5, units="in")    

## RKC CPUE (added 190430) ----
  dat <- read.csv("./data/qP_921_190430.csv")
  
  dat %>% select ("PROJECT_CODE", "YEAR","n","SM_CBar","SM_varC", "LM_CBar","LM_varC","TM_CBar","TM_varC",
                  "TF_CBar","TF_varC") -> dat
  
  ##Calc sample SDs and SEM
  dat %>% mutate(
    SM_SD = SM_varC^.5,
    LM_SD = LM_varC^.5,
    TM_SD = TM_varC^.5,
    TF_SD = TF_varC^.5, 
    SM_SE = SM_SD/(n^.5),
    LM_SE = LM_SD/(n^.5),
    TM_SE = TM_SD/(n^.5),
    TF_SE = TF_SD/(n^.5)) %>%
    
    #remove extra cols, reorder and rename 
    select("Proj" = PROJECT_CODE, "Year" = YEAR, n,
           SM_CBar, SM_SE, LM_CBar, LM_SE, TM_CBar, TM_SE, TF_CBar, TF_SE) %>%
    arrange(Proj, Year) %>% 
    filter (Proj == "T04") -> rkc_pm 
  write.csv(rkc_pm, "./output/921_cpm_190430.csv") # note previously included SD instead of SE
  # Plot TM
  rkc_pm %>%  
    ggplot (aes(x = Year, y = TM_CBar,
                ymin = ifelse((TM_CBar - TM_SE) > 0, (TM_CBar - TM_SE), 0),
                ymax = TM_CBar + TM_SE)) + 
    geom_pointrange() + 
    scale_x_continuous(breaks = seq(1990,2018,1)) + 
    labs( x = "Year", y = "CPUE (crab/nmi)", title = 'Total Males') + 
    theme( axis.text.x = element_text(angle=90, vjust= 0)) -> RKC_TM
  RKC_TM
  # Plot LM
  rkc_pm %>%   
    ggplot (aes(x = Year, y = LM_CBar,
                ymin = ifelse((LM_CBar - LM_SE) > 0, (LM_CBar - LM_SE), 0),
                ymax = LM_CBar + LM_SE)) + 
    geom_pointrange() + 
    scale_x_continuous(breaks = seq(1990,2018,1)) + 
    labs( x = "Year", y = "CPUE (crab/nmi)", title = 'Legal Males') + 
    theme( axis.text.x = element_text(angle=90, vjust= 0)) -> RKC_LM
  RKC_LM
  # Plot TF
  rkc_pm %>%  
    ggplot (aes(x = Year, y = TF_CBar,
                ymin = ifelse((TF_CBar - TF_SE) > 0, (TF_CBar - TF_SE), 0),
                ymax = TF_CBar + TF_SE)) + 
    geom_pointrange() + 
    scale_x_continuous(breaks = seq(1990,2018,1)) + 
    labs( x = "Year", y = "CPUE (crab/nmi)", title = 'Total Females') + 
    theme( axis.text.x = element_text(angle=90, vjust= 0)) -> RKC_TF
  RKC_TF 
  rkcCPM <-arrangeGrob(RKC_TM,RKC_LM,RKC_TF, ncol=1)
  rkcCPM %>% ggsave(file = "./figs/T04_921_cpm.png", dpi=300, height=8.5, width=6.5, units="in")      
        
## Tanner CH vs CW plot ## ----
# 190305 for final vesrion decided to just keep previous figure.  SHould be be virtually unchanged folling nprb edits.  
# If i were to redo this figure should convert to ggplot and look at jz suggestion (exclued VO?)
read.csv ("./data/awl_shellfish_190301.csv") %>% 
filter(PROJECT_CODE == "T04", YEAR > 2008) -> awl # 2006 and 2007 had CH too, but excluding to fit on 6 panel plot
events %>% filter (USED_IN_ESTIMATE == 'YES') %>% select (EVENT_ID) %>% inner_join(awl) -> awl # limit to used in est following previous
    
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
  
# Clutch Tables ----  
# moved to external script revClutchTable_180702.r

# 931 abundance table with new and old split for Apendix ----
  dat_17 %>% filter (PROJECT_CODE == 'T04') %>% 
      select (Year = YEAR, 
            'Pre-4' =  MT10_P_, 'Pre-4_CI' = MT10_P_CI_, 
            'Pre-3' =  MT9_P_, 'Pre-3_CI'  = MT9_P_CI_, 
            'Pre-2n' = MT7_P_, 'Pre-2n_CI' = MT7_P_CI_,
            'Pre-2o' = MT8_P_, 'Pre-2o_CI' = MT8_P_CI_,
            'Pre-1n' = MT5_P_, 'Pre-1n_CI' = MT5_P_CI_,
            'Pre-1o' = MT6_P_, 'Pre-1o_CI' = MT6_P_CI_,
            'Rn'     = MT1_P_, 'Rn_CI'     = MT1_P_CI_, 
            'Ro'     = MT2_P_, 'Ro_CI'     = MT2_P_CI_, 
            'PRn'    = MT3_P_, 'PRn_CI'    = MT3_P_CI_, 
            'PRo'    = MT4_P_, 'PRo_CI'    = MT4_P_CI_) -> ma_17
    write.csv(ma_17,'./output/931PopMales_Apx_17.csv')             
    
    dat_old %>% filter (PROJECT_CODE == 'T04') %>% 
      select (Year = YEAR, 
              'Pre-4' =  MT10_P_, 'Pre-4_CI' = MT10_P_CI_, 
              'Pre-3' =  MT9_P_, 'Pre-3_CI'  = MT9_P_CI_, 
              'Pre-2n' = MT7_P_, 'Pre-2n_CI' = MT7_P_CI_,
              'Pre-2o' = MT8_P_, 'Pre-2o_CI' = MT8_P_CI_,
              'Pre-1n' = MT5_P_, 'Pre-1n_CI' = MT5_P_CI_,
              'Pre-1o' = MT6_P_, 'Pre-1o_CI' = MT6_P_CI_,
              'Rn'     = MT1_P_, 'Rn_CI'     = MT1_P_CI_, 
              'Ro'     = MT2_P_, 'Ro_CI'     = MT2_P_CI_, 
              'PRn'    = MT3_P_, 'PRn_CI'    = MT3_P_CI_, 
              'PRo'    = MT4_P_, 'PRo_CI'    = MT4_P_CI_) -> ma_old
    write.csv(ma_old,'./output/931PopMales_Apx_old.csv')    
