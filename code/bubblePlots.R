# make bubble plots of Tannner by year and length, 190308

# LOAD ----
set.seed(15343437) 
library (FSA)
library(tidyverse)
library(extrafont)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

read.csv('./data/events_190304.csv')%>% 
  filter (PROJECT_CODE == 'T04', USED_IN_ESTIMATE == "YES") %>%
  select( Event = EVENT_ID,
          year = YEAR,
          length=TOW_LENGTH_DESIGNATED) -> event
read.csv('./data/awl_shellfish_190301.csv') %>% 
  filter (SPECIES_CODE == '931', SEX_CODE == 1) %>% 
  select (Event = EVENT_ID, 
          bw = BIOLOGICAL_WIDTH_MM, 
          freq = FREQUENCY,
          type = SAMPLE_TYPE_CODE) -> awl 
read.csv('./data/C_17_190301.csv') %>% 
  transmute( Event = EVENT_ID,
          tm_1 = TM_A + TM_B,
          tm_2 = TM_C + TM_D) -> tot

awl %>% uncount (freq) -> awl # expand awl recs by freq - early years 1 rec represents >1 crab
awl %>% right_join (event) -> awl # filter awl by used in estimate  

##################################################################
# EXPAND LENGTHS BY EVENT ----
##################################################################
events <- unique(awl$Event)  
#i <- '2017T04040' # start w one event 

# Type I  ----
  dat_t1 <-data.frame(Event=factor(), bw = numeric()) #create empty df to hold final expanded awl 
  
  for(i in events){    
    Sys.sleep(0.05)
    print ( c("####",paste (i, "   START") , "####"))
    
    awl %>% filter (Event == i, type == 1) -> awl.i # select 1 tow from awl . Chanage for T2s
  
    m <- nrow(awl.i) #num measured
  
    t <- tot[tot$Event == i, "tm_1"] #tot caught . change for sub vs whole sample 
  
    eawl <- numeric(0) #create empty vectors for expanded size dists so that object exists even if no crab in it, prevents latter errors when combining
    
    #expand 
    if(t > 0 & m > 1 & t - m > 1)   # conditional to prevent terminal errors 
    {eawl <- expandLenFreq(awl.i$bw, w=.1, total = t, decimals = 1) } # mb used width of .1 mm for catch up report. 5mm caused error w T2s on 2013T04039
    eawl # note these are only the additional, not measured crabs.
    length(eawl) - (t - m) #compare num of expanded to tot caught minus measured.  Should = 0
    
    #compare hists.  
    par(mfrow= c(3,1))
    hist (awl.i$bw, breaks = seq(0,200,5), main  = c("measured", i), freq = T, col = 'red')
    hist (eawl, breaks = seq(0,200,5), main  = c("additional", i), freq = T, col = 'blue')
    
    #combine measured with additional lengths
    all <- c(awl.i$bw, eawl)
    
    #compare total measurements in expanded awls to tot cnt from CC. Difs should equal 0. 
    length(all) - t
    
    #assign event to vector of lenghts 
    r <- length(all)
    all.df <- data.frame(Event = as.factor(rep(i, r)),
                       bw = all)
    
    # compare hists to first 2
    hist (all.df$bw, breaks = seq(0,200,5), main  = c("combined", i), freq = T, col = 'purple')
    
    nrow(all.df) - t # compare num recs in expanded df to total from CC
    
    dat_t1 <- rbind(dat_t1,all.df) # append to main df for all events 
    
    Sys.sleep(0.1) 
    print ( c("####",paste (i, "   COMPLETE") , "####"))
    }

# Type II ----
  dat_t2 <-data.frame(Event=factor(), bw = numeric()) #create empty df to hold final expanded awl 
  
  for(i in events){    
    Sys.sleep(0.05)
    print ( c("####",paste (i, "   START") , "####"))
    
    awl %>% filter (Event == i, type == 2) -> awl.i # select 1 tow from awl . Chanage for T2s
    
    m <- nrow(awl.i) #num measured
    
    t <- tot[tot$Event == i, "tm_2"] #tot caught . change for sub vs whole sample 
    
    eawl <- numeric(0) #create empty vectors for expanded size dists so that object exists even if no crab in it, prevents latter errors when combining
    
    #expand 
    if(t > 0 & m > 1 & t - m > 1)   # conditional to prevent terminal errors 
    {eawl <- expandLenFreq(awl.i$bw, w=.1, total = t, decimals = 1) } # mb used width of .1 mm for catch up report. 5mm created error on event 2013T04039
    eawl # note these are only the additional, not measured crabs.
    length(eawl) - (t - m) #compare num of expanded to tot caught minus measured.  Should = 0
    
    #compare hists.  
    par(mfrow= c(3,1))
    hist (awl.i$bw, breaks = seq(0,200,5), main  = c("measured", i), freq = T, col = 'red')
    hist (eawl, breaks = seq(0,200,5), main  = c("additional", i), freq = T, col = 'blue')
    
    #combine measured with additional lengths
    all <- c(awl.i$bw, eawl)
    
    #compare total measurements in expanded awls to tot cnt from CC. Difs should equal 0. 
    length(all) - t
    
    #assign event to vector of lenghts 
    r <- length(all)
    all.df <- data.frame(Event = as.factor(rep(i, r)),
                         bw = all)
    
    # compare hists to first 2
    hist (all.df$bw, breaks = seq(0,200,5), main  = c("combined", i), freq = T, col = 'purple')
    
    nrow(all.df) - t # compare num recs in expanded df to total from CC
    
    dat_t2 <- rbind(dat_t2,all.df) # append to main df for all events 
    
    Sys.sleep(0.1) 
    print ( c("####",paste (i, "   COMPLETE") , "####"))
    } 
  
rbind (dat_t1, dat_t2)  -> dat_comb
#write.csv (dat_comb,"output/expanded931male_Lengths.csv")

## ERROR CHECKING ----                                                                                                  
  #compare totals
  nrow(dat_comb) 
  sum(tot %>% right_join (event) %>% select (tm_1, tm_2)) # 32 fewer on expanded than sum of tot, presumably rounding or simialar
  
  ## compare histograms of awl to dat (combined measured + expanded)
  library (lattice)
  par(mfcol = c(2,1))
  # all
  hist (~ bw , data = awl, breaks = seq(0,200,1), main  = c("measured", "all tows"), freq = T, col = 'red' )
  hist (~ bw , data = dat_comb, breaks = seq(0,200,1), main  = c("expanded", "all tows"), freq = T, col = 'blue')

#############################################################################################################################
## bubble plot from expanded awl  ----   
#############################################################################################################################  
dat <-   read.csv ("output/expanded931male_Lengths.csv")
dat$bw %>% cut_interval (width = 5, boundary = 0, labels = F, n= 40 ) -> dat$bin # bin
dat$cp <- (dat$bin) *5 + 2.5
# join tow lengths 
dat %>% left_join (event) -> dat
dat%>% group_by(Event, bin) %>% summarize (year = first(year),
                                           cnt = n(), 
                                           towLength = first(length), 
                                           cpm = cnt/ towLength,
                                           cp = first(cp))   -> cpmByBinTow

cpmByBinTow %>% group_by (year, cp) %>% summarize (cpm = mean(cpm)) -> cpmByBinYear

ggplot(cpmByBinYear, aes(x= year, y = cp, size= cpm ))+ 
  geom_point(pch = 1) + 
  scale_x_continuous(breaks = seq(1990,2018,1)) +
  scale_y_continuous(breaks = seq(10,200,10)) +
  #scale_size_continuous (range =c(0.001,7)) +
  scale_size (range = c(0,16), name = "Crab per nmi") + # max range adjusted from 9 in 2017T04 to accomadate huge 2018 tow
  theme( axis.text.x = element_text(angle=90, vjust= 0), legend.position = "right") + 
  labs (x = 'Year', y = 'Carapace width (mm)' ) + 
  geom_hline (aes(yintercept = 140, linetype = "solid"), show.legend = F) + 
  geom_hline (aes(yintercept = 114, linetype = "dotted"), show.legend = F) -> bub
  bub
bub %>% ggsave(file =  './figs/bubble_m931_T04.png', dpi = 300, width = 9, height = 5.5, units = "in")

