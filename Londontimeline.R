
#PACKAGES IMPORT====
library(ggpubr)
library(ggplot2)
library(ggExtra)
library(scales)
library(tidyr)
library(dplyr)
library(plyr)
library(openair)
library(devtools)
library(tidyverse)
library(data.table)



#DATASETS SORTING======================================================================================
##MARYLEBONE ROAD //MY1 | Westminster - Marylebone Road | Kerbside====

MY <- importAURN(site = "MY1", meta=TRUE, hc=TRUE, year=c(2018, 2017, 2016, 2015, 
                                              2014))

#import hourly dataframes for all years, all months, using the function importAURN (openair)
#includes metadata (met=TRUE) for station coordinates and hydrocarbons measurements (hc=TRUE) to retrive surface observation of air temperature

MY$date <- as.POSIXct(MY$date, format = "%d/%m/%Y %H:%M" )

#change date format to POSIXct to use scale_x_datetime

MY18 <- MY[MY$date %like% "2018-01", ]
MY17 <- MY[MY$date %like% "2017-01", ]
MY16 <- MY[MY$date %like% "2016-01", ]
MY15 <- MY[MY$date %like% "2015-01", ]
MY14 <- MY[MY$date %like% "2014-01", ]


#create new datasets for each year with data from January only

#AVERAGES MARYLEBONE ROAD====

AVGlistMY <- list(MY17=MY17, MY16=MY16, MY15=MY15, MY14=MY14)

#creating a list with all measurements from all years of the long-term (2005-2016)

AVGsMY <- lapply(AVGlistMY, function(x){
  x[c("no2", "ws", "wd", "temp")]
})

#create an object with no2 concentration, wind speed, wind direction and temperature from all years

AVGsMYdf <- as.data.frame(unlist(AVGsMY, recursive = F))

#transfroming it into a dataframe

AVGsMYdf$AVGNO2 <- rowMeans(AVGsMYdf[,grep("no2", names(AVGsMYdf))], na.rm =TRUE)

#create a column (AVGNO2) with the mean across rows of all the columns containing no2, 


AVGsMYdf$AVGWS <- rowMeans(AVGsMYdf[,grep("ws", names(AVGsMYdf))], na.rm =TRUE)

AVGsMYdf$AVGWD <- rowMeans(AVGsMYdf[,grep("wd", names(AVGsMYdf))], na.rm =TRUE)

AVGsMYdf$AVGTEMP <- rowMeans(AVGsMYdf[,grep("temp", names(AVGsMYdf))], na.rm =TRUE)

#repeat for ws (wind speed), wd (wind direction), temp (ambient temperature)

#create a new object with all the averages of ws, wd, temp, taking the date from MY18 

AVGsMY <- AVGsMYdf %>%
  select(AVGNO2, AVGWS, AVGWD, AVGTEMP) %>%
  #create a dataframe with all the averages calculated above
  cbind(.,MY18$date) %>%
  #add the date column for 2018 (in order to plot) from MY18
  dplyr::rename("date" = `MY18$date`) %>%
  #rename the added date column from "MY18$date" to "date"
  dplyr::rename("ws" = `AVGWS`) %>%
  #rename the column of the average wind speed "AVGWS" to "ws" 
  #in order to use the data with openair functions
  dplyr::rename("wd" = `AVGWD`) %>%
  dplyr::rename("no2" = `AVGNO2`) %>%
  dplyr::rename("temp" = `AVGTEMP`)




###BRIXTON ROAD //LB4 | Lambeth - Brixton Road | Kerbside====

LB <- importKCL(
  site = "LB4",
  year = c(2018, 2017, 2016, 2015, 
           2014), 
  pollutant = "all",
  met = TRUE,
  units = "mass",
  extra = FALSE,
  meta = TRUE,
  to_narrow = FALSE
)

#import hourly dataset using another function -> importKCL(openair)

LB$date <- as.POSIXct(LB$date, format = "%d/%m/%Y %H:%M" )

LB18 <- LB[LB$date %like% "2018-01", ]
LB17 <- LB[LB$date %like% "2017-01", ]
LB16 <- LB[LB$date %like% "2016-01", ]
LB15 <- LB[LB$date %like% "2015-01", ]
LB14 <- LB[LB$date %like% "2014-01", ]



#AVERAGE NO2 BRIXTON ROAD====

#calculating average no2 concentration for Brixton road.

AVGlistLB <- list(LB17=LB17, LB16=LB16, LB15=LB15, LB14=LB14)

# creating a list of dataframes for all years to calculate the average

AVGlistLB <- lapply(AVGlistLB, function(x){
  x[c("no2")]
})

AVGsLBdf <- as.data.frame(unlist(AVGlistLB, recursive = F))

AVGsLBdf$AVGNO2 <- rowMeans(AVGsLBdf[,1:4], na.rm =TRUE)

#calculating the mean across rows (years 14-17) for no2

AVGsLB <- AVGsLBdf %>%
  select(AVGNO2) %>%
  cbind(.,LB18$date) %>%
  dplyr::rename("date" = `LB18$date`) %>%
  dplyr::rename("no2" = `AVGNO2`)

#creating a datafrrame containing date from 2018 and average no2 

#DATASET FOR PLOTTING NO2TIMELBR====

NO2TIMELBR <- AVGsLB %>%
  select(no2, date) %>%
  cbind(.,LB18$no2) %>%
  dplyr::rename("no2avg" = `no2`) %>%
  dplyr::rename("no218" = `LB18$no2`)

#creating a dataframe with no2 from 2018 and no2 average (years 2014-2017)

#DATASET FOR PLOTTING WSAVG18====

WSWDTAVG18 <- AVGsMY %>%
  select(date, ws, wd, temp) %>%
  cbind(.,MY18$wd, MY18$ws, MY18$temp) %>%
  dplyr::rename("wsavg" = `ws`) %>%
  dplyr::rename("wdavg" = `wd`) %>%
  dplyr::rename("tempavg" = `temp`) %>%
  dplyr::rename("wd18" = `MY18$wd`) %>%
  dplyr::rename("ws18" = `MY18$ws`) %>%
  dplyr::rename("temp18" = `MY18$temp`)

# creating a new dataset with averages of ws, wd and temp from Marylebone road (14-17), 
# and ws, wd and temp for 2018

#####BACKGROUND STATIONS======

"In this section, no2 timeline is comapred between 3 reference stations investigate wether the changes
in no2 concentrations are due to local factors or similar trends confirm that no2 was transported to London."

####URBAN BACKGROUND // CLL2 London Bloomsbury -0.125889 51.52229 London Borough of Camden==== 

CL<- importAURN(site = "CLL2", hc=TRUE, year=c(2018))
#creating a dataframe importing air pollutants concentration, including hydrocarbons to obtain ws, wd and temp

CL$date <- as.POSIXct(CL$date, format = "%d/%m/%Y %H:%M" )
#converting date format

CL18 <- CL[CL$date %like% "2018-01", ]
#selecting only the month of January 2018
# the process is repeated for another 2 stations below

#####SUBURBAN BACKGROUND // LON6 London Eltham 0.070842 51.45266 London Borough of Greenwich====

LON<- importAURN(site = "LON6", hc=TRUE, year=c(2018))

LON$date <- as.POSIXct(LON$date, format = "%d/%m/%Y %H:%M" )

LON18 <- LON[LON$date %like% "2018-01", ]

####

######RURAL BACKGROUND // ROCH Rochester Stoke 0.634889 51.45617 Medway ====
####

ROCH<- importAURN(site = "ROCH", hc=TRUE, year=c(2018))

ROCH$date <- as.POSIXct(ROCH$date, format = "%d/%m/%Y %H:%M" )

ROCH18 <- ROCH[ROCH$date %like% "2018-01", ]

#TIMELINES=============================================================================================
#SET DESIRED APPEARANCE OF PLOT AXIS=======================

lims <- as.POSIXct(strptime(c("2018-01-05 00:00","2018-01-09 00:00"), format = "%Y-%m-%d %H:%M"))

#set = labels for last plot (with visible dates)

scal <- scale_x_datetime(labels = date_format("%d %b"), 
                         breaks = "1 day",
                         expand = c(0,0))
                        

scalb <- scale_x_datetime(labels = date_format("%d %b %H"), 
                         breaks = "1 day",
                         expand = c(0,0),
                         limits = lims)
                         
#scal =  for all plots apart from last to show only ticks and not labels

set <- scale_x_datetime(labels = date_format(" "), 
                        breaks = "1 day",
                        expand = c(0,0))
#personalised theme (plot background)

theme2 <- theme(axis.text.x = element_text(angle = 45, hjust = 1),
                #tilt x axis 45 degrees
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(colour = "gray", size = 0.5),
                #set grid to be gray and desired size
                panel.background = element_rect(fill = "white"),
                #white filling in the cells of the grid
                panel.border = element_rect(colour = "black", fill = NA, size = 0.5 ),
                #set border of the panel as a black line ,
                legend.title = element_blank())
#no legend title

#NO2 TIMELINE BRIXTON ROAD 2018 + AVG====

lbavg18 <- NO2TIMELBR %>%
  select(date, no2avg, no218) %>%
  gather(key = "variable", value= "value", -date)
#preparing a dataset with no2 concentration from brixon road from the average and for 2018 

# creating a plot as an object in order to merge different plots in a timeline
lbavg18p <- 
  ggplot(lbavg18, aes(x = date, y = value, color = variable, fill = variable)) +
  # setting the aestethics for ggplot 
  geom_area(alpha = 0.3, position = "identity")+
  #timeline with shaded area (setting 0.3 for fade)
  scale_color_manual(values = c("#00008B", "#8B2323"), guide = FALSE) + 
  #chosing the colout and no legend title
  scale_fill_manual(labels = c("2018", "Climatology"), values = c("#00008B", "#8B2323"))+
  #allocating the legend for 2018 and average 
  xlab(" ")+
  #no labels on the x axis "black"
  ylab(expression(NO["2"]*" "* ~ (µg/m["3\n"]))) +
  #preparing label for X axis (NO2 with "2" as a subscript and units)
  scal +
  #calling function "scal"
  geom_hline(yintercept = 200, color='red', size=1) +
  #add a red horizontal line that intercepts y at the value 200, to display when the no2 pollution limit was exceeded
  geom_vline(xintercept=as.numeric(LB18$date[360])) +
  #dividing the x axis in JF1 from 00:00 01/01/2018 to 23:00 15/01/2018 (line 0-360),
  #and JF2 from 23:00 15/01/2018 to 23:00 31/01/2018 (line 361-744)
  geom_point()+
  #adding points to highlight each hourly measurement
  theme2 +
  #calling function "theme 2"
  scale_y_continuous(breaks = seq(0,300, by = 20),
                     labels = c(0, rep("",2), 60, rep("",2), 120, rep("",2), 
                                180, rep("", 2), 240, rep("", 2), 300),
                     limits = c(0,300), expand = c(0,0))
 # setting custom ticks and labels on x axis, from 0 to 300, with ticks every 20 units and label every 60 units


#WS TIMELINE 18+AVG====

  wsavg18 <- WSWDTAVG18 %>%
    select(date, wsavg, ws18) %>%
    gather(key = "variable", value= "value", -date)
  
  wsavg18p <- 
  ggplot(wsavg18, aes(x = date, y = value, color = variable, fill = variable)) +
    geom_area(alpha = 0.3, position = "identity")+
    scale_color_manual(values = c("#030303", "#8B6914"), guide = FALSE) + 
    scale_fill_manual(labels = c("2018", "Climatology"), values = c("#030303", "#8B6914"))+
    xlab(" ")+
    ylab(expression("Wind speed (m/s)")) +
    scal +
    geom_vline(xintercept=as.numeric(WSWDTAVG18$date[360])) +
    geom_point()+
    theme2 +
    scale_y_continuous(breaks = seq(0,15, by = 3),
                       limits = c(0,15), expand = c(0,0))
#WD TIMELINE 18+AVG======
  
  wdavg18 <- WSWDTAVG18 %>%
    select(date, wdavg, wd18) %>%
    gather(key = "variable", value= "value", -date)
  
  wdavg18p <- 
  ggplot(wdavg18, aes(x = date, y = value, color = variable, fill = variable)) +
    geom_area(alpha = 0.3, position = "identity")+
    scale_color_manual(values = c("#008B00", "#8B8B00"), guide = FALSE) + 
    scale_fill_manual(labels = c("2018", "Climatology"), values = c("#008B00", "#8B8B00"))+
    xlab(" ")+
    ylab(expression("Wind direction (°)")) +
    scal +
    geom_vline(xintercept=as.numeric(WSWDTAVG18$date[360])) +
    geom_point()+
    theme2 +
    scale_y_continuous(breaks = seq(0,360, by = 45),
                       limits = c(0,360), expand = c(0,0))
  
#TEMPERATURE TIMELINE 18+AVG======
  
  tavg18 <- WSWDTAVG18 %>%
    select(date, tempavg, temp18) %>%
    gather(key = "variable", value= "value", -date)
  
  tavg18p <- 
  ggplot(tavg18, aes(x = date, y = value, color = variable, fill = variable)) +
    geom_area(alpha = 0.3, position = "identity")+
    scale_color_manual(values = c("#458B74", "#7A378B"), guide = FALSE) + 
    scale_fill_manual(labels = c("2018", "Climatology"), values = c("#458B74", "#7A378B"))+
    xlab(" ")+
    ylab(expression("Temperature (°C)")) +
    scal +
    geom_vline(xintercept=as.numeric(WSWDTAVG18$date[360])) +
    geom_point()+
    theme2 +
    scale_y_continuous(breaks = seq(-5,15, by = 3),
                       limits = c(-5,15), expand = c(0,0))

#KERBSIDE BACKGROUND NO2 CONCENTRATION====

  b18p <-
    ggplot(LB18, aes(x = date, y = no2)) +
    geom_area(alpha = 0.3, position = "identity", color="#CD3333", fill="#CD3333")+
    geom_point() +
    geom_hline(yintercept = 200, color='red', size=1) +
    xlab(" ")+
    ylab(expression(NO["2"]*" "* ~ (µg/m["3\n"]))) +
    scal +
    geom_vline(xintercept=as.numeric(LB18$date[360])) +
    theme2 +
    scale_y_continuous(breaks = seq(0,300, by = 20),
                       labels = c(0, rep("",2), 60, rep("",2), 120, rep("",2), 
                                  180, rep("", 2), 240, rep("", 2), 300),
                       limits = c(0,300), expand = c(0,0))
  
#URBAN BACKGROUND NO2 CONCENTRATION====

  CL18p <-
    ggplot(CL18, aes(x = date, y = no2)) +
    geom_area(alpha = 0.3, position = "identity", color="#00008B", fill="#00008B")+
    geom_point() +
    geom_hline(yintercept = 200, color='red', size=1) +
    xlab(" ")+
    ylab(expression(NO["2"]*" "* ~ (µg/m["3\n"]))) +
    scal +
    geom_vline(xintercept=as.numeric(CL18$date[360])) +
    theme2 +
    scale_y_continuous(breaks = seq(0, 135, by = 15),
                       labels = c(0, rep("",2), 45, rep("",2), 90, rep("",2), 
                                  135),
                       limits = c(0,135), expand = c(0,0))
#SUBURBAN# BACKGROUND NO2 CONCENTRATION====               
  
  LON18p <-
    ggplot(LON18, aes(x = date, y = no2)) +
    geom_area(alpha = 0.3, position = "identity", color="#68228B", fill="#68228B")+
    geom_point() +
    geom_hline(yintercept = 200, color='red', size=1) +
    xlab(" ")+
    ylab(expression(NO["2"]*" "* ~ (µg/m["3\n"]))) +
    scal +
    geom_vline(xintercept=as.numeric(LON18$date[360])) +
    theme2 +
    scale_y_continuous(breaks = seq(0,60, by = 10),
                       labels = c(0, rep("",2), 30, rep("",2), 60),
                       limits = c(0,60), expand = c(0,0))
  
#RURAL BACKGROUND NO2 CONCENTRATION====
  
  ROCH18p <-
    ggplot(ROCH18, aes(x = date, y = no2)) +
    geom_area(alpha = 0.3, position = "identity", color="#006400", fill="#006400")+
    geom_point() +
    geom_hline(yintercept = 200, color='red', size=1) +
    xlab(" ")+
    ylab(expression(NO["2"]*" "* ~ (µg/m["3\n"]))) +
    scal +
    geom_vline(xintercept=as.numeric(ROCH18$date[360])) +
    theme2 +
    scale_y_continuous(breaks = seq(0,60, by = 10),
                       labels = c(0, rep("",2), 30, rep("",2), 60),
                       limits = c(0,60), expand = c(0,0))
#Nox
#METEOCHECK====
  
  
  wsmyche <-
    ggplot(MY18, aes(x = date, y = ws)) +
    geom_area(alpha = 0.3, position = "identity", color="#030303", fill="#030303")+
    geom_point() +
    xlab(" ")+
    ylab(expression("Wind speed (m/s)my")) +
    scalb +
    geom_vline(xintercept=as.numeric(MY18$date[121])) +
    geom_vline(xintercept=as.numeric(MY18$date[145])) +
    theme2 +
    scale_y_continuous(breaks = seq(0,15, by = 3),
                       limits = c(0,15), expand = c(0,0))
  
  wdmyche <-
    ggplot(MY18, aes(x = date, y = wd)) +
    geom_area(alpha = 0.3, position = "identity", color="#030303", fill="#030303")+
    geom_point() +
    xlab(" ")+
    ylab(expression("Wdmy")) +
    scalb +
    geom_vline(xintercept=as.numeric(MY18$date[121])) +
    geom_vline(xintercept=as.numeric(MY18$date[145])) +
    theme2 +
    scale_y_continuous(breaks = seq(0,360, by = 45),
                       limits = c(0,360), expand = c(0,0))
  
  wsrochche <-
    ggplot(ROCH18, aes(x = date, y = ws)) +
    geom_area(alpha = 0.3, position = "identity", color="#006400", fill="#006400")+
    geom_point() +
    geom_hline(yintercept = 200, color='red', size=1) +
    xlab(" ")+
    ylab("wsrur") +
    scalb +
    geom_vline(xintercept=as.numeric(MY18$date[121])) +
    geom_vline(xintercept=as.numeric(MY18$date[145])) +
    theme2 +
    scale_y_continuous(breaks = seq(0,15, by = 3),
                       #labels = c(0, rep("",2), 30, rep("",2), 60),
                       limits = c(0,15), expand = c(0,0))
  
  wdrochche <-
    ggplot(ROCH18, aes(x = date, y = wd)) +
    geom_area(alpha = 0.3, position = "identity", color="#006400", fill="#006400")+
    geom_point() +
    xlab(" ")+
    ylab("wdrur") +
    scalb +
    geom_vline(xintercept=as.numeric(MY18$date[121])) +
    geom_vline(xintercept=as.numeric(MY18$date[145])) +
    theme2 +
    scale_y_continuous(breaks = seq(0,360, by = 45),
                       #labels = c(0, rep("",2), 30, rep("",2), 60),
                       limits = c(0,360), expand = c(0,0))
 
  (panelws <- ggarrange(wsmyche, wsrochche, wdmyche, wdrochche, ncol = 1))
#TIMELINE PANELS=======================================================================================
#TIMELINE PANEL FOR ANALYSIS======
  (analysispanel <- ggarrange(lbavg18p, wdavg18p, wsavg18p, tavg18p, ncol = 1,
                              labels = (c("A\n", "B\n", "C\n", "D\n")),
                              vjust = c(0.9,0.9,0.9,0.9)))
  
#TIMELINE PANEL FOR LRT====
  (lrtpanel <- ggarrange(b18p, CL18p, LON18p, ROCH18p, ncol = 1,
  labels = (c("KBS\n", "UBB\n", "SUB\n", "RUR\n")),
  vjust = c(0.4,0.4,0.4,0.4),
  hjust = c(-0, -0, -0, -0)))
  
 
#TIMELINE PANEL 2017====
  
  (panel_17 <- ggarrange(no217p, wd17p, ws17p, t17p, ncol = 1))
#KERNELEXCEED
"The kernel exceed function was used to investigate the wind speed and direction during high pollution events, 
  for additional information see Beijing_timeline"
  

lb188<- NO2TIMELBR %>%
  select(no2avg, no218) %>%
  cbind(.,WSWDTAVG18$wsavg, WSWDTAVG18$ws18, WSWDTAVG18$wdavg, WSWDTAVG18$wd18, LB18$date)
  #creating new datafram with wind speed, wind direction and no2 concentration for 2018 and average (2014- 2017)

LBf1 <- lb188[1:360,]
LBf2 <- lb188[361:744,]

"dividing January datasets in 1st fortnight ( from 00:00 1/1 to 23:00 15/01 and 16/01 ) and second 
fortnight (from 00:00 16/01 to 23:00 31/01 )"


LBf118 <- LBf1 %>%
  dplyr::rename("date" = `LB18$date`) %>%
  dplyr::rename("ws" = `WSWDTAVG18$ws18`) %>%
  dplyr::rename("wd" = `WSWDTAVG18$wd18`) %>%
  dplyr::rename("no2" = `no218`) 

#renaming the variables to "ws", "wd" and "no2" in order to use the kernelexceed function.

LBf218 <- LBf2 %>%
  dplyr::rename("date" = `LB18$date`) %>%
  dplyr::rename("ws" = `WSWDTAVG18$ws18`) %>%
  dplyr::rename("wd" = `WSWDTAVG18$wd18`) %>%
  dplyr::rename("no2" = `no218`) 
  
LBfav118 <- LBf1 %>%
  dplyr::rename("date" = `LB18$date`) %>%
  dplyr::rename("ws" = `WSWDTAVG18$wsavg`) %>%
  dplyr::rename("wd" = `WSWDTAVG18$wdavg`) %>%
  dplyr::rename("no2" = `no2avg`)

LBfav218 <- LBf2 %>%
  dplyr::rename("date" = `LB18$date`) %>%
  dplyr::rename("ws" = `WSWDTAVG18$wsavg`) %>%
  dplyr::rename("wd" = `WSWDTAVG18$wdavg`) %>%
  dplyr::rename("no2" = `no2avg`)


kernelExceed(
  LBf118,
  x = "wd",
  y = "ws",
  pollutant = "no2", by = "hour", limit = 200, xlim = c(10, 360), col = "hue", colorkey = TRUE) 

#setting limits to investigate hourly exceedance of NO2 EU limits of 200 (ug/m3)

kernelExceed(
  LBf218,
  x = "wd",
  y = "ws",
  pollutant = "no2", by = "hour", limit = 200, xlim = c(10, 360), col = "hue", colorkey = TRUE)
kernelExceed(
  LBfav118,
  x = "wd",
  y = "ws",
  pollutant = "no2", by = "hour", limit = 200, xlim = c(10, 360), col = "hue", colorkey = TRUE)

kernelExceed(
  LBfav218,
  x = "wd",
  y = "ws",
  pollutant = "no2", by = "hour", limit = 200, xlim = c(10, 360), col = "hue", colorkey = TRUE)
