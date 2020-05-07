####BEIJING======

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

#DATASET SORTING====

setwd("C:/Users/User/Desktop/ALLana/Pollution_data_FINALISSIMO/Pollution_data_READY/PLOTS/Beijing")

AO <- read.csv("BEALL.csv")
#importing dataset from csv

pm25AO <- AO %>%
  select(AVGPM2.5_1316, date17, PM2.517, avg_t_1316, temp17, avg_ws_1316, ws17, avg_wd_1316, wd17) %>%
  #creating dataframe with values from 2017 and average between 2013 and 2016 of PM 2.5 concentration, wind speed and wind direction
  dplyr::rename("pmavg" = `AVGPM2.5_1316`) %>%
  dplyr::rename("date" = `date17`) %>%
  dplyr::rename("avgt" = `avg_t_1316`) %>%
  dplyr::rename("avgws" = `avg_ws_1316`) %>%
  dplyr::rename("avgwd" = `avg_wd_1316`) 

pm25AO$date <- as.POSIXct(pm25AO$date, format = "%d/%m/%Y %H:%M" )
#converting to POSIXT to use scale_x_datetime function, with format DD/MM/YY, HH/mm

#SETTING AXIS AND THEME====


scalb <- scale_x_datetime(labels = date_format("%d %b"), 
                          breaks = "2 days",
                          expand = c(0,0))


theme2 <- theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(colour = "gray", size = 0.5),
                panel.background = element_rect(fill = "white"),
                panel.border = element_rect(colour = "black", fill = NA, size = 0.5 ),
                legend.title = element_blank())

#REFER TO "FINALSCRIPT.R FOR MORE INFORMATION
#PM25/AVG====

aopm25 <- pm25AO %>%
  select(date, pmavg, PM2.517) %>%
  gather(key = "variable", value= "value", -date)

aopm25p <- 
  ggplot(aopm25, aes(x = date, y = value, color = variable, fill = variable)) +
  geom_area(alpha = 0.3, position = "identity")+
  scale_color_manual(values = c("#00008B", "#8B2323"), guide = FALSE) + 
  scale_fill_manual(labels = c("2017", "Average"), values = c("#00008B", "#8B2323"))+
  xlab(" ")+
  ylab(expression(PM["2.5"]*" "* ~ (µg/m["3\n"]))) +
  scalb +
  geom_vline(xintercept=as.numeric(aopm25$date[721])) +
  geom_point()+
  theme2 +
  scale_y_continuous(breaks = seq(0, 400, by =40),
                       labels = c(0, rep("",1), 80 , rep("",1), 160, rep("",1), 
                                  240, rep("",1), 320, rep("",1), 400),
                       limits = c(0,400), expand = c(0,0))
#REFER TO "FINALSCRIPT.R FOR MORE INFORMATION
#WS=====

aows <- pm25AO %>%
    select(date, avgws, ws17 ) %>%
    gather(key = "variable", value= "value", -date)
  
  aowsp <- 
  ggplot(aows, aes(x = date, y = value, color = variable, fill = variable)) +
    geom_area(alpha = 0.3, position = "identity")+
    scale_color_manual(values = c("#030303", "#8B6914"), guide = FALSE) + 
    scale_fill_manual(labels = c("2017", "Average"), values = c("#030303", "#8B6914"))+
    xlab(" ")+
    ylab(expression("Wind speed (m/s)")) +
    scalb +
    geom_vline(xintercept=as.numeric(aopm25$date[721])) +
    geom_point()+
    theme2 +
    scale_y_continuous(breaks = seq(0,8, by = 2),
                       limits = c(0,8), expand = c(0,0))
  #REFER TO "FINALSCRIPT.R FOR MORE INFORMATION
#WD====
  
  aowd <- pm25AO %>%
    select(date, avgwd, wd17) %>%
    gather(key = "variable", value= "value", -date)
  
  aowdp <- 
  ggplot(aows, aes(x = date, y = value, color = variable, fill = variable)) +
    geom_area(alpha = 0.3, position = "identity")+
    scale_color_manual(values = c("#008B00", "#8B8B00"), guide = FALSE) + 
    scale_fill_manual(labels = c("2017", "Average"), values = c("#008B00", "#8B8B00"))+
    xlab(" ")+
    ylab(expression("Wind direction (°)")) +
    scalb +
    geom_vline(xintercept=as.numeric(aopm25$date[721])) +
    geom_point()+
    theme2 +
    scale_y_continuous(breaks = seq(0,360, by = 45),
                       limits = c(0,360), expand = c(0,0))
  #REFER TO "FINALSCRIPT.R FOR MORE INFORMATION
#Temp=====
  
  aot <- pm25AO %>%
    select(date, avgt, temp17) %>%
    gather(key = "variable", value= "value", -date)
  
  aotp <- 
  ggplot(aot, aes(x = date, y = value, color = variable, fill = variable)) +
    geom_area(alpha = 0.3, position = "identity")+
    scale_color_manual(values = c("#458B74", "#7A378B"), guide = FALSE) + 
    scale_fill_manual(labels = c("2017", "Average"), values = c("#458B74", "#7A378B"))+
    xlab(" ")+
    ylab(expression("Wind direction (°)")) +
    scalb +
    geom_vline(xintercept=as.numeric(aopm25$date[721])) +
    geom_point()+
    theme2 +
    scale_y_continuous(breaks = seq(-10,22, by = 3),
                       limits = c(-10,22), expand = c(0,0))
  #REFER TO "FINALSCRIPT.R FOR MORE INFORMATION
#Analysis_Panel=========
  (analysispanel <- ggarrange(aopm25p, aowsp, aowdp, aotp, ncol = 1,
                              labels = (c("A\n", "B\n", "C\n", "D\n")),
                              vjust = c(0.9,0.9,0.9,0.9)))
  #REFER TO "FINALSCRIPT.R FOR MORE INFORMATION
#AVG24h mean========

  

avg24 <- read.csv("24havg.csv")
#importing 24h average pm 2.5 dataset

avg24pp <- avg24 %>%
  select(date, avg24, avg2417) %>%
  gather(key = "variable", value= "value", -date)

avg24pp$date <- as.POSIXct(avg24pp$date, format = "%d/%m/%Y" )


#sortingaxis====


scala <- scale_x_datetime(labels = date_format("%d %b"), 
                          breaks = "2 days",
                          expand = c(0,0))

#avg24ppp <- 
ggplot(avg24pp, aes(x = date, y = value, color = variable, fill = variable)) +
  geom_area(alpha = 0.3, position = "identity")+
  scale_color_manual(values = c("#8B2323", "#00008B"), guide = FALSE) + 
  scale_fill_manual(labels = c("Average", "2017"), values = c("#8B2323", "#00008B"))+
  xlab(" ")+
  ylab(expression(PM["2.5"]*" "* ~ (µg/m["3\n"]))) +
  scala +
  geom_vline(xintercept=as.numeric(avg24pp$date[31])) +
  geom_hline(yintercept = 25, color='red', size=1) +
  geom_point()+
  theme2 +
  scale_y_continuous(breaks = seq(0, 320, by =40),
                     labels = c(0, rep("",1), 80 , rep("",1), 160, rep("",1), 
                                240, rep("",1), 320),
                     limits = c(0,320), expand = c(0,0))
#plotting avg 24h for 2017 and for average of previous years (2013-2016)

#Dividing dataset for kernelexceed====

pm25AO <- AO %>%
  select(AVGPM2.5_1316, date17, PM2.517, avg_t_1316, temp17, avg_ws_1316, ws17, avg_wd_1316, wd17) %>%
  dplyr::rename("pmavg" = `AVGPM2.5_1316`) %>%
  dplyr::rename("date" = `date17`) %>%
  dplyr::rename("avgt" = `avg_t_1316`) %>%
  dplyr::rename("avgws" = `avg_ws_1316`) %>%
  dplyr::rename("avgwd" = `avg_wd_1316`) 

pm25nd <- pm25AO %>%
  select(date, PM2.517, pmavg, wd17, avgwd, ws17, avgws)

pm25n <- pm25nd[0:720,]
pm25d <- pm25nd[721:1464,]
#dividing dataset in pm25n november from 00:00 01/11/ to 23:00 31/11 (row 0 to 720) 
#and December  (row 721 to 1464)00:00 01/12/ to 23:00 31/12


p25n17 <- pm25n %>%
  select(date, PM2.517, wd17, ws17) %>%
  dplyr::rename("pm25" = `PM2.517`) %>%
  dplyr::rename("wd" = `wd17`) %>%
  dplyr::rename("ws" = `ws17`) 

# creating new dataframe for november , renaming variable pm25, wd and ws to be used with Kernelexceed

p25navg <- pm25n %>%
  select(date, pmavg, avgwd, avgws) %>%
  dplyr::rename("pm25" = `pmavg`) %>%
  dplyr::rename("wd" = `avgwd`) %>%
  dplyr::rename("ws" = `avgws`) 

#creating new dataframe for december 2017

p25d17 <- pm25d %>%
  select(date, PM2.517, wd17, ws17) %>%
  dplyr::rename("pm25" = `PM2.517`) %>%
  dplyr::rename("wd" = `wd17`) %>%
  dplyr::rename("ws" = `ws17`)

#repeating for average november

p25davg <- pm25d %>%
  select(date, pmavg, avgwd, avgws) %>%
  dplyr::rename("pm25" = `pmavg`) %>%
  dplyr::rename("wd" = `avgwd`) %>%
  dplyr::rename("ws" = `avgws`) 

#repeating for average december


#KernelexceedNOV=======

kernelExceed(
  p25n17,
  x = "wd",
  y = "ws",
  pollutant = "pm25", by = "day", limit = 25, col = "heat")

"setting kernel exceed to provide number of days in which pm 2.5 cncentrations exceeded the WHO limit
of 25 ug/m3. For Kernelexceed documentation refer to https://www.rdocumentation.org/packages/openair/versions/2.7-0/topics/kernelExceed
or https://cran.r-project.org/web/packages/openair/openair.pdf page 65" 
# Below the procedure is repeated with dataframes from December 2017, average november and average december.


kernelExceed(
  p25navg,
  x = "wd",
  y = "ws",
  pollutant = "pm25", by = "day", limit = 25, col = "heat", xlim = c(10, 360))

#KernelexceedDEC=======

kernelExceed(
  p25d17,
  x = "wd",
  y = "ws",
  pollutant = "pm25", by = "day", limit = 25, col = "heat")

kernelExceed(
  p25davg,
  x = "wd",
  y = "ws",
  pollutant = "pm25", by = "day", limit = 25, col = "heat", xlim = c(10, 360))
