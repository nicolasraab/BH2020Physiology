rm(list = ls())


library(dplyr)
library(tibble)
library(readxl)
library(ggplot2)
library(lubridate)
library(naniar)
library(stringr)
library(reshape2)
library(ggplot2)

library(reshape)


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2020/Physiology_2020/")

BH_2020_Physio <- read.csv("data_physiology_complete_BH_2020.csv")


BH_2020_Physio$date <- as.Date(BH_2020_Physio$date)

BH_2020_Physio <- add_column(BH_2020_Physio,TIMESTAMP = ymd_hms(paste(BH_2020_Physio$date,BH_2020_Physio$hhmmss, sep = " ")),.after = "date")

BH_2020_Physio <- add_column(BH_2020_Physio,Treat_Round = paste(BH_2020_Physio$date,BH_2020_Physio$BLOCK,BH_2020_Physio$round,sep = "-"),.after = "BLOCK")

gsw_mean <- aggregate(BH_2020_Physio$gsw~BH_2020_Physio$Treat_Round,FUN = mean, na.action = na.omit)
colnames(gsw_mean) <- c('Treatment','g_sw')
gsw_mean <- add_column(gsw_mean, round = substr(gsw_mean$Treatment,nchar(gsw_mean$Treatment),nchar(gsw_mean$Treatment)),.after="Treatment")

gsw_mean <- gsw_mean %>% mutate(hour = case_when(
  round == 1 ~ "05:00:00",
  round == 2 ~ "09:00:00",
  round == 3 ~ "12:00:00",
  round == 4 ~ "15:00:00",
  round == 5 ~ "18:00:00",
  
  #TRUE ~ hms(hour)
))

gsw_mean <- add_column(gsw_mean, TIMESTAMP = paste(substr(gsw_mean$Treatment,1,10),gsw_mean$hour, sep = " "), .before = "Treatment")
gsw_mean$TIMESTAMP <- as.POSIXct(gsw_mean$TIMESTAMP)

gsw_mean <- add_column(gsw_mean, date_block = substr(gsw_mean$Treatment,1,15), .before = "Treatment")

gsw_mean_PD <- gsw_mean %>% filter(hour == "05:00:00")
gsw_mean_MM <- gsw_mean %>% filter(hour == "09:00:00")
gsw_mean_MD <- gsw_mean %>% filter(hour == "12:00:00")
gsw_mean_MA <- gsw_mean %>% filter(hour == "15:00:00")
gsw_mean_AF <- gsw_mean %>% filter(hour == "18:00:00")





#####################################################################
####################### Homework for Bambach #######################
#####################################################################

Var_inter <- BH_2020_Physio$gsw


Code <-paste(BH_2020_Physio$date,BH_2020_Physio$BLOCK,BH_2020_Physio$VINE,BH_2020_Physio$LEAF,sep = "-")



unique_Code <- unique(Code)

List_var <- NULL
for(i in 1:length(unique_Code)){
  
  rows <- which(unique_Code[i] == Code)
  
  List <- BH_2020_Physio[rows,] %>% select(date,BLOCK,VINE,LEAF,round,gsw) 
  
  matrix_var <- matrix(nrow = 1, ncol = 5, data = NA)
  
  for (j in 1:nrow(List)){
    
    matrix_var[1,List$round[j]] <- List$gsw[j]
  }

    List_var[[i]] <- bind_cols(data.frame(unique_Code[i]), data.frame(matrix_var))
    }

hola <- do.call(bind_rows,List_var)

#####################################################################

lwp_mean <- aggregate(BH_2020_Physio$leaf_wp_MPa~BH_2020_Physio$Treat_Round,FUN = mean, na.action = na.omit)
colnames(lwp_mean) <- c('Treatment','L_wp')
lwp_mean <- add_column(lwp_mean, round = substr(lwp_mean$Treatment,nchar(lwp_mean$Treatment),nchar(lwp_mean$Treatment)),.after="Treatment")

lwp_mean <- lwp_mean %>% mutate(hour = case_when(
  round == 1 ~ "05:00:00",
  round == 2 ~ "09:00:00",
  round == 3 ~ "12:00:00",
  round == 4 ~ "15:00:00",
  round == 5 ~ "18:00:00",
  
  #TRUE ~ hms(hour)
))

lwp_mean <- add_column(lwp_mean, TIMESTAMP = paste(substr(lwp_mean$Treatment,1,10),lwp_mean$hour, sep = " "), .before = "Treatment")
lwp_mean$TIMESTAMP <- as.POSIXct(lwp_mean$TIMESTAMP)

lwp_mean <- add_column(lwp_mean, date_block = substr(lwp_mean$Treatment,1,15), .before = "Treatment")

lwp_mean_PD <- lwp_mean %>% filter(hour == "05:00:00")
lwp_mean_MM <- lwp_mean %>% filter(hour == "09:00:00")
lwp_mean_MD <- lwp_mean %>% filter(hour == "12:00:00")
lwp_mean_MA <- lwp_mean %>% filter(hour == "15:00:00")
lwp_mean_AF <- lwp_mean %>% filter(hour == "18:00:00")














###################### ###################### ###################### ###################### 
###################### Adding Evapotranspiration from Bambach ####################
###################### ###################### ###################### ###################### 



SR_ET_2020 <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2020/output/derived_data/ETwl_AllST_2020.csv")

SR_ET_melt <- SR_ET_2020 %>% select(-c(T_max))
SR_ET_melt <- SR_ET_melt %>% melt(id = "DATE")

SR_ET_melt$DATE <- format(as.Date(SR_ET_melt$DATE,'%m/%d/%y'), '%Y-%m-%d')
SR_ET_melt$variable <- as.factor(SR_ET_melt$variable)
SR_ET_melt$value <- as.numeric(SR_ET_melt$value)


SR_ET_melt$DATE <- as.Date(SR_ET_melt$DATE)

ggplot(data = SR_ET_melt, aes(x = DATE, y = value, color = variable))+geom_line()+geom_point()+ylim(0,7.5)+
  scale_x_date(limits = as.Date(c("2020-07-15","2020-09-15")),date_breaks = "2 week", date_labels = "%b %d") + labs(title = "Surface renewal evapotranspiration") +
  xlab("Days") + ylab("ET (mm/day)")


###### Merging Surface Renewal to 2020 data ##########

SR_ET_melt <- SR_ET_melt %>% 
  mutate(variable = case_when(
    variable == "ET_B1R1" ~ "B1R1", 
    variable == "ET_B1R4" ~ "B1R4", 
    variable == "ET_B2R1" ~ "B2R1", 
    variable == "ET_B2R2" ~ "B2R2",
    variable == "ET_B2R3" ~ "B2R3",
    variable == "ET_B3R1" ~ "B3R1", 
    variable == "ET_B3R2" ~ "B3R2", 
    variable == "ET_B3R3" ~ "B3R3",
    
    TRUE ~ as.character(variable)
  ))



SR_ET_melt <- add_column(SR_ET_melt, Date_Station = paste(as.Date(SR_ET_melt$DATE),SR_ET_melt$variable, sep = "-"), .after = "DATE")


Met_ALL_2020 <- read.csv("/Volumes/GoogleDrive/My Drive/BordenHill_Outputs/Met_15min/BordenHill2020_All.csv")
Met_ALL_2020$TIMESTAMP <- as.Date(Met_ALL_2020$TIMESTAMP, '%m/%d/%y %H:%M')


Met_ALL_2020 <- add_column(Met_ALL_2020, Date_Station = paste(Met_ALL_2020$TIMESTAMP,Met_ALL_2020$Station, sep = "-"), .after = "TIMESTAMP")



Met_ALL_ET <- merge(Met_ALL_2020, SR_ET_melt, by.x = "Date_Station", by.y = "Date_Station", all.x = TRUE)

Met_ALL_ET <- Met_ALL_ET %>% select(-c(DATE,variable))
Met_ALL_ET <- rename(Met_ALL_ET, ET_mmday = value)



hola <- (aggregate(Met_ALL_ET[,"VW_Avg"]~Met_ALL_ET$Date_Station,FUN=min,na.action = na.omit))
colnames(hola) <- c("Date_Station","Soil_Moisture")
hola <- hola %>% mutate(Soil_Moisture = Soil_Moisture*100)
  
  chao <- merge(hola,SR_ET_melt, by.x = "Date_Station", by.y = "Date_Station", all.x = TRUE)

chao <- add_column(chao, Station = substr(chao$Date_Station, 12,15), .after = "Date_Station")
chao <- add_column(chao, Date = substr(chao$Date_Station, 1,10), .after = "Station")
  chao$Date <- as.Date(chao$Date)
  
  chao_station <- chao %>% filter(Station == "B1R1")
  
  ggplot(data = chao_station, aes(x = DATE, y = value, colour = "Evapotranspiration"))+geom_line()+geom_point() +
    geom_line(aes(y = Soil_Moisture/5, colour = "Soil Moisture"))+ scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Soil Moisture [%]"))+
    scale_colour_manual(values = c("royalblue4", "skyblue3")) + labs(y = "Evapotranspiration [mm/day]",
                                                                     x = "Date",
                                                                     colour = "",
                                                                     title = "B1R1") +
    theme(legend.position = c(0.25, 0.15)) +
    scale_x_date(limits = as.Date(c("2020-07-20","2020-09-15")),date_breaks = "2 week", date_labels = "%b %d")
   
  ggsave("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2020/output/figures/B3R3_ET_SM.pdf")
  
  #########################################################################################################################################################
  
  ET_MidDay_gs <- merge(chao,gsw_mean_MD,by.x = "Date_Station", by.y = "date_block", all.x = TRUE)
  
  
  ET_PreDawnGs_Station <- ET_MidDay_gs %>% filter(Station == "B3R3")
  
  
  
  ggplot(data = ET_PreDawnGs_Station, aes(x = DATE, y = value, colour = "Evapotranspiration"))+geom_line()+geom_point() +
    geom_point(aes(y = g_sw*20, colour = "Mid Day Stomatal Conductance"))+ scale_y_continuous(sec.axis = sec_axis(~./20, name = "Stomatal Conductance [mol H2O m^-2 s^-1]"))+
    scale_colour_manual(values = c("royalblue4", "springgreen4")) + labs(y = "Evapotranspiration [mm/day]",
                                                                     x = "Date",
                                                                     colour = "",
                                                                     title = "B3R3") +
    theme(legend.position = c(0.35, 0.15)) +
    scale_x_date(limits = as.Date(c("2020-08-10","2020-09-15")),date_breaks = "2 week", date_labels = "%b %d")
  
  
  
  ggsave("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2020/output/figures/B3R3_ET_GS.pdf")
  
  
  
#########################################################################################################  
  
  
  
  ET_PreDawn_lwp <- merge(chao,lwp_mean_PD,by.x = "Date_Station", by.y = "date_block", all.x = TRUE)
  
  
  ET_PreDawnLWP_Station <- ET_PreDawn_lwp %>% filter(Station == "B3R3")
  
  
  
  ggplot(data = ET_PreDawnLWP_Station, aes(x = DATE, y = value, colour = "Evapotranspiration"))+geom_line()+geom_point() +
    geom_point(aes(y = L_wp*7, colour = "PD Leaf WP"))+ scale_y_continuous(sec.axis = sec_axis(~./7, name = "Leaf Water Potential [MPa]"))+
    scale_colour_manual(values = c("royalblue4", "magenta4")) + labs(y = "Evapotranspiration [mm/day]",
                                                                         x = "Date",
                                                                         colour = "",
                                                                         title = "B3R3") +
    #theme(legend.position = c(0.35, 0.15)) +
    scale_x_date(limits = as.Date(c("2020-08-10","2020-09-15")),date_breaks = "2 week", date_labels = "%b %d")
  
  
  
  ggsave("/Volumes/GoogleDrive/.shortcut-targets-by-id/1r1oKDNHCr46TTMS-33jRDHiZMbC6tDsI/Lodi_Heatwave/Data/Lodi_2020/output/figures/B3R3_ET_PD_LWP.pdf")
  
  
  
  
  
  ggplot(data = chao, aes(x= DATE,y=Soil_Moisture, colour = Station))+ geom_line()+ geom_point() +
    scale_color_manual(values = c("firebrick4","firebrick3","firebrick2",
                                  "springgreen4","springgreen3","springgreen2",
      "blue","royalblue4","skyblue3"))

  
  
  