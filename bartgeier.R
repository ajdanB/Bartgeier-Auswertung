library(readxl)
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(ggbeeswarm)
library(ggforce)
library(oce)
library(ggthemes)
library(solartime)
library(svglite)


# only for testing purposes 
ts <- function() {
  
  longitude <- 13.00187
  t <- as.POSIXct("2021-06-03 00:02:00")
  st <- getSolarTimeHour(t, longitude)
  sonnenzeit_zu_zeit(st)
  
}

sonnenzeit_zu_zeit <- function(sonnenzeit) {
  sonnenzeit <- sonnenzeit %% 24
  
  stunde <- sonnenzeit %/% 1
  stunde_rest = sonnenzeit - stunde
  
  minute_all <- 60 * stunde_rest
  minute = minute_all %/% 1
  minute_rest <- minute_all - minute
  
  sekunde <- (60 * minute_rest) %/% 1
  
  time <- sprintf("%02d:%02d:%02d", stunde, minute, sekunde)
  #time <- as.POSIXct(time, format="%T")
  
  return(time)
}


read_bartgeier_excel <- function(path) {
  data_bartgeier <- read_excel(path, 
                               col_types = c("numeric", "date", "text", 
                                             "text", "date", "date", "text", "date", 
                                             "date", "text", "text", "text", 
                                             "text", "text", "text", "text", 
                                             "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text"))
  
  # Zeiten konvertieren
  data_bartgeier <- data_bartgeier %>% 
    mutate(`Start (Uhrzeit)` = as.POSIXct(paste(format(Datum, "%Y-%m-%d"), format(`Start (Uhrzeit)`, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")) %>%
    mutate(`Ende (Uhrzeit)` = as.POSIXct(paste(format(Datum, "%Y-%m-%d"), format(`Ende (Uhrzeit)`, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")) %>%
    mutate(Startzeit = as.POSIXct(paste(format(Datum, "%Y-%m-%d"), format(Startzeit, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")) %>%
    mutate(Endzeit = as.POSIXct(paste(format(Datum, "%Y-%m-%d"), format(Endzeit, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"))
  
  
  print(data_bartgeier)
  return(data_bartgeier)
}

bartgeier_graph <- function(path, name, erster_flug) {
  
  data_bartgeier <- read_bartgeier_excel(path)

  #####################
  
  data_bartgeier_vor_erster_flug <- filter(data_bartgeier, Datum < erster_flug)
  data_bartgeier_ab_erster_flug <- filter(data_bartgeier, Datum >= erster_flug) 
  
  

  titel1 = paste("Verhaltensanalyse ", name, ", geplottet zur lokalen Sonnenzeit", sep="")
  titel2 = paste("Verhaltensanalyse ", name, ", geplottet zur lokalen Sonnenzeit, bis zum 1. Flug", sep="")
  titel3 = paste("Verhaltensanalyse ", name, ", geplottet zur lokalen Sonnenzeit, ab dem 1. Flug", sep="")
  
  filename1 <- paste(name, "_gesammt", sep = "")
  filename2 <- paste(name, "_bis_erstflug", sep = "")
  filename3 <- paste(name, "_ab_erstflug", sep = "")
  
  plot(data_bartgeier, titel1, filename1)
  plot(data_bartgeier_vor_erster_flug, titel2, filename2)
  plot(data_bartgeier_ab_erster_flug, titel3, filename3)
}  

plot <- function(data_bartgeier, titel, filename) {
  
  # Definieren Sie den Standort durch die geografische Breite und Länge
  latitude <- 47.63236  # Berchtesgaden
  longitude <- 13.00187
  

  daktion <- data_bartgeier %>% select(Startzeit, Trinken, Nahrungsaufnahme, Ausscheidung, Interaktion, Gefiederpflege, `Rast/Schlafplatz Qualität Schlafmuster`, Betteln, Flügelschlagserien)  #%>%   #mutate(Startzeit = format(Startzeit, "%H:%M"))
  
  df_melted <- melt(daktion, id.vars = "Startzeit", value.name = "Value", na.rm = FALSE) %>% filter(complete.cases(.))
  
  df_melted$oStartzeit <- format(as.POSIXct(df_melted$Startzeit), "%T")
  
  # Sonnenazimuth der Zeit berechnen (geht Zeitumstellung etc. aus dem Weg)
  df_melted$sStartzeit <- sunAngle(df_melted$Startzeit, latitude, longitude)[["time"]]
  df_melted$sStartzeit <- format(as.POSIXct(df_melted$sStartzeit), "%T")
  
  df_melted$aStartzeit <- sunAngle(df_melted$Startzeit, latitude, longitude)[["azimuth"]]
  
  df_melted$solarStartzeit <- sonnenzeit_zu_zeit(getSolarTimeHour(df_melted$Startzeit, longitude))

  
  
  #ggplot(df_melted, aes(x = as.POSIXct(oStartzeit, format="%H:%M:%S"), y = variable)) +
  #  scale_x_datetime(date_labels = "%H:%M:%S")+
  #  geom_violin(adjust=0.2)+
  #  #geom_jitter(size=0.1, alpha=0.3)+
  #  geom_beeswarm(cex = 0.5, size=0.1) +
  #  theme_minimal()
  
  plt <- ggplot(df_melted, aes(x = as.POSIXct(solarStartzeit, format="%H:%M:%S"), y = variable, color = as.POSIXct(solarStartzeit, format="%H:%M:%S"))) +
    ggtitle(titel)+
    xlab("Sonnenzeit Berchtesgaden")+
    ylab("")+
    scale_x_datetime(date_labels = "%H:%M", limits = as.POSIXct(c("02:45:00","19:00:00"), format="%H:%M:%S"))+
    geom_violin(adjust = 0.125, scale = "width") +
    geom_beeswarm(cex = 0.8, size=0.7, alpha=1) +
    #geom_dotplot(dotsize = 0.5, binaxis = "x", stackdir = "center")+
    theme_minimal()+
    theme(legend.position = "none")+
    scale_color_gradient(low = "#0091ff", high = "#f0650e")
  #geom_beeswarm(size=0.5)
  
  out_file = paste(filename, ".svg", sep="")
  ggsave(out_file, width = 11, height = 8)
  #ggsave(out_file)
  print(plt)
  
  # ggplot(df_melted, aes(x = aStartzeit, y = variable)) +
  #    geom_violin()+
  #    geom_sina()+
  #    theme_minimal()
  #geom_beeswarm(size=0.5)
  
  #ggplot(df_melted, aes(x = oStartzeit, y = variable)) +
  #  geom_violin()+
  #  geom_sina() 
  
}
