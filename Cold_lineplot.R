#April 28, 2020 - Make line plots/growth curve from a table 

setwd("/Users/Documents")
getwd()

library(ggplot2)

#?read.csv
data<-read.csv(file = "2019_11_27_somecolddata_long.csv", header = TRUE, sep = ",", dec = ".", na.strings = "")
head(data)

data2<-na.omit(data)
head(data2)

#Only want Suv CBS7001 (Suv18) and Scer68
#filter by rows 

data3<-filter(data2, grepl('Suv18',strain))
data3

#select rows with these 2 strings in the strain column 
filtered_data<-filter(data2, strain %in% c("Suv18", "Scer68"))
filtered_data

#another way to do it. select rows. 
SuvScer<-data2 %>% 
  select(time, strain, OD_avg, stdev, ste) %>% 
  filter(strain %in% c("Suv18", "Scer68"))
SuvScer

plot<-ggplot(data = SuvScer, aes(x = time, y = OD_avg, color = strain)) +
  geom_errorbar(aes( ymin=OD_avg-ste, ymax=OD_avg+ste), width = 0.2, color = "lightgrey") +
  geom_line(size = 3) +
  geom_point(size = 3) +
  theme_bw() +
  #omit no gridlines
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_manual(values = c('#2ca25f','#8c510a'),
                     labels = c("S. cerevisiae",
                                "S. uvarum")) +
  #geom_errorbar(aes( ymin=OD_avg-ste, ymax=OD_avg+ste), width = 0.2, color = "grey") +
  labs(title = expression(paste('4',degree,'C growth in liquid YPD',sep='')))+
  #make the title size a bit bigger
  theme(plot.title = element_text(size = rel(1.5))) +
  #center the chart title
  theme(plot.title = element_text(hjust = 0.5)) +
  #make legend bigger????? 
  #theme(guides(size = guide_legend(order = 16))) +
  #move the legend position to top left
  theme(legend.position = c(0.1,0.9)) +
  #no box around legend
  theme(legend.box.background = element_blank(), legend.box.margin = margin(5,5,5,5))+
  #no legend title
  theme(legend.title = element_blank())+
  #theme(legend.key.size = unit(5,"line"))+
  #theme(legend.text = element_text(size = 14)) +
  theme(legend.key.size = unit(2.5,"line"))+
  theme(legend.text = element_text(size = 14))+
  
  #rename x-axis label
  xlab("time in days") +
  #make x-axis label larger font
  theme(axis.title.x = element_text(size = rel(1.5))) +
  #rename y-axis label , with subscript
  ylab(expression('OD 600'[nm])) +
  #make the y-axis label larger font size
  theme(axis.title.y = element_text(size = rel(1.5))) +
  #make the x- axis text larger font size
  theme(axis.text.x = element_text(size = 16)) +
  #make the y-axis text larger font size
  theme(axis.text.y = element_text(size = 16))
plot



