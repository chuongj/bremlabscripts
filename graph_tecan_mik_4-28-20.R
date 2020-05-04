#Apr27,2020 - Graph the TECAN 96 well plate formatted data as growth curves
#1)mostly datacleaning/wrangling using baseR and tidyverse
#2)then use ggplot2 graph the curves

library(tidyverse)
library(ggplot2)

setwd("~/Documents/bremlabdata/apr27/figs_for_Rachel")

#Matt Smik Scer Pseudomonas data
raw_data<-read.csv(file = "smik&scerDATAcopyjc.csv", header = FALSE, sep = ",",dec = ".")
head(raw_data)
tail(raw_data)

#Well Map 
#Smikatae’ :[[‘D01’,‘E01’,‘G01’], [‘#EC5B03’], [5.0], [‘-’]],
#Scerevisiae’ : [[‘A01’,‘B01’,‘C01’], [‘#B47001’], [5.0], [‘-’]],
#Scerevisiae w/Pfluorescens extract’ :[[‘A08’,‘B08’,‘C08’], [‘#33B5FF’], [5.0], [‘-’]],
# Smikatae w/Pfluorescens extract’ : [[‘D08’,‘E08’,‘G08’], [‘#000000’], [5.0], [‘-’]],

#omit row F
noFdata<-filter(raw_data, !grepl('F',V1))

head(noFdata)
tail(noFdata)
data

#i only want the columns V1, V2, and V9. 
#omit columns V3, V4, V5, V6, V7, V8, V10, V11, V12, V13
#using index to delete the columns 

data<-select(noFdata, -c(3:8, 10:13))
head(data)
tail(data)

#omit row H
data<-filter(data, !grepl('H',V1))
head(data)

# 
data$V2

#maybe I should omit the <> rows. 
#then take the means across very 3 in the data$V2 and every 3 in data$V9 (1-3, 4-6. )

#omit rows containing '<>'
data<-filter(data, !grepl('<>',V1))
data


#CALCULATE MEANS 
###############################################
##You can also do this with base R using by:

#> n<-nrow(Table)
#> index<-ceiling((1:n)/10)
#> by(Table$Pa,index,mean)
#> by(Table$Pa,index,sd)

# cbind(index=unique(index),mean=by(Table$Pa,index,mean),sd=by(Table$Pa,index,sd))

#index       mean         sd
#1     1 -0.1663894 0.07604938
#2     2 -0.1650722 0.07544763

#another way to calculate mean every 3 in a column 
length(data$V2)
head(data)
n<-nrow(data)
n
index<-ceiling((1:n)/3)
index
v2mean = by(data$V2, index, mean)

#calculate stdev of every 3 in the column
v2sd = by(data$V2,index,sd)

#calculate sem , 3 bioreps 
v2sem<-v2sd/sqrt(3)

#make condition column
#ScerYPD, SmikYPD, ScerYPD, SmikYPD, 
condV2<-c("ScerYPD","SmikYPD")
96/2 #48 times 
cond_col<-rep(condV2, 48)
cond_col

#Create new vectors. I can't cbind them because they are in "by" format. 
V2mean<-as.vector(v2mean)
head(V2mean)
V2sd<-as.vector(v2sd)
V2sem<-as.vector(v2sem)
V2sem

cbind(data.frame(time, cond_col, V2mean, V2sd, V2sem))

#create a timepoint sequence
time_seq<-seq(0, 23.5, by = 0.5)
#replicate 2 times
time_seq_rep<-rep(time_seq, times = 2)
time_seq_rep
#make sure it has 144 items
length(time_seq_rep)

#sort it low to high
time<-sort(time_seq_rep)
time

#cbind them all. must use data.frame to mix strings and numbers. 
timed_data<-cbind(data.frame(time, cond_col, V2mean, V2sd, V2sem))
head(timed_data)

#########
#Do it again with Column V9 creating a new table. 

head(data$V9)
length(data$V9) #288

n<-length(data$V9)
n
index<-ceiling((1:n)/3)
index
length(index)#288
typeof(data) #list
v9mean<-by(data$V9,index,mean)
V9mean<-as.vector(v9mean)

#mean
b_mean<-BinMean(b, every = 3)
length(BinMean(b, every = 3)) #96

#stdev 
V9sd<-as.vector(by(data$V9,index,sd))
length(V9sd)

#sem
V9sem<-V9sd/sqrt(3)

#condition column
condV9<-(c("ScerPf","SmikPf"))
cond_col<-rep(condV9, 48)
cond_col

time

V9table<-cbind(data.frame(time, cond_col,V9mean,V9sd,V9sem))
head(V9table)


#merge V2 Table V9 Table using time as the unifying variable. 
#should 96 + 96 rows at the end  = 192 data rows

#rename column names in both tables so they have the same columns. 
V9table<-V9table %>% 
  rename(
    condition = cond_col,
    mean = V9mean, sd = V9sd, sem = V9sem)
head(V9table)

V2table<-timed_data %>%
  rename(
    condition = cond_col,
    mean = V2mean, sd = V2sd, sem = V2sem
  )
head(V2table)

#combine the 2 tables 
newdata<-rbind(V2table,V9table)
tail(rbind(V2table,V9table)) #192 rows! 

head(newdata)
tail(newdata)

#########PLOT###########################################################
plotV9<-ggplot(data = V9table, aes(x = time, y = mean, color = condition)) +
  #geom_smooth()+
  geom_errorbar(aes( ymin=mean-sem, ymax=mean+sem), width = 0.2, color = "lightgrey") +
  geom_line(size = 3) +
  #geom_point(size = 3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = c('#2ca25f','#756bb1'),
                     labels = c("S.cerevisiae in
  Pseudomonas spent media",
                                "S.mikatae in
  Pseudomonas spent media"))+
  #geom_errorbar(aes( ymin=mean-sem, ymax=mean+sem), width = 0.2) +
  #custom title
  #labs(title = expression(paste('37',degree,'C growth',sep='')))+
  #make the title size a bit bigger
  theme(plot.title = element_text(size = rel(1.5))) +
  #center the chart title
  theme(plot.title = element_text(hjust = 0.5)) +
  #move the legend position to top left
  theme(legend.position = c(0.15,0.9)) +
  #add a bounding box to the legend
  theme(legend.box.background = element_blank()) +
  #no legend title
  theme(legend.title = element_blank())+
  #theme(legend.box.margin = margin(1,1,1,1))) +
  theme(legend.key.size = unit(2.5,"line"))+
  theme(legend.text = element_text(size = 12))+
  #rename x-axis label
  xlab("time in hours") +
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
plotV9




## CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)


