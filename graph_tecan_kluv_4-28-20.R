#Apr24,2020 graph the TECAN 96 well plate xfluor formatted data as growth curves 
#1)mostly datacleaning/wrangling using baseR and tidyverse
#2)then use ggplot2 graph the curves


library(tidyverse)
library(ggplot2)

setwd("~/Documents/bremlabdata/apr24/")

#Abel's 37C data
raw_data<-read.csv(file = "37_data_abel_out.csv", header = FALSE, sep = ",",dec = ".")
head(raw_data)
tail(raw_data)

#omit rows A,B,E,F,G,H
#row C is lactis, row D is marx
CD_data<-filter(raw_data, !grepl('A',V1))
CD_data<-filter(CD_data, !grepl('B',V1))
CD_data<-filter(CD_data, !grepl('E',V1))
CD_data<-filter(CD_data, !grepl('F',V1))
CD_data<-filter(CD_data, !grepl('G',V1))
CD_data<-filter(CD_data, !grepl('H',V1))

head(CD_data)
tail(CD_data) #there are 144 data rows 

#create a table with only the 2 blank columns
#so we can subtract it from the average later. 
blanktable<-CD_data %>% select(V2,V13)
head(blanktable)

#average across wells V3 to V12 and subtracted the blank avg. V2 and V13 are blanks.
head(CD_data[3:12])
CD_data$avg_OD<-rowMeans(CD_data[3:12]-rowMeans(blanktable))
head(CD_data)
tail(CD_data) #there are still 144 data rows :)

#Matrix-ize our data in order to calculate stdev later
matrixCD<-as.matrix(as.data.frame(lapply(CD_data, as.numeric)))
head(matrixCD)
tail(matrixCD)

#Use apply() to calculate stdev across specific rows
#The first argument is your data object. 
#The second argument is an integer specifying either 1 for rows or 2 for columns
# (This is the direction the function will be applied to the data frame).
#The final argument is the function you wish to apply to your data frame
#(such as mean or standard deviation (sd) in this case.

stdev<-apply(matrixCD[,3:12],1,sd)  #all rows, columns 3:12 
head(stdev)
tail(stdev)

#cbind stdev list to CD_data frame
CD_data_with_STDEV<-cbind(CD_data,stdev)
head(CD_data_with_STDEV)
tail(CD_data_with_STDEV)

#Next, calculate sem 
sem<-stdev/sqrt(10)# 10bioreps
head(sem)

#cbind it to CD_data frame, new column
CD_data_with_stats<-cbind(CD_data_with_STDEV,sem)
head(CD_data_with_stats)

#filter it selecting columns
#V1, avgOD, stdev, sem

filtered_data<-CD_data_with_stats %>% select(V1, avg_OD, stdev, sem)
head(filtered_data)
tail(filtered_data)

#replace C with Klac and replace D with Kmar
filtered_data$V1
strain<-str_replace(filtered_data$V1, "C", "Klac")
strain<-str_replace(strain, "D", "Kmar")
strain

head(filtered_data)
tail(filtered_data)

#cbind strain to front of the table. 
strain_filtered_data<-cbind(strain, filtered_data)
head(strain_filtered_data)

#create a timepoint sequence
time1<-seq(0, 23.5, by = 0.5)
#replicate 3 times
time2<-rep(time1, times = 3)
time2
#make sure it has 144 items
length(time2)

#sort it low to high
time<-sort(time2)

#cbind it 
timed_data<-cbind(time, strain_filtered_data)
head(timed_data)

#omit rows containing '<>'
clean_data<-filter(timed_data, !grepl('<>',V1))
head(clean_data)
tail(clean_data)

plot<-ggplot(data = clean_data, aes(x = time, y = avg_OD, color = strain)) +
#geom_smooth(size = 2) +
  geom_line(size = 3) +
  #geom_point(size = 2) +
  #make background black and white
  theme_bw() +
  #omit no gridlines
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  #manual color in legend
  scale_color_manual(values = c('#253494','#cc4c02'),
                     labels = c("K. lactis",
                                "K. marxianus"))+
  
  #fc8d59 orange
  # #91bfdb blue
  ##253494 dark blue 
  #cc4c02 dirty orange

  #choose color palette 
  #scale_color_brewer(palette = "Dark2", labels =c("K. lactis",
  #                                                "K. marxianus")) +
  
  #add error bars
  geom_errorbar(aes( ymin=avg_OD-sem, ymax=avg_OD+sem), width = 0.2) +
  #custom title
  labs(title = expression(paste('37',degree,'C growth',sep='')))+
  #make the title size a bit bigger
  theme(plot.title = element_text(size = rel(1.5))) +
  #center the chart title
  theme(plot.title = element_text(hjust = 0.5)) +
  #move the legend position to top left
  theme(legend.position = c(0.11,0.9)) +
  #add a bounding box to the legend
  theme(legend.box.background = element_blank()) +
  #no legend title
  theme(legend.title = element_blank()) +
  #theme(legend.key.size = ) +
  #theme(legend.text = element_text(size = 14)) +
  theme(legend.key.size = unit(2.5,"line"))+
  theme(legend.text = element_text(size = 14))+

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
plot

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


