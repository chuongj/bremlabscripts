#5/5/20-Wrangle TECAN28C data to be long format so then I can get the rate for downstream ANOVA
#TECAN 96wp format is xfluor format. Change to long with time as first column.
#Handle 3 bioreps from different days,  each on a separate .csv 
#Later -  feed it thru the nonlinear regression function. 

setwd("/Users/Documents")
library(tidyverse)

#Right now the tecan data is a .txt file. 
#Save it as a .csv file before feeding it in R.

#####  BIOREP 1  ######## 
raw1<-read.csv(file = "2017_7_19_JC_ScSpmix_28c_xfluor.csv", 
                  header = F, sep = ",", dec = ".")
head(raw1)
tail(raw1)   
ncol(raw1) #13 

#Column13 is the blank
#Row E is Spar62
#Row F is Scer68

#Omit rows A,B,C,D,G,H
fildata<-filter(raw1, !grepl('A',V1))
fildata<-filter(fildata, !grepl('B',V1))
fildata<-filter(fildata, !grepl('C',V1))
fildata<-filter(fildata, !grepl('D',V1))
fildata<-filter(fildata, !grepl('G',V1))
fildata<-filter(fildata, !grepl('H',V1))
head(fildata)

#average across rows (techrep) minus the blank, add new column
fildata$biorep1<-rowMeans(fildata[2:12])-fildata$V13

head(fildata)

#replace E with Spar and replace F with Scer
# Species column 
fildata$V1
species<-str_replace(fildata$V1, "E", "Spar")
species<-str_replace(species, "F", "Scer")
species

#cbind species column to front of data frame
fildata<-cbind(species, fildata)
head(fildata)

#subset the dataframe only taking 'species', 'V1', 'biorep1' columns 
subdata<-select(fildata, c(species,V1,biorep1))
head(subdata)

#create a timepoint list
time<-sort((rep(seq(0, 39.5, by = 0.5),times =3)))
time
length(time) #240 

#cbind time column to front of data frame
subdata<-cbind(time, subdata)
head(subdata)

#omit rows containing '<>'
subdata<-filter(subdata, !grepl('<>',V1))
head(subdata)
tail(subdata)

#omit V1 column
subdata<-subset(subdata, select = -c(V1))
head(subdata)  
  
#change negatives to 0 
subdata$biorep1[subdata$biorep1<0]<-0.0
subdata

########  BIOREP 2##########

#The plan is to JOIN biorep2 and biorep3 into data_nostats
#then take average across 3 bioreps, stdev, and sem.

br2_raw<-read.csv(file = "2017_7_28_JC_remain_APC1_TAF2_swaps28c_xfluor.csv",
                  header = FALSE, sep = ",", dec = ".")
head(br2_raw)

#Row A is Spar62, RowB is Scer68 
#omit Rows C-H

br2data<-filter(br2_raw, !grepl('C', V1))
head(br2data)
br2data<-filter(br2data, !grepl(('D'), V1))
br2data<-filter(br2data, !grepl(('E'), V1))
br2data<-filter(br2data, !grepl(('F'), V1))
br2data<-filter(br2data, !grepl(('G'), V1))
br2data<-filter(br2data, !grepl(('H'), V1))
head(br2data)

#replace A with Spar. Replace B with Scer.
species<-str_replace(br2data$V1, "A", "Spar")
species<-str_replace(species, "B", "Scer")
species

#cbind species column to front of data frame
b2data<-cbind(species, br2data)
head(b2data)

nrow(b2data) #240
ncol(b2data) #14
#take average across rows using columns [3:13] minus column 14. 
#call it biorep2 and add it 

#fildata$avg_OD<-rowMeans(fildata[2:12])-fildata$V13
b2data$biorep2<-rowMeans(b2data[3:13])-br2data$V13
head(b2data)
tail(b2data)

#omit rows containing '<>'
b2data<-filter(b2data, !grepl('<>',V1))
head(b2data)
tail(b2data)

data_2bioreps<-cbind(subdata, b2data$biorep2)
#rename column b2data$biorep2 to biorep1 

head(data_2bioreps)

data_2bioreps<-data_2bioreps %>% rename('biorep2' = 'b2data$biorep2')
head(data_2bioreps)
tail(data_2bioreps)


#change negatives to Zero 
data_2bioreps$biorep2[data_2bioreps$biorep2<0]<-0.0
head(data_2bioreps)

########BIOREP 3 #############

b3raw<-read.csv(file = "2017_12_13_JC_internat28C_xfluor.csv", header = F,
                sep = ",", dec = ".")
head(b3raw)

#wellmap Row E is 62Spar, RowF is 68 Scer, Column 12 is blank. 

#omit rows A, B, C, D, G, H
b3data<-filter(b3raw, !grepl('A', V1))
b3data<-filter(b3data, !grepl('B', V1))
b3data<-filter(b3data, !grepl('C', V1))
b3data<-filter(b3data, !grepl('D', V1))
b3data<-filter(b3data, !grepl('G', V1))
b3data<-filter(b3data, !grepl('H',V1))
head(b3data)

#average across rows/techreps minus the blank and append a new column called biorep3
b3data$biorep3<-rowMeans(b3data[2:13])-b3data$V13

head(b3data)
tail(b3data) #240 data rows

#omit rows containing '<>'
b3data<-filter(b3data, !grepl('<>',V1))
head(b3data)

#add biorep3 column to dataframe containing 2bioreps. 
data_w_3bioreps<-cbind(data_2bioreps,b3data$biorep3)

#rename column name to biorep3
data_w_3bioreps<-data_w_3bioreps %>% rename('biorep3' = 'b3data$biorep3')


head(data_w_3bioreps)

#zero values that are < 0
data_w_3bioreps$biorep3[data_w_3bioreps$biorep3<0]<-0.0
head(data_w_3bioreps)

write.csv(data_w_3bioreps,"filename.csv", row.names = FALSE)

####### CLEAN UP ################
# Clear Environment
rm(list = ls())

# Clear packages
detach("package:datasets", unload = TRUE)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
