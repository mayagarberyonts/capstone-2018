# Environmental Data

library(plyr)
library(dplyr)
library(lubridate)

setwd("~/Desktop/Capstone_2017:18/Environmental Data")

DB_temp <- read.csv('dabob_temp_monthly.csv', header = T)
NB_temp <- read.csv('neahbay_temp_monthly.csv', header = T)
TI_temp <- read.csv('totten_temp_monthy.csv', header = T)

temp_df <- read.csv('full_temp.csv')

  
DB_temp$date <- as.factor(DB_temp$date)

ggplot(temp_df, aes(Date, average_temp, group=1)) +
  geom_line(aes(color=Location, group=Location))+
  theme_classic()+
  scale_color_brewer(palette = "Dark2")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Date')+
  ylab('Average Monthly Temperature (F)')+
  scale_y_continuous(limits = c(40,65))+
  scale_x_discrete(limits=c("November 2016", "December 2016", "January 2017", "February 2017", "March 2017", "April 2017", "May 2017",
                            "June 2017"))   

