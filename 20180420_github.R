

library(tidyr)
library(dplyr)
library(utils)
library(diveRsity)
library(ggplot2)
library(stargazer)
library(stats)
library(utils)
library(tinytex)
library(quantreg)
library(broom)
library(ggpubr)


######################################################################

# Header = TRUE, if your excel file already has the headers in the  imported excel data frame #

mydata<-read.csv("Rdataformat_03132018.csv", header=TRUE)

View(mydata) #viewing all my data before carpentry

# Select only valuable columns

finaldf <- select(mydata, Location, FullScallop_Number, T1_height, T1_width,
                  T1_length, T2_height, T2_width, T2_length, T2_whole_weight, 
                  T2_body_weight, T2_adductor, sex, mc_1_mode, mc_2_mean, mc_3_max, sex)

# Break down data into site specific tables

TI_df <- slice(finaldf, 201:399)
DB_df <- slice(finaldf, 1:170)
NB_df <- slice(finaldf, 171:200)

DB_finaldf <- slice(DB_df, 2:170)

################ Variance Data Frame ###########################

var_df <- mydata %>% 
  select(acini_1,acini_2,acini_3,acini_4,acini_5,acini_6,acini_7,acini_8,acini_9,acini_10,
         acini_11,acini_12,acini_13,acini_14,acini_15,acini_16,acini_17,acini_18,acini_19,
         acini_20,acini_21,acini_22,acini_23,acini_24,acini_25,acini_26,acini_27,acini_28,
         acini_29,acini_30,acini_31,acini_32) 
          mutate(var_df, acini_var = var(1:322))
print(var_df)

# non dplyr #

mean(as.numeric(DB_finaldf$T2_adductor),na.rm=TRUE)


###################################

# Location vs. Adductor #

finaldf$Location <- as.factor(finaldf$Location)
head(finaldf)

finaldf$T2_adductor <- as.numeric(finaldf$T2_adductor)

ggplot(finaldf, aes(x=Location, y=T2_adductor)) +
  geom_boxplot(aes()) +
  geom_jitter(aes(colour=Location))+
  scale_color_brewer(palette = "Dark2")+
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle('Adductor Diameter vs. Site') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = 'bold')) +
  xlab("Site") +
  ylab("Adductor Diameter (mm)")

 kruskal.test(Location ~ T2_adductor, data = finaldf)

############ Location vs Length ##### With Kruskal-Wallis Test

ggplot(finaldf, aes(x=Location, y=T2_length)) +
  geom_boxplot() +
  geom_jitter(aes(colour = Location)) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  ggtitle('Length vs. Site') +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none' ) +
  ylab("Length (mm)") +
  xlab("Site")


kruskal.test(Location ~ T2_length, data = finaldf) #test of power, Krusty Wallace


# Adductor vs. maturation state #

finaldf$mc_1_mode <- as.factor(finaldf$mc_1_mode)
finaldf$T2_adductor <- as.numeric(finaldf$T2_adductor)

ggplot(finaldf, aes(mc_1_mode, T2_adductor), na.rm=TRUE) +
  geom_boxplot() +
  geom_jitter() +
  scale_x_discrete(breaks  = c(-1,0,1,2,3,4,5), labels = c(-1,0,1,2,3,4,5)) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  ggtitle('Maturation Stage vs. Adductor Diameter') +
  theme(plot.title = element_text(hjust = 0.05, size = 20, face= "bold")) +
  xlab("Maturation Stage") +
  ylab("Adductor Diameter (mm)") +
  theme(legend.position = 'none')

kruskal.test(mc_1_mode ~ T2_adductor, data = finaldf)


#### Maturation State vs. Location #######

finaldf$mc_1_mode <- as.numeric(finaldf$mc_1_mode)
finaldf$Location <- as.factor(finaldf$Location)

ggplot(finaldf, aes(Location, mc_1_mode, na.rm = T)) +
  geom_boxplot() +
  geom_jitter(aes(colour=Location)) +
  theme_bw()+
  ylab('Maturation Stage')+
  ggtitle("Maturation Stage vs. Site")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = 'none') +
  xlab('Site')+
  ylim(0,5)


kruskal.test(mc_1_mode ~ Location, data= finaldf)

######## Adductor vs. Length - TI #####################

ggplot(TI_df, aes(x=T2_length, y=T2_adductor)) +
  geom_jitter(stat ="identity") +
  theme_minimal()+
  scale_x_continuous(limits = c(0,85))+
  geom_smooth()


######## Adductor vs. Length - DB #####################

ggplot(DB_finaldf, aes(x=T2_length, y=T2_adductor)) +
  geom_jitter(stat ="identity") +
  theme_minimal()+
  scale_x_continuous(limits = c(0,85))

######## Adductor vs. Length - NB #####################

ggplot(NB_df, aes(x=T2_length, y=T2_adductor)) +
  geom_jitter(stat = 'identity')+
  theme_minimal()+
  scale_x_continuous(limits = c(0,85))


# Shell_height vs. maturation state #

finaldf$mc_1_mode <- as.factor(finaldf$mc_1_mode)

ggplot(finaldf, aes(x=mc_1_mode, y=T2_height)) +
  geom_boxplot() +
  theme_minimal()
kruskal.test(mc_1_mode ~ T2_height, data = finaldf)

# Shell height vs. adductor size, put in linear regression #


plot1<- ggplot(finaldf, aes(T2_length, T2_adductor)) +
  geom_jitter() +
  theme_classic()+
  xlab('Length (mm)')+
  ylab('Diameter (mm)')+
  ggtitle('Adductor Diameter vs. Shell Length')+
  geom_smooth(method='lm', aes(colour='black'))+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20)) +
  scale_y_continuous(limits = c(0,20))+
  scale_x_continuous(limits = c(15,90))+
  annotate('text', label = 'p-value = 1.863815e-37', x = 75, y = 1.5)
  
# p-value

length_p <- lm( T2_length~T2_adductor, data = finaldf) %>% 
  summary()
tidy(length_p)
glance(length_p)
 length_pvalue<- glance(length_p)$p.value
print(length_pvalue)

plot2<- ggplot(finaldf, aes(T2_height, T2_adductor)) +
  geom_jitter() +
  theme_classic()+
  xlab('Height (mm)')+
  ylab('Diameter (mm)')+
  ggtitle('Adductor Diameter vs. Shell Height')+
  geom_smooth(method='lm', aes(colour='black'))+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20))+
  annotate('text', label = 'p-value = 5.037393e-39', x = 75, y = 1.5)

# p-value

height_p <- lm( T2_height~T2_adductor, data = finaldf) %>% 
  summary()
tidy(height_p)
glance(height_p)
height_pvalue<- glance(height_p)$p.value
print(height_pvalue)




plot3<- ggplot(finaldf, aes(T2_width, T2_adductor)) +
  geom_jitter() +
  theme_classic()+
  xlab('Width (mm)')+
  ylab('Diameter (mm)')+
  ggtitle('Adductor Diameter vs. Shell Width')+
  geom_smooth(method='lm', aes(colour='black'))+
  theme(legend.position = 'none') +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold'))+
  annotate('text', label = 'p-value = 4.380831e-30', x = 20, y = 2)+
  xlim(8,25)

# p-value
 
 width_p <- lm( T2_width~T2_adductor, data = finaldf) %>% 
    summary()
  tidy(width_p)
  glance(width_p)
  width_pvalue<- glance(width_p)$p.value
  print(width_pvalue)
  
#build all the plots into one graph
multiplot(plot1,plot2,plot3,cols = 1)



# checking for correlation using cor()

corr_h_adductor <- cor(x=finaldf$T2_length, y=as.numeric(finaldf$T2_adductor))
print(corr_h_adductor)

corr_h_adductor <- cor(x=finaldf$T2_height, y=as.numeric(finaldf$T2_adductor))
print(corr_h_adductor)

corr_h_adductor <- cor(x=finaldf$T2_width, y=as.numeric(finaldf$T2_adductor))
print(corr_h_adductor)


#########  Sex vs. Maturation ######

finaldf$sex <- as.factor(finaldf$sex)
finaldf$mc_1_mode <- as.numeric(finaldf$mc_1_mode)

ggplot(finaldf, aes(sex, mc_1_mode))+
  geom_jitter()+
  geom_boxplot()

finaldf$mc_1_mode <- as.numeric(finaldf$mc_1_mode)

kruskal.test(mc_1_mode ~ sex, data = finaldf)


# Sex vs. Adductor

ggplot(finaldf, aes(sex, T2_adductor))+
  geom_jitter()+
  geom_boxplot()

kruskal.test(T2_adductor ~ sex, data = finaldf)

#no significant difference



######## Survivability ########

DB_survival<-(66) #values are percentages of the total
NB_survival<-(14)
TI_survival<-(98)

location<-c('DB', 'NB', 'TI')
percentage<- c(66,14,98)

dfsurvival<- data.frame(location,percentage)

dfsurvival$location <- as.factor(dfsurvival$location)
dfsurvival$percentage <- as.numeric(dfsurvival$percentage)

hist(percentage, dfsurvival)

ggplot(dfsurvival, aes(x=location, y=percentage)) + 
  geom_bar(stat = 'identity')+
  theme_bw()+
  xlab('Location')+
  ylab('Survival (%)')+
  ggtitle('Survivability')+
  theme(plot.title = element_text(hjust = 0.5))

#survival was significantly different based on the site

############ Body Weight Shell Weight  ############


ggplot(finaldf, aes(Location,T2_body_weight))+
  geom_boxplot()

kruskal.test(T2_body_weight ~ Location, data= finaldf)

ggplot(finaldf, aes(Location,T2_whole_weight))+
  geom_boxplot()


finaldf$sex <- as.factor(finaldf$sex)

ggplot(finaldf, aes(sex,mc_1_mode))+
  geom_boxplot()+
  theme_bw()
  
  

kruskal.test(T2_body_weight ~ sex, data= finaldf)

ggplot(DB_df, aes(sex,T2_whole_weight))+
  geom_boxplot()

ggplot(TI_df, aes(sex,T2_whole_weight))+
  geom_boxplot()

ggplot(NB_df, aes(sex,T2_whole_weight))+
  geom_boxplot()

kruskal.test(T2_whole_weight ~ sex, data= DB_df)
kruskal.test(T2_whole_weight ~ sex, data= TI_df)
kruskal.test(T2_whole_weight ~ sex, data= NB_df)

kruskal.test(T2_whole_weight ~ Location, data= finaldf)


# Kruskal Wallis test, like a non-parametric ANOVA

kruskal.test()



###### GLM and Stargazer Output #####

finaldf$sex <- relevel(finaldf$sex, "m")




finaldf$Location

finaldf$sex <- as.numeric(finaldf$sex)
finaldf$T2_length <- as.numeric(finaldf$T2_length)
finaldf$Location <- as.factor(finaldf$Location)
finaldf$T2_adductor <- as.numeric(finaldf$T2_adductor)


location_NB<- relevel(finaldf$Location, ref = "NB")
location_TI <- relevel(finaldf$Location, ref = "TI")

xmdl= lm(T2_adductor~location_NB+sex+T2_length, data=finaldf)
xmdl2= lm(T2_adductor~location_TI+sex+T2_length+mc_1_mode, data=finaldf)
plot(fitted(xmdl), residuals(xmdl))

xmdl4 <- lm(mc_1_mode ~ Location + sex + T2_length, data = finaldf) #maturation rae
summary(xmdl4)


print(xmdl)
summary(xmdl2)

stargazer(finaldf$mc_1_mode,type="text")

# OUTPUT OF TABLE
stargazer(xmdl, title = "Adductor Diameter: Linear Model",
          type= "text",
          out = "Linear_adductor.html",
          covariate.labels=c( "Dabob Bay","Totten Inlet","Sex","Shell Length (mm)", "Maturation Stage"),
          dep.var.labels=c("Adductor Diameter (mm)", ""),
          single.row = T)



#


length = lm(T2_length ~ length, data=finaldf)

##example



help("stargazer")

###### Variation between Site and Sex

setwd("~/Desktop/Capstone_2017:18/Excel Data")
finaldf_var <- read.csv("scallop_var_filtered_1.csv", header=T)

head(finaldf_var)

ggplot(finaldf_var, aes(sex, Variation), na.rm=TRUE) +
  geom_jitter(aes(colour = sex)) +
  scale_color_brewer(palette = 'Dark2')
  theme_bw() +
  ggtitle('Variation by Sex') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Sex") +
  ylab("Variation per Individual") +
  theme(legend.position = 'none')
  
# Krusty Wallace Test
  
  kruskal.test(sex ~ Variation, data = finaldf_var)

  # Variation within Location
  
ggplot(finaldf_var, aes(Location, Variation), na.rm=TRUE) +
    geom_jitter(aes(colour = Location)) +
    theme_classic() +
    scale_color_brewer(palette = 'Dark2')+
    ggtitle('Variation by Site') +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Site") +
    ylab("Variation per Individual") +
    theme(legend.position = 'none')
  
kruskal.test(Location ~ Variation, data = finaldf_var)












