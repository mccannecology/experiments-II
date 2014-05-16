##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# Day 0 area                             # 
##########################################
library(ggplot2)
library(gridExtra)

# check out the data that you will use 
head(summary_data_area)

# re-order my treatments so they go from low to high
summary_data_area$Nutr <- factor(summary_data_area$Nutr , levels=c("low","med","high"))

###########################
# Mean area at day 0      #
# By each treatment combo #
###########################
mean_area_day0_plot <- ggplot(subset(summary_data_area, summary_data_area$day == 0), aes(x=species, y=area)) 
mean_area_day0_plot <- mean_area_day0_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_day0_plot <- mean_area_day0_plot + geom_point(size=3)
mean_area_day0_plot <- mean_area_day0_plot + facet_grid(Temp ~ Nutr)
mean_area_day0_plot <- mean_area_day0_plot + ylab("initial area (sq. mm)")
mean_area_day0_plot <- mean_area_day0_plot + xlab("species")
mean_area_day0_plot <- mean_area_day0_plot + theme_gray(base_size=18)
mean_area_day0_plot

#####################
# Preliminary anova #
#####################
# Y = Area day 0
# Treatments: species, nutrients, temperature       
area_day0_anova <- aov(area0 ~ species*Temp*Nutr, data=data_raw)
summary(area_day0_anova)
TukeyHSD(area_day0_anova)

#####################
# Examine residuals #
#####################
hist(resid(area_day0_anova)) # plot a histogram 

qqnorm(resid(area_day0_anova)) # QQ plot 
qqline(resid(area_day0_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_day0_anova)) # p-value = 





######################################
# ANOVA @ each treatment combination #
######################################
#####################
# Low Nutr Low temp #
#####################
area0_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="low" & data$Temp=="18"))
summary(area0_anova)
TukeyHSD(area0_anova)

hist(resid(area0_anova)) # plot a histogram 

qqnorm(resid(area0_anova)) # QQ plot 
qqline(resid(area0_anova)) 

# nu18 hypothesis = sample came from a norma18y distributed population 
shapiro.test(resid(area0_anova)) # p-value =  0.4188

###############
# Med Nutr Low Temp #
###############
area0_M18_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="med" & data$Temp=="18"))
summary(area0_M18_anova)
TukeyHSD(area0_M18_anova)

hist(resid(area0_M18_anova)) # plot a histogram 

qqnorm(resid(area0_M18_anova)) # QQ plot 
qqline(resid(area0_M18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_M18_anova)) # p-value =  0.03546

################
# High Nutr Low Temp #
################
area0_H18_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="high" & data$Temp=="18"))
summary(area0_H18_anova)
TukeyHSD(area0_H18_anova)

hist(resid(area0_H18_anova)) # plot a histogram 

qqnorm(resid(area0_H18_anova)) # QQ plot 
qqline(resid(area0_H18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_H18_anova)) # p-value =  0.426

###############
# Low Nutr Med Temp #
###############
area0_L24_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="low" & data$Temp=="24"))
summary(area0_L24_anova)
TukeyHSD(area0_L24_anova)

hist(resid(area0_L24_anova)) # plot a histogram 

qqnorm(resid(area0_L24_anova)) # QQ plot 
qqline(resid(area0_L24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_L24_anova)) # p-value =  0.3333

################
# Low Nutr High Temp #
################
area0_L30_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="low" & data$Temp=="30"))
summary(area0_L30_anova)
TukeyHSD(area0_L30_anova)

hist(resid(area0_L30_anova)) # plot a histogram 

qqnorm(resid(area0_L30_anova)) # QQ plot 
qqline(resid(area0_L30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_L30_anova)) # p-value =  0.005253

###############
# Med Nutr Med Temp #
###############
area0_M24_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="med" & data$Temp=="24"))
summary(area0_M24_anova)
TukeyHSD(area0_M24_anova)

hist(resid(area0_M24_anova)) # plot a histogram 

qqnorm(resid(area0_M24_anova)) # QQ plot 
qqline(resid(area0_M24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_M24_anova)) # p-value =  0.06162

#################
# High Nutr High Temp #
#################
area0_H30_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="high" & data$Temp=="30"))
summary(area0_H30_anova)
TukeyHSD(area0_H30_anova)

hist(resid(area0_H30_anova)) # plot a histogram 

qqnorm(resid(area0_H30_anova)) # QQ plot 
qqline(resid(area0_H30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_H30_anova)) # p-value =  0.04204

################
# Med Nutr High Temp #
################
area0_M30_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="med" & data$Temp=="30"))
summary(area0_M30_anova)
TukeyHSD(area0_M30_anova)

hist(resid(area0_M30_anova)) # plot a histogram 

qqnorm(resid(area0_M30_anova)) # QQ plot 
qqline(resid(area0_M30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_M30_anova)) # p-value =  0.7518

################
# High Nutr Med Temp #
################
area0_H24_anova <- aov(area0 ~ species, data=subset(data_raw, data$Nutr=="high" & data$Temp=="24"))
summary(area0_H24_anova)
TukeyHSD(area0_H24_anova)

hist(resid(area0_H24_anova)) # plot a histogram 

qqnorm(resid(area0_H24_anova)) # QQ plot 
qqline(resid(area0_H24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_H24_anova)) # p-value =  0.5111


