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

ggsave(filename = "mean_area_day0_plot.pdf", mean_area_day0_plot, height=11, width=8)

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
shapiro.test(resid(area_day0_anova)) # p-value = 0.1297





######################################
# ANOVA @ each treatment combination #
######################################
#####################
# Low Nutr Low temp #
#####################
area0_L18_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
summary(area0_L18_anova)
TukeyHSD(area0_L18_anova)

hist(resid(area0_L18_anova)) # plot a histogram 

qqnorm(resid(area0_L18_anova)) # QQ plot 
qqline(resid(area0_L18_anova)) 

# nu18 hypothesis = sample came from a norma18y distributed population 
shapiro.test(resid(area0_L18_anova)) # p-value =  0.5

#####################
# Med Nutr Low Temp #
#####################
area0_M18_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="18"))
summary(area0_M18_anova)
TukeyHSD(area0_M18_anova)

hist(resid(area0_M18_anova)) # plot a histogram 

qqnorm(resid(area0_M18_anova)) # QQ plot 
qqline(resid(area0_M18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_M18_anova)) # p-value =  0.1371

################
# High Nutr Low Temp #
################
area0_H18_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
summary(area0_H18_anova)
TukeyHSD(area0_H18_anova)

hist(resid(area0_H18_anova)) # plot a histogram 

qqnorm(resid(area0_H18_anova)) # QQ plot 
qqline(resid(area0_H18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_H18_anova)) # p-value =  0.02327

#######################################
# power transform and re-do the anova #
#######################################
# figure out the best power transformation 
library(car)
powerTransform(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
power <- -0.4476651

# add the power transformation of stand
data_raw$power_area0 <- ((data_raw$area0)^power - 1) / power 

area0_H18_anova_power <- aov(power_area0 ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
summary(area0_H18_anova_power)
posthoc_area0_H18_anova_power <- TukeyHSD(area0_H18_anova_power)
posthoc_area0_H18_anova_power

hist(resid(area0_H18_anova_power)) # plot a histogram 

qqnorm(resid(area0_H18_anova_power)) # QQ plot 
qqline(resid(area0_H18_anova_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_H18_anova_power)) # p-value =  0.1288


###############
# Low Nutr Med Temp #
###############
area0_L24_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))
summary(area0_L24_anova)
TukeyHSD(area0_L24_anova)

hist(resid(area0_L24_anova)) # plot a histogram 

qqnorm(resid(area0_L24_anova)) # QQ plot 
qqline(resid(area0_L24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_L24_anova)) # p-value =  0.216

################
# Low Nutr High Temp #
################
area0_L30_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))
summary(area0_L30_anova)
TukeyHSD(area0_L30_anova)

hist(resid(area0_L30_anova)) # plot a histogram 

qqnorm(resid(area0_L30_anova)) # QQ plot 
qqline(resid(area0_L30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_L30_anova)) # p-value =  0.8535

###############
# Med Nutr Med Temp #
###############
area0_M24_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))
summary(area0_M24_anova)
TukeyHSD(area0_M24_anova)

hist(resid(area0_M24_anova)) # plot a histogram 

qqnorm(resid(area0_M24_anova)) # QQ plot 
qqline(resid(area0_M24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_M24_anova)) # p-value =  0.8311

#################
# High Nutr High Temp #
#################
area0_H30_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="30"))
summary(area0_H30_anova)
TukeyHSD(area0_H30_anova)

hist(resid(area0_H30_anova)) # plot a histogram 

qqnorm(resid(area0_H30_anova)) # QQ plot 
qqline(resid(area0_H30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_H30_anova)) # p-value =  0.7446

################
# Med Nutr High Temp #
################
area0_M30_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30"))
summary(area0_M30_anova)
TukeyHSD(area0_M30_anova)

hist(resid(area0_M30_anova)) # plot a histogram 

qqnorm(resid(area0_M30_anova)) # QQ plot 
qqline(resid(area0_M30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_M30_anova)) # p-value =  0.6779

################
# High Nutr Med Temp #
################
area0_H24_anova <- aov(area0 ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="24"))
summary(area0_H24_anova)
TukeyHSD(area0_H24_anova)

hist(resid(area0_H24_anova)) # plot a histogram 

qqnorm(resid(area0_H24_anova)) # QQ plot 
qqline(resid(area0_H24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area0_H24_anova)) # p-value =  0.1432


