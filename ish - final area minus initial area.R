#################################################
# Analysis of duckweed experiment               #
# Conducted - Summer 2013                       #
#                                               #
# Final Area - Initial Area                     #    
#################################################

library(ggplot2)

###########################
# Mean area at day 0      #
# By each treatment combo #
###########################
mean_area_final_minus_initial_plot <- ggplot(summary_data_area_final_minus_initial, aes(x=species, y=final_minus_initial)) 
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + geom_errorbar(aes(ymin=final_minus_initial-se, ymax=final_minus_initial+se), width=0.1)
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + geom_point(size=3)
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + facet_grid(nitrogen ~ phosphorus)
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + ylab("final area / initial area")
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + xlab("species")
mean_area_final_minus_initial_plot <- mean_area_final_minus_initial_plot + theme_gray(base_size=18)
mean_area_final_minus_initial_plot

#####################
# Preliminary anova #
# Three-way         #
#####################
area_final_minus_initial_anova <- aov(final_minus_initial ~ species*Nutr*Temp, data=data_raw)
summary(area_final_minus_initial_anova)
TukeyHSD(area_final_minus_initial_anova)

#####################
# Examine residuals #
#####################
hist(resid(area_final_minus_initial_anova)) # plot a histogram 

qqnorm(resid(area_final_minus_initial_anova)) # QQ plot 
qqline(resid(area_final_minus_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_minus_initial_anova)) # p-value = 3.969e-09 









######################################
# ANOVA @ each treatment combination #
######################################
#####################
# Low Nutr Low temp #
#####################
final_minus_initial_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="low" & data$Temp=="18"))
summary(final_minus_initial_anova)
TukeyHSD(final_minus_initial_anova)

hist(resid(final_minus_initial_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_anova)) # QQ plot 
qqline(resid(final_minus_initial_anova)) 

# nu18 hypothesis = sample came from a norma18y distributed population 
shapiro.test(resid(final_minus_initial_anova)) # p-value =  0.4188

###############
# Med Nutr Low Temp #
###############
final_minus_initial_M18_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="med" & data$Temp=="18"))
summary(final_minus_initial_M18_anova)
TukeyHSD(final_minus_initial_M18_anova)

hist(resid(final_minus_initial_M18_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_M18_anova)) # QQ plot 
qqline(resid(final_minus_initial_M18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_M18_anova)) # p-value =  0.03546

################
# High Nutr Low Temp #
################
final_minus_initial_H18_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="high" & data$Temp=="18"))
summary(final_minus_initial_H18_anova)
TukeyHSD(final_minus_initial_H18_anova)

hist(resid(final_minus_initial_H18_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_H18_anova)) # QQ plot 
qqline(resid(final_minus_initial_H18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_H18_anova)) # p-value =  0.426

###############
# Low Nutr Med Temp #
###############
final_minus_initial_L24_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="low" & data$Temp=="24"))
summary(final_minus_initial_L24_anova)
TukeyHSD(final_minus_initial_L24_anova)

hist(resid(final_minus_initial_L24_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_L24_anova)) # QQ plot 
qqline(resid(final_minus_initial_L24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_L24_anova)) # p-value =  0.3333

################
# Low Nutr High Temp #
################
final_minus_initial_L30_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="low" & data$Temp=="30"))
summary(final_minus_initial_L30_anova)
TukeyHSD(final_minus_initial_L30_anova)

hist(resid(final_minus_initial_L30_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_L30_anova)) # QQ plot 
qqline(resid(final_minus_initial_L30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_L30_anova)) # p-value =  0.005253

###############
# Med Nutr Med Temp #
###############
final_minus_initial_M24_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="med" & data$Temp=="24"))
summary(final_minus_initial_M24_anova)
TukeyHSD(final_minus_initial_M24_anova)

hist(resid(final_minus_initial_M24_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_M24_anova)) # QQ plot 
qqline(resid(final_minus_initial_M24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_M24_anova)) # p-value =  0.06162

#################
# High Nutr High Temp #
#################
final_minus_initial_H30_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="high" & data$Temp=="30"))
summary(final_minus_initial_H30_anova)
TukeyHSD(final_minus_initial_H30_anova)

hist(resid(final_minus_initial_H30_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_H30_anova)) # QQ plot 
qqline(resid(final_minus_initial_H30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_H30_anova)) # p-value =  0.04204

################
# Med Nutr High Temp #
################
final_minus_initial_M30_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="med" & data$Temp=="30"))
summary(final_minus_initial_M30_anova)
TukeyHSD(final_minus_initial_M30_anova)

hist(resid(final_minus_initial_M30_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_M30_anova)) # QQ plot 
qqline(resid(final_minus_initial_M30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_M30_anova)) # p-value =  0.7518

################
# High Nutr Med Temp #
################
final_minus_initial_H24_anova <- aov(final_minus_initial ~ species, data=subset(data_raw, data$Nutr=="high" & data$Temp=="24"))
summary(final_minus_initial_H24_anova)
TukeyHSD(final_minus_initial_H24_anova)

hist(resid(final_minus_initial_H24_anova)) # plot a histogram 

qqnorm(resid(final_minus_initial_H24_anova)) # QQ plot 
qqline(resid(final_minus_initial_H24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(final_minus_initial_H24_anova)) # p-value =  0.5111

