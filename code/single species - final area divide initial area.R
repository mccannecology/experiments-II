#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Final Area / Initial Area                     #    
#################################################

library(ggplot2)

# check out the data that you will use 
head(summary_data_final_divide_initial)

###########################
# Mean area at day 0      #
# By each treatment combo #
###########################
mean_area_final_divide_initial_plot <- ggplot(summary_data_final_divide_initial, aes(x=species, y=final_divide_initial)) 
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_errorbar(aes(ymin=final_divide_initial-se, ymax=final_divide_initial+se), width=0.1)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_point(size=3)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + facet_grid(Temp ~ Nutr)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + ylab("final area / initial area")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + xlab("species")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + theme_gray(base_size=18)
mean_area_final_divide_initial_plot

ggsave(filename = "mean_area_final_divide_initial_plot.pdf", mean_area_final_divide_initial_plot, height=11, width=8)

#####################
# Preliminary anova #
# Three-way         #
#####################
area_final_divide_initial_anova <- aov(final_divide_initial ~ species*Nutr*Temp, data=data_raw)
summary(area_final_divide_initial_anova)
TukeyHSD(area_final_divide_initial_anova)

#####################
# Examine residuals #
#####################
hist(resid(area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_anova)) # p-value = 1.395e-07 

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_raw$final_divide_initial ~ data_raw$species * data_raw$Temp * data_raw$Nutr) # p-value = 0.01662


#################################
# transform and re-do the anova #
#################################
#############################
# try a sqrt transformation #
#############################
sqrt_area_final_divide_initial_anova <- aov(sqrt(final_divide_initial) ~ species*Nutr*Temp, data=data_raw)
summary(sqrt_area_final_divide_initial_anova)
posthoc_sqrt_area_final_divide_initial_anova<- TukeyHSD(sqrt_area_final_divide_initial_anova)
posthoc_sqrt_area_final_divide_initial_anova

#####################
# Examine residuals #
#####################
hist(resid(sqrt_area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(sqrt_area_final_divide_initial_anova)) # QQ plot 
qqline(resid(sqrt_area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(sqrt_area_final_divide_initial_anova)) # p-value = 0.004994 

#################################
# transform and re-do the anova #
#################################
###############################
# try a logx+1 transformation #
###############################
logx1_area_final_divide_initial_anova <- aov(log(final_divide_initial+1) ~ species*Nutr*Temp, data=data_raw)
summary(logx1_area_final_divide_initial_anova)
posthoc_logx1_area_final_divide_initial_anova <- TukeyHSD(logx1_area_final_divide_initial_anova)
posthoc_logx1_area_final_divide_initial_anova[7]

#####################
# Examine residuals #
#####################
hist(resid(logx1_area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(logx1_area_final_divide_initial_anova)) # QQ plot 
qqline(resid(logx1_area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(logx1_area_final_divide_initial_anova)) # p-value = 0.01497 

#################################
# transform and re-do the anova #
#################################
###############################
# try a power  transformation #
###############################
library(car)
powerTransform(final_divide_initial ~ species * Nutr * Temp, data=data_raw)
power <- 0.1910548

# add the power transformation of stand
data_raw$power_final_divide_initial <- ((data_raw$final_divide_initial)^power - 1) / power 

power_area_final_divide_initial_anova <- aov(power_final_divide_initial ~ species * Nutr * Temp, data=data_raw)
summary(power_area_final_divide_initial_anova)
posthoc_power_area_final_divide_initial_anova <- TukeyHSD(power_area_final_divide_initial_anova)
posthoc_power_area_final_divide_initial_anova

hist(resid(power_area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(power_area_final_divide_initial_anova)) # QQ plot 
qqline(resid(power_area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_area_final_divide_initial_anova)) # p-value =  0.001228




######################################
# ANOVA @ each treatment combination #
######################################
#####################
# Low Nutr Low temp #
#####################
area_final_divide_initial_L18_anova <- aov(final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
summary(area_final_divide_initial_L18_anova)
TukeyHSD(area_final_divide_initial_L18_anova)

hist(resid(area_final_divide_initial_L18_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_L18_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_L18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_L18_anova)) # p-value =  0.0241

# try a sqrt+1 transformation 
sqrt_area_final_divide_initial_L18_anova <- aov(sqrt(final_divide_initial+1) ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
summary(sqrt_area_final_divide_initial_L18_anova)
posthoc_sqrt_area_final_divide_initial_L18_anova<- TukeyHSD(sqrt_area_final_divide_initial_L18_anova)
posthoc_sqrt_area_final_divide_initial_L18_anova

# Examine residuals 
hist(resid(sqrt_area_final_divide_initial_L18_anova)) # plot a histogram 

qqnorm(resid(sqrt_area_final_divide_initial_L18_anova)) # QQ plot 
qqline(resid(sqrt_area_final_divide_initial_L18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(sqrt_area_final_divide_initial_L18_anova)) # p-value = 0.01924

# try a power transformation 
library(car)
powerTransform(final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
power <- -0.7105515 

# add the power transformation
data_raw$power_final_divide_initial <- ((data_raw$final_divide_initial)^power - 1) / power 

power_area_final_divide_initial_L18_anova <- aov(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
summary(power_area_final_divide_initial_L18_anova)
posthoc_power_area_final_divide_initial_L18_anova <- TukeyHSD(power_area_final_divide_initial_L18_anova)
posthoc_power_area_final_divide_initial_L18_anova

# Examine residuals 
hist(resid(power_area_final_divide_initial_L18_anova)) # plot a histogram 

qqnorm(resid(power_area_final_divide_initial_L18_anova)) # QQ plot 
qqline(resid(power_area_final_divide_initial_L18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_area_final_divide_initial_L18_anova)) # p-value = 0.04694

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18")) 

######################
# Med Nutr Low Temp  #
######################
area_final_divide_initial_M18_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="18"))
summary(area_final_divide_initial_M18_anova)
TukeyHSD(area_final_divide_initial_M18_anova)

hist(resid(area_final_divide_initial_M18_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_M18_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_M18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_M18_anova)) # p-value =  0.1601

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="18"))

######################
# High Nutr Low Temp #
######################
area_final_divide_initial_H18_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
summary(area_final_divide_initial_H18_anova)
TukeyHSD(area_final_divide_initial_H18_anova)

hist(resid(area_final_divide_initial_H18_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_H18_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_H18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_H18_anova)) # p-value =  0.3485

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))

######################
# Low Nutr Med Temp  #
######################
area_final_divide_initial_L24_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))
summary(area_final_divide_initial_L24_anova)
TukeyHSD(area_final_divide_initial_L24_anova)

hist(resid(area_final_divide_initial_L24_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_L24_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_L24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_L24_anova)) # p-value =  0.1932

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))

######################
# Low Nutr High Temp #
######################
area_final_divide_initial_L30_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))
summary(area_final_divide_initial_L30_anova)
TukeyHSD(area_final_divide_initial_L30_anova)

hist(resid(area_final_divide_initial_L30_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_L30_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_L30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_L30_anova)) # p-value =  0.3845

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))

######################
# Med Nutr Med Temp  #
######################
area_final_divide_initial_M24_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))
summary(area_final_divide_initial_M24_anova)
TukeyHSD(area_final_divide_initial_M24_anova)

hist(resid(area_final_divide_initial_M24_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_M24_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_M24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_M24_anova)) # p-value =  0.03133

# try a power transformation 
library(car)
powerTransform(final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))
power <- -1.057338 

# add the power transformation
data_raw$power_final_divide_initial <- ((data_raw$final_divide_initial)^power - 1) / power 

power_area_final_divide_initial_M24_anova <- aov(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))
summary(power_area_final_divide_initial_M24_anova)
posthoc_power_area_final_divide_initial_M24_anova <- TukeyHSD(power_area_final_divide_initial_M24_anova)
posthoc_power_area_final_divide_initial_M24_anova

# Examine residuals 
hist(resid(power_area_final_divide_initial_M24_anova)) # plot a histogram 

qqnorm(resid(power_area_final_divide_initial_M24_anova)) # QQ plot 
qqline(resid(power_area_final_divide_initial_M24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_area_final_divide_initial_M24_anova)) # p-value = 0.233

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))

#######################
# High Nutr High Temp #
#######################
area_final_divide_initial_H30_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="30"))
summary(area_final_divide_initial_H30_anova)
TukeyHSD(area_final_divide_initial_H30_anova)

hist(resid(area_final_divide_initial_H30_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_H30_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_H30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_H30_anova)) # p-value =  0.06673

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="30"))

######################
# Med Nutr High Temp #
######################
area_final_divide_initial_M30_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30"))
summary(area_final_divide_initial_M30_anova)
TukeyHSD(area_final_divide_initial_M30_anova)

hist(resid(area_final_divide_initial_M30_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_M30_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_M30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_M30_anova)) # p-value =  0.09426

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30"))

######################
# High Nutr Med Temp #
######################
area_final_divide_initial_H24_anova <- aov(final_divide_initial~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="24"))
summary(area_final_divide_initial_H24_anova)
TukeyHSD(area_final_divide_initial_H24_anova)

hist(resid(area_final_divide_initial_H24_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_H24_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_H24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_H24_anova)) # p-value =  0.6954

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(final_divide_initial ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="24"))
