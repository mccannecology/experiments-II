##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# RGR                                    #
# average within a replicate             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr)

############
# avgRGR  #
# Raw data #
############
# colour
raw_avgRGR_plot <- ggplot(data_raw, aes(x=species,y=avgRGR)) + geom_point() 
raw_avgRGR_plot <- raw_avgRGR_plot + facet_grid(Nutr ~ Temp)
raw_avgRGR_plot <- raw_avgRGR_plot + ylab("Mean RGR")
raw_avgRGR_plot 

############
# avgRGR  #
# Average  #
############
# colour
mean_avgRGR_plot <- ggplot(summary_data_avgRGR, aes(x=species,y=avgRGR)) + geom_point() 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + facet_grid(Nutr ~ Temp)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab("Mean RGR")
mean_avgRGR_plot 

#####################
# Preliminary anova #
# Y = avgRGR       #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
#####################
avgRGR_anova <- aov(avgRGR ~ species*as.factor(Temp)*Nutr, data=data_raw)
summary(avgRGR_anova)
posthoc_avgRGR_anova <- TukeyHSD(avgRGR_anova)
posthoc_avgRGR_anova

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(avgRGR_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova)) 
qqline(resid(avgRGR_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova)) # p-value = 0.0261 # Residuals are not noramlly distributed 





######################################
# ANOVA @ each treatment combination #
######################################
#####################
# Low Nutr Low temp #
#####################
avgRGR_18_anova <- aov(avgRGR~ species, data=subset(data, data$Nutr=="low" & data$Temp=="18"))
summary(avgRGR_18_anova)
TukeyHSD(avgRGR_18_anova)

hist(resid(avgRGR_18_anova)) # plot a histogram 

qqnorm(resid(avgRGR_18_anova)) # QQ plot 
qqline(resid(avgRGR_18_anova)) 

# nu18 hypothesis = sample came from a norma18y distributed population 
shapiro.test(resid(avgRGR_18_anova)) # p-value =  0.4188

###############
# Med Nutr Low Temp #
###############
avgRGR_M18_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="med" & data$Temp=="18"))
summary(avgRGR_M18_anova)
TukeyHSD(avgRGR_M18_anova)

hist(resid(avgRGR_M18_anova)) # plot a histogram 

qqnorm(resid(avgRGR_M18_anova)) # QQ plot 
qqline(resid(avgRGR_M18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_M18_anova)) # p-value =  0.03546

################
# High Nutr Low Temp #
################
avgRGR_H18_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="high" & data$Temp=="18"))
summary(avgRGR_H18_anova)
TukeyHSD(avgRGR_H18_anova)

hist(resid(avgRGR_H18_anova)) # plot a histogram 

qqnorm(resid(avgRGR_H18_anova)) # QQ plot 
qqline(resid(avgRGR_H18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_H18_anova)) # p-value =  0.426

###############
# Low Nutr Med Temp #
###############
avgRGR_L24_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="low" & data$Temp=="24"))
summary(avgRGR_L24_anova)
TukeyHSD(avgRGR_L24_anova)

hist(resid(avgRGR_L24_anova)) # plot a histogram 

qqnorm(resid(avgRGR_L24_anova)) # QQ plot 
qqline(resid(avgRGR_L24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_L24_anova)) # p-value =  0.3333

################
# Low Nutr High Temp #
################
avgRGR_L30_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="low" & data$Temp=="30"))
summary(avgRGR_L30_anova)
TukeyHSD(avgRGR_L30_anova)

hist(resid(avgRGR_L30_anova)) # plot a histogram 

qqnorm(resid(avgRGR_L30_anova)) # QQ plot 
qqline(resid(avgRGR_L30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_L30_anova)) # p-value =  0.005253

###############
# Med Nutr Med Temp #
###############
avgRGR_M24_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="med" & data$Temp=="24"))
summary(avgRGR_M24_anova)
TukeyHSD(avgRGR_M24_anova)

hist(resid(avgRGR_M24_anova)) # plot a histogram 

qqnorm(resid(avgRGR_M24_anova)) # QQ plot 
qqline(resid(avgRGR_M24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_M24_anova)) # p-value =  0.06162

#################
# High Nutr High Temp #
#################
avgRGR_H30_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="high" & data$Temp=="30"))
summary(avgRGR_H30_anova)
TukeyHSD(avgRGR_H30_anova)

hist(resid(avgRGR_H30_anova)) # plot a histogram 

qqnorm(resid(avgRGR_H30_anova)) # QQ plot 
qqline(resid(avgRGR_H30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_H30_anova)) # p-value =  0.04204

################
# Med Nutr High Temp #
################
avgRGR_M30_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="med" & data$Temp=="30"))
summary(avgRGR_M30_anova)
TukeyHSD(avgRGR_M30_anova)

hist(resid(avgRGR_M30_anova)) # plot a histogram 

qqnorm(resid(avgRGR_M30_anova)) # QQ plot 
qqline(resid(avgRGR_M30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_M30_anova)) # p-value =  0.7518

################
# High Nutr Med Temp #
################
avgRGR_H24_anova <- aov(avgRGR ~ species, data=subset(data, data$Nutr=="high" & data$Temp=="24"))
summary(avgRGR_H24_anova)
TukeyHSD(avgRGR_H24_anova)

hist(resid(avgRGR_H24_anova)) # plot a histogram 

qqnorm(resid(avgRGR_H24_anova)) # QQ plot 
qqline(resid(avgRGR_H24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_H24_anova)) # p-value =  0.5111

