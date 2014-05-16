##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# RGR                                    #
# max within a replicate                 #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr)

############
# maxRGR  #
# Raw data #
############
# colour
raw_maxRGR_plot <- ggplot(data_raw, aes(x=species,y=maxRGR)) + geom_point() 
raw_maxRGR_plot <- raw_maxRGR_plot + facet_grid(Nutr ~ Temp)
raw_maxRGR_plot <- raw_maxRGR_plot + ylab("Max RGR")
raw_maxRGR_plot 

############
# maxRGR  #
# Average  #
############
# colour
mean_maxRGR_plot <- ggplot(summary_data_maxRGR, aes(x=species,y=maxRGR)) + geom_point() 
mean_maxRGR_plot <- mean_maxRGR_plot + geom_errorbar(aes(ymin=maxRGR-se, ymax=maxRGR+se), width=0.1)
mean_maxRGR_plot <- mean_maxRGR_plot + facet_grid(Nutr ~ Temp)
mean_maxRGR_plot <- mean_maxRGR_plot + ylab("Max RGR")
mean_maxRGR_plot 

#####################
# Preliminary anova #
# Y = maxRGR       #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
#####################
maxRGR_anova <- aov(maxRGR ~ species*as.factor(Temp)*Nutr, data=data_raw)
summary(maxRGR_anova)
posthoc_maxRGR_anova<- TukeyHSD(maxRGR_anova)
posthoc_maxRGR_anova

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(maxRGR_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova)) 
qqline(resid(maxRGR_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova)) # p-value = 0.0261 # Residuals are not noramlly distributed 







######################################
# ANOVA @ each treatment combination #
######################################
#####################
# Low Nutr Low temp #
#####################
maxRGR_18_anova <- aov(maxRGR~ species, data=subset(data, data$Nutr=="low" & data$Temp=="18"))
summary(maxRGR_18_anova)
TukeyHSD(maxRGR_18_anova)

hist(resid(maxRGR_18_anova)) # plot a histogram 

qqnorm(resid(maxRGR_18_anova)) # QQ plot 
qqline(resid(maxRGR_18_anova)) 

# nu18 hypothesis = sample came from a norma18y distributed population 
shapiro.test(resid(maxRGR_18_anova)) # p-value =  0.4188

###############
# Med Nutr Low Temp #
###############
maxRGR_M18_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="med" & data$Temp=="18"))
summary(maxRGR_M18_anova)
TukeyHSD(maxRGR_M18_anova)

hist(resid(maxRGR_M18_anova)) # plot a histogram 

qqnorm(resid(maxRGR_M18_anova)) # QQ plot 
qqline(resid(maxRGR_M18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_M18_anova)) # p-value =  0.03546

################
# High Nutr Low Temp #
################
maxRGR_H18_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="high" & data$Temp=="18"))
summary(maxRGR_H18_anova)
TukeyHSD(maxRGR_H18_anova)

hist(resid(maxRGR_H18_anova)) # plot a histogram 

qqnorm(resid(maxRGR_H18_anova)) # QQ plot 
qqline(resid(maxRGR_H18_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_H18_anova)) # p-value =  0.426

###############
# Low Nutr Med Temp #
###############
maxRGR_L24_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="low" & data$Temp=="24"))
summary(maxRGR_L24_anova)
TukeyHSD(maxRGR_L24_anova)

hist(resid(maxRGR_L24_anova)) # plot a histogram 

qqnorm(resid(maxRGR_L24_anova)) # QQ plot 
qqline(resid(maxRGR_L24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_L24_anova)) # p-value =  0.3333

################
# Low Nutr High Temp #
################
maxRGR_L30_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="low" & data$Temp=="30"))
summary(maxRGR_L30_anova)
TukeyHSD(maxRGR_L30_anova)

hist(resid(maxRGR_L30_anova)) # plot a histogram 

qqnorm(resid(maxRGR_L30_anova)) # QQ plot 
qqline(resid(maxRGR_L30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_L30_anova)) # p-value =  0.005253

###############
# Med Nutr Med Temp #
###############
maxRGR_M24_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="med" & data$Temp=="24"))
summary(maxRGR_M24_anova)
TukeyHSD(maxRGR_M24_anova)

hist(resid(maxRGR_M24_anova)) # plot a histogram 

qqnorm(resid(maxRGR_M24_anova)) # QQ plot 
qqline(resid(maxRGR_M24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_M24_anova)) # p-value =  0.06162

#################
# High Nutr High Temp #
#################
maxRGR_H30_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="high" & data$Temp=="30"))
summary(maxRGR_H30_anova)
TukeyHSD(maxRGR_H30_anova)

hist(resid(maxRGR_H30_anova)) # plot a histogram 

qqnorm(resid(maxRGR_H30_anova)) # QQ plot 
qqline(resid(maxRGR_H30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_H30_anova)) # p-value =  0.04204

################
# Med Nutr High Temp #
################
maxRGR_M30_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="med" & data$Temp=="30"))
summary(maxRGR_M30_anova)
TukeyHSD(maxRGR_M30_anova)

hist(resid(maxRGR_M30_anova)) # plot a histogram 

qqnorm(resid(maxRGR_M30_anova)) # QQ plot 
qqline(resid(maxRGR_M30_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_M30_anova)) # p-value =  0.7518

################
# High Nutr Med Temp #
################
maxRGR_H24_anova <- aov(maxRGR ~ species, data=subset(data, data$Nutr=="high" & data$Temp=="24"))
summary(maxRGR_H24_anova)
TukeyHSD(maxRGR_H24_anova)

hist(resid(maxRGR_H24_anova)) # plot a histogram 

qqnorm(resid(maxRGR_H24_anova)) # QQ plot 
qqline(resid(maxRGR_H24_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_H24_anova)) # p-value =  0.5111

