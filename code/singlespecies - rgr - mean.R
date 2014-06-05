##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# avgRGR                                 #
# average within a replicate             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_raw)
head(summary_data_avgRGR)

############
# avgRGR   #
# Average  #
############
# colour
mean_avgRGR_plot <- ggplot(summary_data_avgRGR, aes(x=species,y=avgRGR)) + geom_point() 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + facet_grid(Temp ~ Nutr)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab("Average RGR")
mean_avgRGR_plot <- mean_avgRGR_plot + theme_gray(base_size=18)
mean_avgRGR_plot 

ggsave(filename = "mean_avgRGR_plot.pdf", mean_avgRGR_plot, height=11, width=8)

#####################
# Preliminary anova #
# Y = avgRGR        #
# Treatments:       #
# species,          #
# Nutr,         #
# Temp,       #
#####################
avgRGR_anova <- aov(avgRGR ~ species*Nutr*Temp, data=data_raw)
summary(avgRGR_anova)
posthoc_avgRGR_anova <- TukeyHSD(avgRGR_anova)
posthoc_avgRGR_anova
threeway <- posthoc_avgRGR_anova[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

# Examine residuals #
# plot a histogram 
# looks normal-ish
hist(resid(avgRGR_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova)) 
qqline(resid(avgRGR_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova)) # p-value = 0.01016 # Residuals are not noramlly distributed 

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_raw$avgRGR ~ data_raw$species * data_raw$Temp * data_raw$Nutr) # p-value = 0.2016


#################################
# transform and re-do the anova #
#################################
################################
# try a log + 1 transformation #
################################
avgRGR_anova_logx1 <- aov(log(avgRGR+1) ~ species*Nutr*Temp, data=data_raw)
summary(avgRGR_anova_logx1)
posthoc_avgRGR_anova_logx1 <- TukeyHSD(avgRGR_anova_logx1)
posthoc_avgRGR_anova_logx1
threeway <- posthoc_avgRGR_anova_logx1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(avgRGR_anova_logx1))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova_logx1)) 
qqline(resid(avgRGR_anova_logx1)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova_logx1)) # p-value = 0.00218 



#################################
# transform and re-do the anova #
#################################
################################
# try a sqrt + 1 transformation #
################################
avgRGR_anova_sqrt1 <- aov(sqrt(avgRGR+1) ~ species*Nutr*Temp, data=data_raw)
summary(avgRGR_anova_sqrt1)
posthoc_avgRGR_anova_sqrt1 <- TukeyHSD(avgRGR_anova_sqrt1)
posthoc_avgRGR_anova_sqrt1
threeway <- posthoc_avgRGR_anova_sqrt1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(avgRGR_anova_sqrt1))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova_sqrt1)) 
qqline(resid(avgRGR_anova_sqrt1)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova_sqrt1)) # p-value = 0.005152 


#################################
# transform and re-do the anova #
#################################
# DO NOT WORK: 
# arcsine sqrt 
# arcsine sqrt + 1 
# problems with negatives or #'s not in (0,1)




######################################
# ANOVA @ each treatment combination #
######################################
###############
# Low Nutr Low Temp #
###############
avgRGR_LL_anova <- aov(avgRGR~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
summary(avgRGR_LL_anova)
TukeyHSD(avgRGR_LL_anova)

hist(resid(avgRGR_LL_anova)) # plot a histogram 

qqnorm(resid(avgRGR_LL_anova)) # QQ plot 
qqline(resid(avgRGR_LL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_LL_anova)) # p-value =  0.01705

# try a power transformation 
library(car)
powerTransform(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
power <- 0.5835013 

# add the power transformation
data_raw$power_avgRGR <- ((data_raw$avgRGR)^power - 1) / power 

power_avgRGR_LM_anova <- aov(power_avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
summary(power_avgRGR_LM_anova)
posthoc_power_avgRGR_LM_anova <- TukeyHSD(power_avgRGR_LM_anova)
posthoc_power_avgRGR_LM_anova

# Examine residuals 
hist(resid(power_avgRGR_LM_anova)) # plot a histogram 

qqnorm(resid(power_avgRGR_LM_anova)) # QQ plot 
qqline(resid(power_avgRGR_LM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_avgRGR_LM_anova)) # p-value = 0.09846

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))

###############
# Med Nutr Low Temp #
###############
avgRGR_ML_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="18"))
summary(avgRGR_ML_anova)
TukeyHSD(avgRGR_ML_anova)

hist(resid(avgRGR_ML_anova)) # plot a histogram 

qqnorm(resid(avgRGR_ML_anova)) # QQ plot 
qqline(resid(avgRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_ML_anova)) # p-value =  0.2524

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="18"))

################
# High Nutr Low Temp #
################
avgRGR_HL_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
summary(avgRGR_HL_anova)
TukeyHSD(avgRGR_HL_anova)

hist(resid(avgRGR_HL_anova)) # plot a histogram 

qqnorm(resid(avgRGR_HL_anova)) # QQ plot 
qqline(resid(avgRGR_HL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_HL_anova)) # p-value =  0.3119

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))

###############
# Low Nutr Med Temp #
###############
avgRGR_LM_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))
summary(avgRGR_LM_anova)
TukeyHSD(avgRGR_LM_anova)

hist(resid(avgRGR_LM_anova)) # plot a histogram 

qqnorm(resid(avgRGR_LM_anova)) # QQ plot 
qqline(resid(avgRGR_LM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_LM_anova)) # p-value =  0.7246

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))


######################
# Low Nutr High Temp #
######################
avgRGR_LH_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))
summary(avgRGR_LH_anova)
TukeyHSD(avgRGR_LH_anova)

# the data don't look too crazy !
hist(subset(data_raw$avgRGR, data_raw$Nutr=="low" & data_raw$Temp=="30"))

hist(resid(avgRGR_LH_anova)) # plot a histogram 

qqnorm(resid(avgRGR_LH_anova)) # QQ plot 
qqline(resid(avgRGR_LH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_LH_anova)) # p-value =  0.0007772

# try a sqrt + 1 transformation
sqrt_avgRGR_LH_anova <- aov(sqrt(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))
summary(sqrt_avgRGR_LH_anova)
TukeyHSD(sqrt_avgRGR_LH_anova)

hist(resid(sqrt_avgRGR_LH_anova)) # plot a histogram 

qqnorm(resid(sqrt_avgRGR_LH_anova)) # QQ plot 
qqline(resid(sqrt_avgRGR_LH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(sqrt_avgRGR_LH_anova)) # p-value =  0.0005892

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(sqrt(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30")) # p = 0.03626


# try a logx x + 1 transformation
log_avgRGR_LH_anova <- aov(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))
summary(log_avgRGR_LH_anova)
TukeyHSD(sqrt_avgRGR_LH_anova)

hist(subset(log(data_raw$avgRGR+1), data_raw$Nutr=="low" & data_raw$Temp=="30"))

hist(resid(log_avgRGR_LH_anova)) # plot a histogram 

qqnorm(resid(log_avgRGR_LH_anova)) # QQ plot 
qqline(resid(log_avgRGR_LH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(log_avgRGR_LH_anova)) # p-value =  0.000445

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30")) # p = 0.03257

# levene's test: homogeneity of variance 
# null hypothesis: equal variance 
library(car)
leveneTest(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))


###############
# Med Nutr Med Temp #
###############
avgRGR_MM_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))
summary(avgRGR_MM_anova)
TukeyHSD(avgRGR_MM_anova)

hist(resid(avgRGR_MM_anova)) # plot a histogram 

qqnorm(resid(avgRGR_MM_anova)) # QQ plot 
qqline(resid(avgRGR_MM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_MM_anova)) # p-value =  0.01644

# log x+1 transformation
logx1_avgRGR_MM_anova <- aov(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))
summary(logx1_avgRGR_MM_anova)
TukeyHSD(logx1_avgRGR_MM_anova)

hist(resid(logx1_avgRGR_MM_anova)) # plot a histogram 

qqnorm(resid(logx1_avgRGR_MM_anova)) # QQ plot 
qqline(resid(logx1_avgRGR_MM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(logx1_avgRGR_MM_anova)) # p-value =  0.2381

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24")) # 0.1064

#################
# High Nutr High Temp #
#################
avgRGR_HH_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="30"))
summary(avgRGR_HH_anova)
TukeyHSD(avgRGR_HH_anova)

hist(resid(avgRGR_HH_anova)) # plot a histogram 

qqnorm(resid(avgRGR_HH_anova)) # QQ plot 
qqline(resid(avgRGR_HH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_HH_anova)) # p-value =  0.8861

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="30"))

################
# Med Nutr High Temp #
################
avgRGR_MH_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30"))
summary(avgRGR_MH_anova)
TukeyHSD(avgRGR_MH_anova)

hist(resid(avgRGR_MH_anova)) # plot a histogram 

qqnorm(resid(avgRGR_MH_anova)) # QQ plot 
qqline(resid(avgRGR_MH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_MH_anova)) # p-value =  0.05815

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30"))

################
# High Nutr Med Temp #
################
avgRGR_HM_anova <- aov(avgRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="24"))
summary(avgRGR_HM_anova)
TukeyHSD(avgRGR_HM_anova)

hist(resid(avgRGR_HM_anova)) # plot a histogram 

qqnorm(resid(avgRGR_HM_anova)) # QQ plot 
qqline(resid(avgRGR_HM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_HM_anova)) # p-value =  0.2611

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(log(avgRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="24"))


