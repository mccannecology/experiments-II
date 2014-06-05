##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# maxRGR                                 #
# max within a replicate                 #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_maxRGR)

############
# maxRGR   #
# Average  #
############
# colour
mean_maxRGR_plot <- ggplot(summary_data_maxRGR, aes(x=species,y=maxRGR)) + geom_point() 
mean_maxRGR_plot <- mean_maxRGR_plot + geom_errorbar(aes(ymin=maxRGR-se, ymax=maxRGR+se), width=0.1)
mean_maxRGR_plot <- mean_maxRGR_plot + facet_grid(Temp ~ Nutr)
mean_maxRGR_plot <- mean_maxRGR_plot + ylab("Maximum RGR")
mean_maxRGR_plot <- mean_maxRGR_plot + theme_gray(base_size=18)
mean_maxRGR_plot 

ggsave(filename = "mean_maxRGR_plot.pdf", mean_maxRGR_plot, height=11, width=8)


#####################
# Preliminary anova #
# Y = maxRGR        #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
#####################
maxRGR_anova <- aov(maxRGR ~ species*Nutr*Temp, data=data_raw)
summary(maxRGR_anova)
posthoc_maxRGR_anova<- TukeyHSD(maxRGR_anova)
posthoc_maxRGR_anova
threeway <- posthoc_maxRGR_anova[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 
significant

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

#################################
# other parameteric assumptions #
#################################
# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_raw$maxRGR ~ data_raw$species * data_raw$Temp * data_raw$Nutr) # p-value = 0.9483


#################################
# transform and re-do the anova #
#################################
###############################
# try a sqrt+1 transformation #
###############################
maxRGR_anova_sqrt <- aov(sqrt(maxRGR+1) ~ species*Nutr*Temp, data=data_raw)
summary(maxRGR_anova_sqrt)
posthoc_maxRGR_anova_sqrt <- TukeyHSD(maxRGR_anova_sqrt)
posthoc_maxRGR_anova_sqrt
threeway <- posthoc_maxRGR_anova_sqrt[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_sqrt))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_sqrt)) 
qqline(resid(maxRGR_anova_sqrt)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_sqrt)) # p-value = 0.02419 



#################################
# transform and re-do the anova #
#################################
################################
# try a log + 1 transformation #
################################
maxRGR_anova_logx1 <- aov(log(maxRGR+1) ~ species*Nutr*Temp, data=data_raw)
summary(maxRGR_anova_logx1)
posthoc_maxRGR_anova_logx1 <- TukeyHSD(maxRGR_anova_logx1)
posthoc_maxRGR_anova_logx1
threeway <- posthoc_maxRGR_anova_logx1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_logx1))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_logx1)) 
qqline(resid(maxRGR_anova_logx1)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_logx1)) # p-value = 0.01486 




#################################
# transform and re-do the anova #
#################################
####################################
# try a arcsinesqrt transformation #
####################################
maxRGR_anova_asinsqrt <- aov(asin(sqrt(maxRGR)) ~ species*as.factor(Nutr)*as.factor(Temp), data=data_raw)
summary(maxRGR_anova_asinsqrt)
posthoc_maxRGR_anova_asinsqrt <- TukeyHSD(maxRGR_anova_asinsqrt)
posthoc_maxRGR_anova_asinsqrt
threeway <- posthoc_maxRGR_anova_asinsqrt[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_asinsqrt))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_asinsqrt)) 
qqline(resid(maxRGR_anova_asinsqrt)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_asinsqrt)) # p-value = 0.01486 



#################################
# transform and re-do the anova #
#################################
##############################
# try a power transformation #
##############################
# power transformation cannot handle (-) values
# convert (-) values to 0 
data_raw$maxRGR 
data_raw$maxRGR[data_raw$maxRGR <= 0] <- 0.001

# figure out the best power transformation 
library(car)
powerTransform(maxRGR ~ species*as.factor(Nutr)*as.factor(Temp), data=data_raw)
power <- 0.7332737

# add the power transformation of stand
data_raw$power_maxRGR <- ((data_raw$maxRGR)^power - 1) / power 

maxRGR_anova_power <- aov(power_maxRGR ~ species*Nutr*Temp, data=data_raw)
summary(maxRGR_anova_power)
posthoc_maxRGR_anova_power <- TukeyHSD(maxRGR_anova_power)
posthoc_maxRGR_anova_power
threeway <- posthoc_maxRGR_anova_logx1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_power))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_power)) 
qqline(resid(maxRGR_anova_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_power)) # p-value = 0.02066 






######################################
# ANOVA @ each treatment combination #
######################################
###############
# Low Nutr Low Temp #
###############
maxRGR_LL_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18"))
summary(maxRGR_LL_anova)
TukeyHSD(maxRGR_LL_anova)

hist(resid(maxRGR_LL_anova)) # plot a histogram 

qqnorm(resid(maxRGR_LL_anova)) # QQ plot 
qqline(resid(maxRGR_LL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_LL_anova)) # p-value =  0.09561

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species * Temp * Nutr, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="18")) # p-value = 0.7428

###############
# Med Nutr Low Temp #
###############
maxRGR_ML_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="18"))
summary(maxRGR_ML_anova)
TukeyHSD(maxRGR_ML_anova)

hist(resid(maxRGR_ML_anova)) # plot a histogram 

qqnorm(resid(maxRGR_ML_anova)) # QQ plot 
qqline(resid(maxRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_ML_anova)) # p-value =  0.9133

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species * Temp * Nutr, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="18")) # p-value = 0.3841


################
# High Nutr Low Temp #
################
maxRGR_HL_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
summary(maxRGR_HL_anova)
TukeyHSD(maxRGR_HL_anova)

hist(resid(maxRGR_HL_anova)) # plot a histogram 

qqnorm(resid(maxRGR_HL_anova)) # QQ plot 
qqline(resid(maxRGR_HL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_HL_anova)) # p-value =  0.7315

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species * Temp * Nutr, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18")) # p-value = 0.01982

# log x + 1 transformation
log_maxRGR_HL_anova <- aov(log(maxRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
summary(log_maxRGR_HL_anova)
TukeyHSD(log_maxRGR_HL_anova)

hist(resid(log_maxRGR_HL_anova)) # plot a histogram 

qqnorm(resid(log_maxRGR_HL_anova)) # QQ plot 
qqline(resid(log_maxRGR_HL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(log_maxRGR_HL_anova)) # p-value =  0.6752

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(log(maxRGR+1) ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18")) # p-value = 0.01754

# try a power transformation 
library(car)
powerTransform(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
power <- 2.731675 

# add the power transformation
data_raw$power_maxRGR <- ((data_raw$maxRGR)^power - 1) / power 

power_maxRGR_HL_anova <- aov(power_maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18"))
summary(power_maxRGR_HL_anova)
posthoc_power_maxRGR_HL_anova <- TukeyHSD(power_maxRGR_HL_anova)
posthoc_power_maxRGR_HL_anova

# Examine residuals 
hist(resid(power_maxRGR_HL_anova)) # plot a histogram 

qqnorm(resid(power_maxRGR_HL_anova)) # QQ plot 
qqline(resid(power_maxRGR_HL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_maxRGR_HL_anova)) # p-value = 0.7904

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18")) # p-value = 0.04213

# levene's test: homogeneity of variance 
# null hypothesis: equal variance 
library(car)
leveneTest(power_maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="18")) 

###############
# Low Nutr Med Temp #
###############
maxRGR_LM_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))
summary(maxRGR_LM_anova)
TukeyHSD(maxRGR_LM_anova)

hist(resid(maxRGR_LM_anova)) # plot a histogram 

qqnorm(resid(maxRGR_LM_anova)) # QQ plot 
qqline(resid(maxRGR_LM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_LM_anova)) # p-value =  0.01148

# try a power transformation 
library(car)
powerTransform(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))
power <- 0.1092525 

# add the power transformation
data_raw$power_maxRGR <- ((data_raw$maxRGR)^power - 1) / power 

power_maxRGR_LM_anova <- aov(power_maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24"))
summary(power_maxRGR_LM_anova)
posthoc_power_maxRGR_LM_anova <- TukeyHSD(power_maxRGR_LM_anova)
posthoc_power_maxRGR_LM_anova

# Examine residuals 
hist(resid(power_maxRGR_LM_anova)) # plot a histogram 

qqnorm(resid(power_maxRGR_LM_anova)) # QQ plot 
qqline(resid(power_maxRGR_LM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_maxRGR_LM_anova)) # p-value = 0.09496

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24")) # p-value = 0.01754

# levene's test: homogeneity of variance 
# null hypothesis: equal variance 
library(car)
leveneTest(power_maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="24")) 

################
# Low Nutr High Temp #
################
maxRGR_LH_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30"))
summary(maxRGR_LH_anova)
TukeyHSD(maxRGR_LH_anova)

hist(resid(maxRGR_LH_anova)) # plot a histogram 

qqnorm(resid(maxRGR_LH_anova)) # QQ plot 
qqline(resid(maxRGR_LH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_LH_anova)) # p-value =  0.4481

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="low" & data_raw$Temp=="30")) # p-value =  0.5456

###############
# Med Nutr Med Temp #
###############
maxRGR_MM_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24"))
summary(maxRGR_MM_anova)
TukeyHSD(maxRGR_MM_anova)

hist(resid(maxRGR_MM_anova)) # plot a histogram 

qqnorm(resid(maxRGR_MM_anova)) # QQ plot 
qqline(resid(maxRGR_MM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_MM_anova)) # p-value =   0.42

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="24")) # p = 0.3988


#################
# High Nutr High Temp #
#################
maxRGR_HH_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="30"))
summary(maxRGR_HH_anova)
TukeyHSD(maxRGR_HH_anova)

hist(resid(maxRGR_HH_anova)) # plot a histogram 

qqnorm(resid(maxRGR_HH_anova)) # QQ plot 
qqline(resid(maxRGR_HH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_HH_anova)) # p-value =  0.9693

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="30"))

################
# Med Nutr High Temp #
################
maxRGR_MH_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30"))
summary(maxRGR_MH_anova)
TukeyHSD(maxRGR_MH_anova)

hist(resid(maxRGR_MH_anova)) # plot a histogram 

qqnorm(resid(maxRGR_MH_anova)) # QQ plot 
qqline(resid(maxRGR_MH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_MH_anova)) # p-value =  0.08134

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30"))

# levene's test: homogeneity of variance 
# null hypothesis: equal variance 
library(car)
leveneTest(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="med" & data_raw$Temp=="30")) 

################
# High Nutr Med Temp #
################
maxRGR_HM_anova <- aov(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="24"))
summary(maxRGR_HM_anova)
TukeyHSD(maxRGR_HM_anova)

hist(resid(maxRGR_HM_anova)) # plot a histogram 

qqnorm(resid(maxRGR_HM_anova)) # QQ plot 
qqline(resid(maxRGR_HM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_HM_anova)) # p-value =  0.1852

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data_raw, data_raw$Nutr=="high" & data_raw$Temp=="24"))

