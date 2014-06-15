##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# turionsTOT                             #
##########################################
library(ggplot2)

# check out the data you will use
data_turions_WB <- subset(data_turion, data_turion$species=="WB")
head(data_turion)
head(data_turions_WB)
head(summary_data_turions)
summary_data_turion_area_per_day_WB

##############################
# anova                      #
# WOLFFIA only               #
# Y = turion_area_per_day    #
# Treatments:                #
# nutrients,                 #
# temperature,               #
##############################
turions_areaTOT_anova <- aov(turion_area_per_day ~ Nutr*Temp, data=data_turions_WB)
summary(turions_areaTOT_anova)
posthoc_turions_areaTOT_anova <- TukeyHSD(turions_areaTOT_anova)
posthoc_turions_areaTOT_anova

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(turions_areaTOT_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(turions_areaTOT_anova)) 
qqline(resid(turions_areaTOT_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(turions_areaTOT_anova)) # p-value = 0.06986

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_turions_WB$turion_area_per_day ~ data_turions_WB$Temp * data_turions_WB$Nutr) # p-value 0.002256

# Levene Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(data_turions_WB$turion_area_per_day ~ data_turions_WB$Temp * data_turions_WB$Nutr) # p-value 0.0534




#####################
# anova             #
# WOLFFIA only      #
# Y = turionsTOT    #
# Treatments:       #
# nutrients,        #
# temperature,      #
#####################
turionsTOT_anova <- aov(turionsTOT ~ Nutr*Temp, data=data_turions_WB)
summary(turionsTOT_anova)
posthoc_turionsTOT_anova<- TukeyHSD(turionsTOT_anova)
posthoc_turionsTOT_anova

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(turionsTOT_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(turionsTOT_anova)) 
qqline(resid(turionsTOT_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(turionsTOT_anova)) # p-value = 0.06984

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_turions_WB$turionsTOT ~ data_turions_WB$Temp * data_turions_WB$Nutr) # p-value 0.002267

# Levene Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(data_turions_WB$turionsTOT ~ data_turions_WB$Temp * data_turions_WB$Nutr) # p-value 0.05341

