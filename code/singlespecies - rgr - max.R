##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# turionsTOT                             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_turion)
head(summary_data_turions)

data_turions_WB <- subset(data_turion, data_turion$species=="WB")

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

