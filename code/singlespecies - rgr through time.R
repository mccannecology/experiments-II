##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# RGR                                    #
# Through time (day 0 to 12)             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr)

############
# RGR      #
# Raw data #
############
# colour
raw_rgr_0to12_plot <- ggplot(data_rgr, aes(x=day,y=rgr,group=id,colour=species)) + geom_line() + geom_point() 
raw_rgr_0to12_plot <- raw_rgr_0to12_plot + facet_grid(Nutr ~ Temp)
raw_rgr_0to12_plot <- raw_rgr_0to12_plot + scale_x_discrete(breaks=c(1.5,4,6,8.5,11),labels=c(1.5,4,6,8.5,11))
raw_rgr_0to12_plot <- raw_rgr_0to12_plot + ylab("rgr (sq. mm")
raw_rgr_0to12_plot 

############
# RGR      #
# Average  #
############
# colour
mean_rgr_0to12_plot <- ggplot(summary_data_rgr, aes(x=day,y=rgr,colour=species)) + geom_line() + geom_point() 
mean_rgr_0to12_plot <- mean_rgr_0to12_plot + geom_errorbar(aes(ymin=rgr-se, ymax=rgr+se), width=0.1)
mean_rgr_0to12_plot <- mean_rgr_0to12_plot + facet_grid(Nutr ~ Temp)
mean_rgr_0to12_plot <- mean_rgr_0to12_plot + scale_x_discrete(breaks=c(1.5,4,6,8.5,11),labels=c(1.5,4,6,8.5,11))
mean_rgr_0to12_plot <- mean_rgr_0to12_plot + ylab("rgr (sq. mm")
mean_rgr_0to12_plot 

#####################
# Preliminary anova #
# Y = rgr           #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
# day               #
#####################
rgr_anova <- aov(rgr ~ species*Temp*Nutr*day, data=data_rgr)
summary(rgr_anova)
TukeyHSD(rgr_anova)

