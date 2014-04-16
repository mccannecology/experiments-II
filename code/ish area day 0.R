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

# re-order my treatments so they go from monocultures to polycultures in alphabetical order 
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

#####################
# Preliminary anova #
#####################
# Y = Area day 0
# Treatments: species, nutrients, temperature       
area_day0_anova <- aov(area ~ species*Temp*Nutr, data=subset(summary_data_area, summary_data_area$day == 0))
summary(area_day0_anova)
TukeyHSD(area_day0_anova)

