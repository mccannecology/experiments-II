#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Plotting data                                 #
# Area                                          #    
# Through time (day 0 to 12)                    #
#################################################
library(ggplot2)

# check out the data you will use
head(data_area)
head(summary_data_area)

############
# Raw data #
############
# colour
raw_area_0to12_plot <- ggplot(data_area, aes(x=day,y=area,group=id,colour=species)) + geom_line() + geom_point() 
raw_area_0to12_plot <- raw_area_0to12_plot + facet_grid(Temp ~ Nutr)
raw_area_0to12_plot <- raw_area_0to12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
raw_area_0to12_plot <- raw_area_0to12_plot + ylab("area (sq. mm)")
raw_area_0to12_plot <- raw_area_0to12_plot + theme_gray(base_size=18)
raw_area_0to12_plot 

ggsave(filename = "raw_area_0to12_plot.pdf", raw_area_0to12_plot, height=11, width=8)


############
# Average  #
############
# colour
mean_area_0to12_plot <- ggplot(summary_data_area, aes(x=day,y=area,colour=species)) + geom_line() + geom_point() 
mean_area_0to12_plot <- mean_area_0to12_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_0to12_plot <- mean_area_0to12_plot + facet_grid(Temp ~ Nutr)
mean_area_0to12_plot <- mean_area_0to12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
mean_area_0to12_plot <- mean_area_0to12_plot + ylab("area (sq. mm)")
mean_area_0to12_plot <- mean_area_0to12_plot + theme_gray(base_size=18)
mean_area_0to12_plot 

ggsave(filename = "mean_area_0to12_plot.pdf", mean_area_0to12_plot, height=11, width=8)


#####################
# Preliminary anova #
# Y = Area          #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
# day               #
#####################
area_anova <- aov(area ~ species*Nutr*Temp*day, data=data_area)
summary(area_anova)
TukeyHSD(area_anova)

