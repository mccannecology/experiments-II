##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# Area                                   #    
# Through time (day 0 to 12)             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_area)
head(summary_data_area)

############
# Raw data #
############
# colour
raw_area_0to12_plot <- ggplot(data_area, aes(x=day,y=area,group=id,colour=species)) + geom_line() + geom_point() 
raw_area_0to12_plot <- raw_area_0to12_plot + facet_grid(Nutr ~ Temp)
raw_area_0to12_plot <- raw_area_0to12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
raw_area_0to12_plot <- raw_area_0to12_plot + ylab("area (sq. mm")
raw_area_0to12_plot 

############
# Average  #
############
# colour
mean_area_0to12_plot <- ggplot(summary_data_area, aes(x=day,y=area,colour=species)) + geom_line() + geom_point() 
mean_area_0to12_plot <- mean_area_0to12_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_0to12_plot <- mean_area_0to12_plot + facet_grid(Nutr ~ Temp)
mean_area_0to12_plot <- mean_area_0to12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
mean_area_0to12_plot <- mean_area_0to12_plot + ylab("area (sq. mm")
mean_area_0to12_plot 


