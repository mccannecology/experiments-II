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
# import the post-hoc test labels 
summary_data_maxRGR <- read.csv("maxRGR_posthoc.csv")
summary_data_maxRGR$Nutr <- factor(summary_data_maxRGR$Nutr , levels=c("low","med","high"))
summary_data_maxRGR$Temp <- as.factor(summary_data_maxRGR$Temp)
summary_data_maxRGR$Temp <- factor(summary_data_maxRGR$Temp , levels=c("18","24","30"))

# labelling the facet variables
nutrient_names <- list("low"="Low nutrients","med"="Medium nutrients","high"="High nutrients")
temperature_names <- list("18"=expression(paste("18 ",degree,"C")),"24"=expression(paste("24 ",degree,"C")),"30"=expression(paste("30 ",degree,"C")))

labeller_function <- function(variable,value){
  if (variable=="Temp") {
    return(temperature_names[value])
  } else {
    return(nutrient_names[value])
  }
}

# making the plot 
mean_maxRGR_plot <- ggplot(summary_data_maxRGR, aes(x=species,y=maxRGR)) + geom_point() 
mean_maxRGR_plot <- mean_maxRGR_plot + geom_errorbar(aes(ymin=maxRGR-se, ymax=maxRGR+se), width=0.1)
mean_maxRGR_plot <- mean_maxRGR_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_maxRGR_plot <- mean_maxRGR_plot + ylab("Maximum Relative Growth Rate")
mean_maxRGR_plot <- mean_maxRGR_plot + xlab("Species")
mean_maxRGR_plot <- mean_maxRGR_plot + ylim(0,0.4)
mean_maxRGR_plot <- mean_maxRGR_plot + theme_bw(base_size=18)
#mean_maxRGR_plot <- mean_maxRGR_plot + geom_text(data=summary_data_maxRGR,aes(x=species, y=maxRGR+se+0.025,label=label))
mean_maxRGR_plot 

# save it 
ggsave(filename = "mean_maxRGR_plot.jpg", mean_maxRGR_plot, height=11, width=11)


