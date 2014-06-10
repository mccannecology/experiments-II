##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# turionsTOT                             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_turions)
head(summary_data_turions)

summary_data_turions_WB <- subset(summary_data_turions, summary_data_turions$species=="WB")

##############
# turionsTOT #
# Average    #
##############
# import the post-hoc test labels 
summary_data_turions <- read.csv("summary_data_turions_posthoc.csv")
summary_data_turions$Nutr <- factor(summary_data_turions$Nutr , levels=c("low","med","high"))
summary_data_turions$Temp <- as.factor(summary_data_turions$Temp)
summary_data_turions$Temp <- factor(summary_data_turions$Temp , levels=c("18","24","30"))

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
mean_summary_data_turions_plot <- ggplot(summary_data_turions_WB, aes(x=species,y=turionsTOT)) + geom_point() 
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_errorbar(aes(ymin=turionsTOT-se, ymax=turionsTOT+se), width=0.1)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + ylab("Total turions produced per replicate")
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + xlab("Species")
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + theme_bw(base_size=18)
#mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_text(data=summary_data_summary_data_turions,aes(x=species, y=summary_data_turions+se+0.025,label=label))
mean_summary_data_turions_plot 

# save it 
ggsave(filename = "mean_summary_data_turions_plot.jpg", mean_summary_data_turions_plot, height=11, width=11)


