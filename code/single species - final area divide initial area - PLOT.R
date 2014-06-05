#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Final Area / Initial Area                     #    
#################################################

library(ggplot2)

# check out the data that you will use 
head(summary_data_final_divide_initial)

###########################
# Final / initial area    #
# By each treatment combo #
###########################
# import the post-hoc test labels 
summary_data_final_divide_initial <- read.csv("final_divide_initial_posthoc.csv")
summary_data_final_divide_initial$Nutr <- factor(summary_data_final_divide_initial$Nutr , levels=c("low","med","high"))
summary_data_final_divide_initial$Temp <- as.factor(summary_data_final_divide_initial$Temp)
summary_data_final_divide_initial$Temp <- factor(summary_data_final_divide_initial$Temp , levels=c("18","24","30"))

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
mean_final_divide_initial_plot <- ggplot(summary_data_final_divide_initial, aes(x=species,y=final_divide_initial)) + geom_point() 
mean_final_divide_initial_plot <- mean_final_divide_initial_plot + geom_errorbar(aes(ymin=final_divide_initial-se, ymax=final_divide_initial+se), width=0.1)
mean_final_divide_initial_plot <- mean_final_divide_initial_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_final_divide_initial_plot <- mean_final_divide_initial_plot + ylab("Average Relative Growth Rate")
mean_final_divide_initial_plot <- mean_final_divide_initial_plot + xlab("Species")
mean_final_divide_initial_plot <- mean_final_divide_initial_plot + ylim(0,17)
mean_final_divide_initial_plot <- mean_final_divide_initial_plot + theme_bw(base_size=18)
mean_final_divide_initial_plot <- mean_final_divide_initial_plot + geom_text(data=summary_data_final_divide_initial,aes(x=species, y=final_divide_initial+se+1,label=label))
mean_final_divide_initial_plot 

# save it 
ggsave(filename = "mean_final_divide_initial_plot.jpg", mean_final_divide_initial_plot, height=11, width=11)

