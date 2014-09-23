############################################
# Turion # to Turion area                  #
# Conversion                               #
# Data from Fall 2012 Stoichiometry exp.   #
############################################
library(plyr)
library(ggplot2)

# import data 
data_turion_area <- read.csv("turion_area-FOR_CONVERSION_ONLY.csv")
head(data_turion_area)

# add a new variable that combines plate and well to use as an ID 
data_turion_area$id <- paste(data_turion_area$plate,data_turion_area$row,data_turion_area$col,sep="")

# reshape the data 
a <- reshape(data_turion_area, 
             idvar="id",
             varying = c("turion_area5","turion_area7","turion_area10","turion_area12"),
             times = c(5,7,10,12),
             timevar = "day",
             v.names = "turion_AREA",
             direction = "long")

b <- reshape(data_turion_area, 
             idvar="id",
             varying = c("turions5","turions7","turions10","turions12"),
             times = c(5,7,10,12),
             timevar = "day",
             v.names = "turion_NUMB",
             direction = "long")

# remove un-needed variables 
a[9:28] <- list(NULL)
b[9:28] <- list(NULL)

# merge 
data_turion_area_long <- merge(a,b)

# clean-up
row.names(data_turion_area_long) <- seq(nrow(data_turion_area_long)) # Re-name the rows so they're not so ugly

# check it out 
head(data_turion_area_long)

# Remove any cases that are 0,0
wolffia_data <- subset(data_turion_area_long, data_turion_area_long$species=="W")
wolffia_data <- subset(wolffia_data, wolffia_data$turion_NUMB > 0)
wolffia_data


###########
# Wolffia #
# OLS     #
###########
turion_area_WB <- ggplot(wolffia_data, aes(x=turion_NUMB,y=turion_AREA))
turion_area_WB <- turion_area_WB + geom_point(size=3)
#turion_area_WB <- turion_area_WB + ggtitle("Wolffia brasiliensis")
turion_area_WB <- turion_area_WB + xlab("Number of turions")
turion_area_WB <- turion_area_WB + ylab(expression(paste("Turion area (",mm^2,")",sep="")))
turion_area_WB <- turion_area_WB + theme_classic(base_size=18)
turion_area_WB <- turion_area_WB + geom_smooth(method='lm',colour="black")
turion_area_WB

# get the slope and intercept 
W_turion_lm <- lm(turion_AREA ~ turion_NUMB, data=wolffia_data)
summary(W_turion_lm)
# Intercept: -0.004554
# Slope: 0.183665 
# adj R sq: 0.9265

# test for heterogeneity of variance - Breusch-Pagan Test 
# The Breusch-Pagan test fits a linear regression model to the residuals of a linear regression model
# (by default the same explanatory variables are taken as in the main regression model) and rejects if
# too much of the variance is explained by the additional explanatory variables.
library(lmtest)
bptest(turion_AREA ~ turion_NUMB, data=wolffia_data)

# examine the residuals against the x values 
plot(wolffia_data$turion_NUMB, resid(W_turion_lm), ylab="Residuals", xlab="Number of turions") 
abline(0, 0) 

# examine the residuals against the fitted values of y
plot(W_turion_lm$fitted, resid(W_turion_lm), ylab="Residuals", xlab="Fitted values") 
abline(0, 0) 

plot(fitted.values(W_turion_lm),rstudent(W_turion_lm),main="Studentized residuals vs fitted values")
abline(h=0,lty=2)

library(car)
spreadLevelPlot(W_turion_lm)
# this suggests a power transformation 

##############
# Wolffia    #
# robust SEs #
##############
# http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week4/14.Heteroskedastic.pdf

library(car)

# hccm = "heterogeneity consistent covariance matrix"
sqrt(diag(hccm(W_turion_lm)))

# the squareroot of the HCCM matrix gives the robust SEs 

# define the robust.se function 
robust.se <- function(model) {
  s <- summary(model)
  wse <- sqrt(diag(hccm(model))) # these are your robust SEs (also called "White standard errors")
  t <- model$coefficients / wse # divide the original coefficients by the robust SE 
  p <- 2*pnorm(-abs(t)) 
  results <- cbind(model$coefficients, wse, t, p)
  dimnames(results) <- dimnames(s$coefficients)
  results
}

# use the robust.se function
robust.se(W_turion_lm)

###########
# Wolffia #
# WLS     #
###########
# get the slope and intercept 
W_turion_lm_weighted <- lm(turion_AREA ~ turion_NUMB, data=wolffia_data, weights=1/(wolffia_data$turion_NUMB^2))
summary(W_turion_lm_weighted)
# Intercept: 0.090386
# Slope: 0.180118
# adj R sq: 0.9452

# examine the residuals against the x values 
plot(wolffia_data$turion_NUMB, resid(W_turion_lm_weighted), ylab="Residuals", xlab="Number of turions") 
abline(0, 0) 

# examine the residuals against the fitted values of y
plot(W_turion_lm_weighted$fitted, resid(W_turion_lm_weighted), ylab="Residuals", xlab="Fitted values") 
abline(0, 0) 

#######################
# plot both OLS & WLS #
#######################
turion_area_WB_2 <- ggplot(wolffia_data, aes(x=turion_NUMB,y=turion_AREA))
turion_area_WB_2 <- turion_area_WB_2 + geom_point(size=3)
turion_area_WB_2 <- turion_area_WB_2 + xlab("Number of turions")
turion_area_WB_2 <- turion_area_WB_2 + ylab(expression(paste("Turion area (",mm^2,")",sep="")))
turion_area_WB_2 <- turion_area_WB_2 + theme_classic(base_size=18)
turion_area_WB_2 <- turion_area_WB_2 + geom_abline(intercept=-0.004554,slope=0.180118,colour="blue") # this is the OLS line 
turion_area_WB_2 <- turion_area_WB_2 + geom_abline(intercept=0.090386,slope=0.180118,colour="red") # this is the WLS line 
turion_area_WB_2

ggsave("turion_numb_to_area - Wolffia - OLS & WLS.jpg",turion_area_WB_2,height=6,width=6)


#############
# Spirodela #
#############
turion_area_SP <- ggplot(subset(data_turion_area_long, data_turion_area_long$species=="S"), aes(x=turion_NUMB,y=turion_AREA))
turion_area_SP <- turion_area_SP + geom_point(size=3)
turion_area_SP <- turion_area_SP + ggtitle("Spirodela polyrhiza")
turion_area_SP <- turion_area_SP + xlab("Number of turions")
turion_area_SP <- turion_area_SP + ylab("Turion area (sq. mm)")
turion_area_SP <- turion_area_SP + theme_bw(base_size=18)
turion_area_SP <- turion_area_SP + geom_smooth(method='lm')
turion_area_SP

# get the subset of spirodela data where theere is a single turion 
single_spirodel_turions <- subset(data_turion_area_long, data_turion_area_long$species=="S" & data_turion_area_long$turion_NUMB==1)
# plot a histogram 
hist(single_spirodel_turions$turion_AREA,xlab="Turion area (sq.mm)")

# frequency distribution for size of single turions
turion_area_SP2 <- ggplot(single_spirodel_turions, aes(x=turion_AREA))
turion_area_SP2 <- turion_area_SP2 + geom_histogram(binwidth=0.5,colour="black",fill="grey")
turion_area_SP2 <- turion_area_SP2 + ylab("Frequency")
turion_area_SP2 <- turion_area_SP2 + xlab(expression(paste("Turion area (",mm^2,")",sep="")))
turion_area_SP2 <- turion_area_SP2 + theme_classic(base_size=18)
turion_area_SP2 <- turion_area_SP2 +geom_vline(x=2.793108, linetype="dashed")
turion_area_SP2

# summary statistics
numb_area <- length(single_spirodel_turions$turion_AREA)
mean_area <- mean(single_spirodel_turions$turion_AREA) # 2.793108
sd_area <- sd(single_spirodel_turions$turion_AREA)
se_area <- sd_area/sqrt(numb_area)
error <- qnorm(0.975)*sd_area/sqrt(numb_area)
lower <- mean_area - error
upper <- mean_area + error
