##################
# read the data  #
##################

fmly_raw <- read.csv("/Users/liqiansitu/Library/Mobile Documents/com~apple~CloudDocs/Data/.Household_Expenditure.csv.icloud", header=T)
dim(fmly_raw)

######################################
# Delete the Obs with missing vlaues #
######################################

fmly <- na.omit(fmly_raw)
dim(fmly)

names(fmly)
attach(fmly)

#################################


##################
# create dummy   #
##################

#############

fmly$income.f <- factor(fmly$inclass)
summary(fmly$inclass)
contrasts(fmly$income.f) <- contr.treatment(9, base=9)
summary(fmly$income.f)

#############

fmly$bls.f <- factor(fmly$bls_urbn)
summary(fmly$bls.urbn)
contrasts(fmly$bls.f) <- contr.treatment(2, base=2)
summary(fmly$bls.f)

#############

fmly$cuten.f <- factor(fmly$cutenure)
summary(fmly$cutenure)
contrasts(fmly$cuten.f) <- contr.treatment(2, base=2)
summary(fmly$cuten.f)

#############

educ <- abs(fmly$educ_ref-9)
fmly$educ_ref.f <- factor(educ)
summary(educ)
contrasts(fmly$educ_ref.f ) <- contr.treatment(9, base=9)
summary(fmly$educ_ref.f)

#############

fmly$marital.f <- factor(fmly$marital1)
summary(fmly$marital1)
contrasts(fmly$marital.f) <- contr.treatment(5, base=5)
summary(fmly$marital.f)

#############

summary(fmly$region)
fmly$region.f <- factor(fmly$region)
contrasts(fmly$region.f) <- contr.treatment(4, base=4)
summary(fmly$region.f)

#################################

################
## check VIF  ##
################

attach(fmly)
names(fmly)

s_fam_size <- scale(fmly$fam_size)
s_vehq <- scale(fmly$vehq)
s_fincatax <- scale(fmly$fincatax)
s_age_ref <- scale(fmly$age_ref)
s_num_auto <- scale(fmly$num_auto)
s_renteqvx <- scale(fmly$renteqvx)
s_inc_rank <- scale(fmly$inc_rank)
s_no_earnr <- scale(fmly$no_earnr)

X <- cbind(s_fam_size, s_vehq, s_fincatax, s_age_ref, s_num_auto, s_renteqvx, s_inc_rank, s_no_earnr, income.f, bls.f, cuten.f, educ_ref.f, marital.f, region.f)
colnames(X) <- c( "fam_size", "vehq" ,"fincatax", "age_ref",    "num_auto",   "renteqvx",   "inc_rank",   "no_earnr",   "income.f", "bls.f" ,    "cuten.f",    "educ_ref.f" , "marital.f",   "region.f"  )
cor(X)
diag_invese_X <- diag(solve(cor(X)))
diag_invese_X[diag_invese_X>10]

vif_matrix <- as.matrix(diag_invese_X)
colnames(vif_matrix) <- c("VIF")
vif_matrix

###################
# Another Method  #
################################
                          ######
install.packages("car")   ######
library(car)              ######
vif(fit_fmly_2)           ######
                          ######
################################

#########################
# no VIF > 10           #
#########################

#################################
## First Try to Fit the Model  ##
#################################

##################################
# Calculate correlations between #
# reponse and numeric predictors #
###################################

################################
##  Correlation Table: y vs x ##
################################

cor_table <- cor(X, fmly$etotapx4)
colnames(cor_table) <- c("etotapx4")
cor_table

plot(cor_table)

##################### 
### Our Full Model ##
#####################

######################################
## first, let y is not transformed  ##
######################################

fit_fmly <- lm(etotapx4~vehq + fam_size + fincatax + age_ref + num_auto + renteqvx + inc_rank + no_earnr+income.f+bls.f+cuten.f+educ_ref.f+marital.f+region.f, data=fmly)
summary(fit_fmly)
anova(fit_fmly)

qqnorm(rstudent(fit_fmly))
abline(0, 1, col="red")

plot(fitted.values(fit_fmly), rstudent(fit_fmly), xlab="Fitted Values", ylab="Studentized Residuals", main="Studentized Residuals V.S Fitted Values")

#######################################

############################################################
## Select the better model--Select the predictors         ##
############################################################

################################################### 
####  variable selection--fancy table version   ###
###################################################

library(leaps)

all <- regsubsets(x=cbind(fmly$vehq, fmly$fam_size, fmly$fincatax, fmly$age_ref, fmly$num_auto, fmly$renteqvx, fmly$inc_rank, fmly$no_earnr, fmly$income.f, fmly$bls.f, fmly$cuten.f, fmly$educ_ref.f, fmly$marital.f, fmly$region.f ), y= fmly$etotapx4,  method = "exhaustive", all.best = T)
summary(all)

Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which

p <- apply(Matrix,1, sum)
MSE <- SSRes/(13-p)

output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
colnames(output)[3:16] <- c("vehq", "fam_size", "fincatax", "age_ref", "num_auto", "renteqvx", "inc_rank", "no_earnr", "income.f", "bls.f", "cuten.f", "educ_ref.f", "marital.f", "region.f")
output 

model.table <- lm(etotapx4~vehq+fam_size+fincatax+renteqvx+inc_rank+cuten.f+bls.f+educ_ref.f, data=fmly)

#####################################
## Automate Stepwise Selection     ##
#####################################

null.model <- lm(etotapx4~1, data=fmly)
model.sw <- step(null.model, scope=list(lower=null.model, upper=fit_fmly), direction="both")


#####################################
## Automate Forward Selection      ##
#####################################

model.forward <- step(null.model, scope=list(lower=null.model, upper=fit_fmly), direction="forward")


#####################################
## Automate Backward Selection     ##
#####################################

model.backward <- step(fit_fmly, direction="backward")



#####################################################################
### Summary of the Stepwise, Backward and Forward Selected Models ###
#####################################################################

##############################################
## Which one do you think we should choose? ##
##############################################

summary(model.table)
summary(model.sw)
summary(model.forward)
summary(model.backward)

summary(model.table)$adj.r.squared
summary(model.sw)$adj.r.squared
summary(model.forward)$adj.r.squared
summary(model.backward)$adj.r.squared

summary(model.table)$sigma^2
summary(model.sw)$sigma^2
summary(model.forward)$sigma^2
summary(model.backward)$sigma^2

AIC(model.table)
AIC(model.sw)
AIC(model.forward)
AIC(model.backward)

BIC(model.table)
BIC(model.sw)
BIC(model.forward)
BIC(model.backward)

press_model_table <- sum((model.table$residuals/(1-hatvalues(model.table)))^2)
press_model_sw <- sum((model.sw $residuals/(1-hatvalues(model.sw)))^2)
press_model_forward <- sum(( model.forward $residuals/(1-hatvalues(model.forward)))^2)
press_model_backward <- sum(( model.backward $residuals/(1-hatvalues(model.backward)))^2)


#################################################################################################################
# After comparing several possible criteria, the stepwise, forward and backward models are the same, which are ##
# slightly better than the table model                                                                         ##
#################################################################################################################

##############################
# Maybe Stepwise ??????????  #
##############################

##############
##   Plots  ##
##############

par(mfrow = c(2,1), pty = "s")
qqnorm(rstudent(model.sw)) ## error terms are not normal ??? ##
abline(0,1, col="red")
plot(fitted.values(model.sw), rstudent(model.sw), ylim=c(-5,5), xlim=c(-2,40), main="Residuals v.s Fitted Values") ## residuals are not constant ?? ##
abline(0, 0, col="red")

par(mfrow=c(1,1))

plot(fmly$vehq, rstudent(model.sw))
plot(fmly$fam_size, rstudent(model.sw))
plot(fmly$fincatax, rstudent(model.sw))
plot(fmly$renteqvx, rstudent(model.sw))
plot(fmly$inc_rank, rstudent(model.sw))

plot(fitted.values(model.sw), rstudent(model.sw)) ## residuals are not constant ?? ##
abline(0, 0, col="red")

####################################################
## We need some sort of transformation on response##
####################################################

##############
## Box-Cox  ##
##############

summary(fmly$etotapx4)

fit_shift <-lm(I(etotapx4+1)~fincatax+renteqvx+inc_rank+vehq+cuten.f+educ_ref.f+fam_size+region.f+bls.f+no_earnr, data=fmly)

summary(fit_shift)

library(MASS)

BC <- boxcox(fit_shift)
BC
BC$x[BC$y==max(BC$y)]
BC$x

# confidence interval
S <- max(BC$y) - 0.5*qchisq(0.95,1)
S
BC$x[BC$y>S]

##############################
# Lamda is closed to zero    #
##############################


##################
## log   Model  ##
##################

log_fit_final <- lm(I(log(etotapx4+1))~fincatax+renteqvx+inc_rank+vehq+cuten.f+educ_ref.f+fam_size+region.f+bls.f+no_earnr, data=fmly)

summary(log_fit_final)
summary(log_fit_final)$r.squared
summary(log_fit_final)$sigma^2
AIC(log_fit_final)
BIC(log_fit_final)
PRESS <- sum((log_fit_final$residuals/(1-hatvalues(log_fit_final)))^2)
PRESS

qqnorm(rstudent(log_fit_final)) ## Too good to be true??? ###
abline(0,1, col="red")
plot(fitted.values(log_fit_final), rstudent(log_fit_final), main="Residuals v.s Fitted Values") ### constant ?? ###
abline(0, 0, col=2)

plot(fmly$vehq[1:3534], rstudent(log_fit_final))
plot(fmly$fam_size[1:3534], rstudent(log_fit_final))
plot(fmly$fincatax[1:3534], rstudent(log_fit_final))
plot(fmly$renteqvx[1:3534], rstudent(log_fit_final))
plot(fmly$inc_rank[1:3534], rstudent(log_fit_final))


###########################
# Check the leverage and  #
# and influential points, #
# to see if our model can #
# be improved futher      #
###########################

#################################
# Standard Residuals & Leverage #
#################################

################
##  outliners ##
################

standard_residuals <- resid(log_fit_final)/summary(log_fit_final)$sigma^2 
sort_standard_residuals <- c(sort(abs(standard_residuals)))
head(sort_standard_residuals)


fmly_delete_1 <- fmly[-c(1114     ,     810     ,     716    ,     3390    ,     2652     ,     351), ]
dim(fmly_delete_1)


######################
## leverage points  ##
######################

leverage_cut_off <- 2*length(log_fit_final $coefficients)/nrow(fmly)
h_ii <-lm.influence(log_fit_final)$hat
sort_leverage_cut_off <- c(sort(h_ii[h_ii>leverage_cut_off]))
head(sort_leverage_cut_off)

fmly_delete <- fmly_delete_1[-c(3409    ,   2373    ,   2348      ,  720     ,  1993    ,   1186 ),]
dim(fmly_delete)

###################
# Cook's Distance #
###################
c <- cooks.distance(log_fit_final)
cook <- cooks.distance(log_fit_final) > 1
sort(cook>1)

################################
# dfbeatas & dffits & covratio #
################################

dfbetas(log_fit_final) > 2/sqrt(nrow(fmly))
dffits(log_fit_final) > 2*sqrt(length(log_fit_final$coefficients)/nrow(fmly))
covratio(log_fit_final) > 1+3*length(log_fit_final$coefficients)/nrow(fmly)
covratio(log_fit_final) < 1-3*length(log_fit_final$coefficients)/nrow(fmly)

#################################################
# Re-fit our final model based on "fmly_delete" #
#################################################

refit_final <- lm(I(log(etotapx4+1))~fincatax+renteqvx+inc_rank+vehq+cuten.f+educ_ref.f+fam_size+region.f+bls.f+no_earnr, data=fmly_delete)

summary(refit_final)
anova(refit_final)

qqnorm(rstudent(refit_final)) ## Too good to be true??? ###
abline(0,1, col="red")

length(rstudent(refit_final))
plot(fmly$vehq[1:3522], rstudent(refit_final))
plot(fmly$fam_size[1:3522], rstudent(refit_final))
plot(fmly$fincatax[1:3522], rstudent(refit_final))
plot(fmly$renteqvx[1:3522], rstudent(refit_final))
plot(fmly$inc_rank[1:3522], rstudent(refit_final))
plot(fitted.values(refit_final), rstudent(refit_final)) ### constant ?? ###
abline(0,0, col="red")

########################################################################
# Compare final models with and without outliners and leverage points  #
########################################################################

summary(log_fit_final)
summary(refit_final)

summary(log_fit_final)$sigma^2
summary(refit_final)$sigma^2

summary(log_fit_final)$r.squared
summary(refit_final)$r.squared

press_fit_final <- sum(( log_fit_final$residuals/(1-hatvalues(log_fit_final)))^2)
press_refit_final <- sum(( refit_final $residuals/(1-hatvalues(refit_final)))^2)
press_fit_final
press_refit_final

AIC(log_fit_final)
AIC(refit_final)

BIC(log_fit_final)
BIC(refit_final)

######################################################################################
## Verdict: after removing the 6 worst outliners and 6 leverage points, our models  ##
## improves a little bit                                                            ##
######################################################################################


############################################
## The End of the Show!!!!!!!!!!!!!!!!!!! ##
############################################

#####################
# The Density Plots #
#####################
par(mfrow = c(2,1), pty = "m")
hist(etotapx4) 
hist(log(etotapx4+1)) 

par(mfrow=c(1,1), pty="m")

?hist

?par

############
## log(y) ##
############

fit_fmly_2 <- lm(I(log(etotapx4+1)) ~vehq + fam_size + fincatax + age_ref + num_auto + renteqvx + inc_rank+no_earnr+income.f+bls.f+cuten.f+educ_ref.f+marital.f+region.f, data=fmly)
summary(fit_fmly_2)
anova(fit_fmly_2)

qqnorm(rstudent(fit_fmly_2))
abline(0,1, col="red")
plot(fitted.values(fit_fmly_2), rstudent(fit_fmly_2))
abline(0,0, col="red")

AIC(fit_fmly)
BIC(fit_fmly)
AIC(fit_fmly_2)
BIC(fit_fmly_2)

library(leaps)

all <- regsubsets(x=cbind(fmly$vehq, fmly$fam_size, fmly$fincatax, fmly$age_ref, fmly$num_auto, fmly$renteqvx, fmly$inc_rank, fmly$no_earnr, fmly$income.f, fmly$bls.f, fmly$cuten.f, fmly$educ_ref.f, fmly$marital.f, fmly$region.f ), y=I(log(etotapx4+1)) ,  method = "exhaustive", all.best = T)
summary(all)

Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which

p <- apply(Matrix,1, sum)
MSE <- SSRes/(13-p)

output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
colnames(output)[3:16] <- c("vehq", "fam_size", "fincatax", "age_ref", "num_auto", "renteqvx", "inc_rank", "no_earnr", "income.f", "bls.f", "cuten.f", "educ_ref.f", "marital.f", "region.f")
output 

model.table.log <- lm(I(log(etotapx4+1))~vehq+fam_size+fincatax+renteqvx+inc_rank+income.f+bls.f+cuten.f+educ_ref.f, data=fmly)

#####################################
## Automate Stepwise Selection     ##
#####################################

null.model.log <- lm(I(log(etotapx4+1))~1, data=fmly)
model.sw.log <- step(null.model, scope=list(lower=null.model, upper=fit_fmly_2), direction="both")

#####################################
## Automate Forward Selection      ##
#####################################

model.forward.log <- step(null.model, scope=list(lower=null.model, upper=fit_fmly_2), direction="forward")

#####################################
## Automate Backward Selection     ##
#####################################

model.backward.log <- step(fit_fmly_2, direction="backward")

#####################################################################
### Summary of the Stepwise, Backward and Forward Selected Models ###
#####################################################################

##############################################
## Which one do you think we should choose? ##
##############################################

summary(model.table.log)$r.squared
summary(model.sw.log)$r.squared
summary(model.forward.log)$r.squared
summary(model.backward.log)$r.squared  ### the best ###

summary(model.table.log)$sigma^2
summary(model.sw.log)$sigma^2
summary(model.forward.log)$sigma^2
summary(model.backward.log)$sigma^2  ### the best ###

AIC(model.table.log)
AIC(model.sw.log)      ### tied ###
AIC(model.forward.log) ### tied ###
AIC(model.backward.log)

BIC(model.table.log)
BIC (model.sw.log)     ### tied ###
BIC(model.forward.log) ### tied ###
BIC(model.backward.log)

press.table <- sum(( model.table.log$residuals/(1-hatvalues(model.table.log)))^2)
press.sw<- sum(( model.sw.log$residuals/(1-hatvalues(model.sw.log)))^2)
press.forward <- sum(( model.forward.log$residuals/(1-hatvalues(model.forward.log)))^2)
press.backward <- sum(( model.backward.log$residuals/(1-hatvalues(model.backward.log)))^2) ### the best ###

press.table
press.sw
press.forward
press.backward

#####################################
# the final verdict: backward model #
#####################################

qqnorm(rstudent(model.backward.log)) ## error terms are not normal ??? ##
plot(fmly$vehq, rstudent(model.backward.log))
plot(fmly$fam_size, rstudent(model.backward.log))
plot(fmly$fincatax, rstudent(model.backward.log))
plot(fmly$renteqvx, rstudent(model.backward.log))
plot(fmly$inc_rank, rstudent(model.backward.log))
plot(fitted.values(model.backward.log), rstudent(model.backward.log)) ## residuals are not constant ?? ##

#############################################################################

###########################
# Check the leverage and  #
# and influential points, #
# to see if our model can #
# be improved futher      #
###########################

#################################
# Standard Residuals & Leverage #
#################################

################
##  outliners ##
################

standard_residuals <- resid(model.backward.log)/summary(model.backward.log)$sigma^2 
sort_standard_residuals <- c(sort(abs(standard_residuals)))
head(sort_standard_residuals)

fmly_delete_log_1 <- fmly[-c(         966   ,       956     ,     809   ,      3085     ,     338    ,     3310  ), ]
dim(fmly_delete_log_1)

######################
## leverage points  ##
######################

leverage_cut_off <- 2*length(model.backward.log$coefficients)/nrow(fmly)
h_ii <-lm.influence(model.backward.log)$hat
sort_leverage_cut_off <- c(sort(h_ii[h_ii>leverage_cut_off]))
head(sort_leverage_cut_off)

fmly_delete_log <- fmly_delete_log_1[-c(3543     ,  2286     ,  2348   ,    1582   ,    1478    ,   3434 ),]
dim(fmly_delete_log)

#####################################################
# Re-fit our final model based on "fmly_delete_log" #
#####################################################

summary(model.backward.log)

refit_final_log <- lm(formula = I(log(etotapx4 + 1)) ~ vehq + fam_size + fincatax + renteqvx + inc_rank + no_earnr + income.f + cuten.f + educ_ref.f + marital.f + region.f, data = fmly_delete_log)

summary(refit_final_log)
anova(refit_final_log)

qqnorm(rstudent(refit_final_log)) ## Too good to be true??? ###
abline(0,1)

length(rstudent(refit_final_log))
plot(fmly$vehq[1:3522], rstudent(refit_final_log))
plot(fmly$fam_size[1:3522], rstudent(refit_final_log))
plot(fmly$fincatax[1:3522], rstudent(refit_final_log))
plot(fmly$renteqvx[1:3522], rstudent(refit_final_log))
plot(fmly$inc_rank[1:3522], rstudent(refit_final_log))
plot(fitted.values(refit_final_log), rstudent(refit_final_log)) ### constant ?? ###

########################################################################
# Compare final models with and without outliners and leverage points  #
########################################################################


summary(model.backward.log)$sigma
summary(refit_final_log)$sigma

summary(model.backward.log)$adj.r.squared
summary(refit_final_log)$adj.r.squared

press_fit_final <- sum(( model.backward.log $residuals/(1-hatvalues(model.backward.log)))^2)
press_refit_final <- sum(( refit_final_log$residuals/(1-hatvalues(refit_final_log)))^2)

AIC(model.backward.log)
AIC(refit_final_log)

BIC(model.backward.log)
BIC(refit_final_log)

#################################################################################
## Verdict: after removing the 6 worst outliners and observations, our models  ##
## improves a little bit                                                       ##
#################################################################################


############################################################################
## The End !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ###
############################################################################





