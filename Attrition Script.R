###########Simulating Attrition Script#################
####Charlie Ebersole, Laura Michaelson, Fred Pampel, Last Update: 4-15-2020####

####Loading required packages####

###The first time you use this tool, run the following five lines###
install.packages("metafor")
install.packages("compute.es")
install.packages("psych")
install.packages("ggplot2")
install.packages("gridExtra")

###Everytime you use this script, load the following packages (run lines 10-14)###
require(metafor)
require(compute.es)
require(psych)
require(ggplot2)
require(gridExtra)

###############User Inputs#################

###What is the effect size you'd like to investigate? Input either a Cohen's d, a Pearson's correlation coefficient, or an Odds Ratio###
###Make sure only one effect size is not 0. Also make sure to run all three lines, regardless of which effect size you input###

CohensD <- 0
  
CorrelationCoefficient <- 0
  
OddsRatio <- 0

###What are your sample sizes for your observed data (that is, the number of participants who completed your outcome measure)?###
###Enter a number for your treatment condition and your control condition in the two lines below###
ObservedTreatment <- 
ObservedControl <- 

###What are your sample sizes for your missing data (that is, the number of participants who did NOT complete your outcome measure)?###
###Enter a number for your treatment condition and your control condition in the two lines below###
MissingTreatment <- 
MissingControl <- 
  
###Numbers check###
#The following line should return your total sample size (both observed and missing). If it does not, check your numbers, above#
ObservedTreatment + ObservedControl + MissingTreatment + MissingControl
  
####Now, run all of the code below. This will generate a figure showing the robustness of your findings at several levels of correlations between attrition and your outcome####
  
###The next line sets the number of simulation trials to run at each level of attrition bias###
ntrials <- 10000  

#The next lines indicate values for the correlation between your outcome and attrition

d <- abs(CohensD + r2d(CorrelationCoefficient) + OddsRatio*(sqrt(3)/pi))

rt <- c(-.4, -.35, -.3, -.25, -.2, -.15, -.1, -.05, 0)  #the correlation between the outcome and attrition in the treatment condition
rc <- c(0, .05, .1, .15, .2, .25, .3, .35, .4)  #the correlation between the outcome and attrition in the control condition

#setting up empty lists to save results to during simulations
p_value <- rep(1, ntrials)

set.seed(3)  #setting seed so simulation will be reproducible

########################################
###########RUNNING SIMULATION###########
########################################

#############First, for correlations with attrition in treatment condition################

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[1]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness1 <- num_sig_p/ntrials
Robustness1

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[2]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness2 <- num_sig_p/ntrials
Robustness2

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[3]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness3 <- num_sig_p/ntrials
Robustness3

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[4]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness4 <- num_sig_p/ntrials
Robustness4

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[5]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness5 <- num_sig_p/ntrials
Robustness5

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[6]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness6 <- num_sig_p/ntrials
Robustness6

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[7]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness7 <- num_sig_p/ntrials
Robustness7

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[8]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness8 <- num_sig_p/ntrials
Robustness8

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d + r2d(rt[9]), sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0, sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness9 <- num_sig_p/ntrials
Robustness9

#############Now, for correlations with attrition in control condition################

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[1]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness10 <- num_sig_p/ntrials
Robustness10

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[2]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness11 <- num_sig_p/ntrials
Robustness11

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[3]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness12 <- num_sig_p/ntrials
Robustness12

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[4]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness13 <- num_sig_p/ntrials
Robustness13

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[5]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness14 <- num_sig_p/ntrials
Robustness14

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[6]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness15 <- num_sig_p/ntrials
Robustness15

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[7]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness16 <- num_sig_p/ntrials
Robustness16

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[8]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness17 <- num_sig_p/ntrials
Robustness17

for (i in 1:ntrials){
  cond1obs <- rnorm(n = ObservedTreatment, mean = d, sd = 1) #creating data for observed treatment
  cond2obs <- rnorm(n = ObservedControl, mean = 0, sd = 1) #creating data for observed control
  cond1miss <- rnorm(n = MissingTreatment, mean = d, sd = 1) #creating data for missing treatment
  cond2miss <- rnorm(n = MissingControl, mean = 0 + r2d(rc[9]), sd = 1) #creating data for missing control
  data1<-c(cond1obs,cond1miss)
  data2<-c(cond2obs,cond2miss)
  t_results <- t.test(data1, data2, paired = F, var.equal = T, alternative = "greater", conf.level = .95) #running the t-test
  p_value[i] <- t_results$p.value #saving resulting p-value
}

num_sig_p <- sum(p_value <= .05) #calculating the number of significant results to check robustness
Robustness18 <- num_sig_p/ntrials
Robustness18

##############Graphing Results#####################

RobustRT<-c(Robustness1,Robustness2,Robustness3,Robustness4,Robustness5,Robustness6,Robustness7,Robustness8,Robustness9)
RobustData1<-as.data.frame(cbind(rt,RobustRT))

RobustRC<-c(Robustness10,Robustness11,Robustness12,Robustness13,Robustness14,Robustness15,Robustness16,Robustness17,Robustness18)
RobustData2<-as.data.frame(cbind(rc,RobustRC))

p = ggplot() + 
  geom_hline(yintercept = .95, linetype = "longdash", color = "grey", size = 2) +
  geom_hline(yintercept = .9, linetype = "dashed", color = "grey", size = 2) +
  geom_hline(yintercept = .8, linetype = "dotted", color = "grey", size = 2) +
  geom_line(data = RobustData1, aes(x = rt, y = RobustRT), size = 2, colour = "black") +
  geom_point(data = RobustData1, aes(x = rt, y = RobustRT), size = 4) +
  theme(text = element_text(size=20)) +
  xlab('Attrition-Outcome Correlation: Treatment') +
  ylab('Robustness') +
  ylim(0,1) +
  xlim(-.4,0) +
  theme_bw()
print(p)

q = ggplot() + 
  geom_hline(yintercept = .95, linetype = "longdash", color = "grey", size = 2) +
  geom_hline(yintercept = .9, linetype = "dashed", color = "grey", size = 2) +
  geom_hline(yintercept = .8, linetype = "dotted", color = "grey", size = 2) +
  geom_line(data = RobustData2, aes(x = rc, y = RobustRC), size = 2, colour = "black") +
  geom_point(data = RobustData2, aes(x = rc, y = RobustRC), size = 4) +
  theme(text = element_text(size=20)) +
  xlab('Attrition-Outcome Correlation: Control') +
  ylab('Robustness') +
  ylim(0,1) +
  xlim(0,.4)+
  theme_bw()
print(q)

grid.arrange(p, q, ncol=2)

