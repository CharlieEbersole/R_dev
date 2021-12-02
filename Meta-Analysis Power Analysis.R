###########Meta-Analysis Power Analysis###############
#####Charlie Ebersole#####

#To calculate power, we will be using the following function

#Power for meta-analysis from https://osf.io/mbv9s/

es <- 0.1 # Enter your summary effect size
as <- 50  # Average per number per group
mk <- 10  # Number of effect sizes
hg <- 1   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
Power <- (1-pnorm(1.96-eq5)) # Two-tailed
Power

########Calculating power using different effect size benchmarks######
EffectSizes<-seq(0,.5, by = .01) #hypothetical effect sizes, in standardized mean differences

es <- EffectSizes # Enter your summary effect size
as <- 100  # Average per number per group (treatment vs. control)
mk <- 25  # Number of effect sizes
hg <- .33   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
PowerLow <- (1-pnorm(1.96-eq5)) # Two-tailed

es <- EffectSizes # Enter your summary effect size
as <- 100  # Average per number per group (treatment vs. control)
mk <- 25  # Number of effect sizes
hg <- 1   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
PowerModerate <- (1-pnorm(1.96-eq5)) # Two-tailed


es <- EffectSizes # Enter your summary effect size
as <- 100  # Average per number per group (treatment vs. control)
mk <- 25  # Number of effect sizes
hg <- 3   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
PowerHigh <- (1-pnorm(1.96-eq5)) # Two-tailed


###Recording power estimates at different heterogeneity levels###
Low<-PowerLow
Moderate<-PowerModerate
High<-PowerHigh

###Creating power figure###

PowerData<-as.data.frame(cbind(EffectSizes,Low,Moderate,High))
PowerData

require(reshape2)
PowerDataLong<-melt(PowerData,id.vars=c("EffectSizes"),measure.vars=c("Low","Moderate","High"))
str(PowerDataLong)
colnames(PowerDataLong)[colnames(PowerDataLong)=="variable"] <- "Heterogeneity"

require(ggplot2)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p = ggplot() + 
  geom_line(data = PowerDataLong, aes(x = EffectSizes, y = value, colour = Heterogeneity), size = 2) +
  geom_point() +
  theme(text = element_text(size=10)) +
  theme(panel.background = element_rect(fill = "White")) +
  theme(panel.grid = element_line(colour = "Gray")) +
  xlab('Standardized Mean Difference') +
  ylab('Power') +
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1))+
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette)+
  geom_hline(yintercept = .8, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0.092, linetype = "dotted", size = 1.5, colour = "#000000") +
  geom_vline(xintercept = 0.113, linetype = "dotted", size = 1.5, colour = "#E69F00") +
  geom_vline(xintercept = 0.159, linetype = "dotted", size = 1.5, colour = "#56B4E9") 
print(p)

####calculating needed effect size for a particular level of power####
es <- seq(0, 1, by = .001) # creates a vector of possible effect sizes to test
as <- 100  # Average per number per group
mk <- 25  # Number of effect sizes
hg <- .33   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)
desired <- .8 # Target level of power

for(i in 1:length(es)){
eq1 <- ((as+as)/((as)*(as))) + ((es[i]^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es[i]/sqrt(eq4))
Power <- (1-pnorm(1.96-eq5)) # Two-tailed
print(es[i])
if(Power >= desired){
  break
}
}
