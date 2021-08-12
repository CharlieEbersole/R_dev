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
EffectSizes<-c(.05, .1, .15, .20, .25, .20, .35, .40) #hypothetical effect sizes, in standardized mean differences

es <- EffectSizes # Enter your summary effect size
as <- 100  # Average per number per group (protocol version)
mk <- 100  # Number of effect sizes
hg <- .33   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
PowerLow <- (1-pnorm(1.96-eq5)) # Two-tailed

es <- EffectSizes # Enter your summary effect size
as <- 100  # Average per number per group (protocol version)
mk <- 100  # Number of effect sizes
hg <- 1   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
PowerModerate <- (1-pnorm(1.96-eq5)) # Two-tailed


es <- EffectSizes # Enter your summary effect size
as <- 100  # Average per number per group (protocol version)
mk <- 100  # Number of effect sizes
hg <- 3   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
PowerHigh <- (1-pnorm(1.96-eq5)) # Two-tailed


###Recording power estimates at different heterogeneity levels###
LowHeterogeneity<-PowerLow
ModerateHeterogeneity<-PowerModerate
HighHeterogeneity<-PowerHigh

###Creating power figure###

PowerData<-as.data.frame(cbind(EffectSizes,LowHeterogeneity,ModerateHeterogeneity,HighHeterogeneity))
PowerData

require(reshape2)
PowerDataLong<-melt(PowerData,id.vars=c("EffectSizes"),measure.vars=c("LowHeterogeneity","ModerateHeterogeneity","HighHeterogeneity"))
str(PowerDataLong)
colnames(PowerDataLong)[colnames(PowerDataLong)=="variable"] <- "Heterogeneity"

require(ggplot2)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p = ggplot() + 
  geom_line(data = PowerDataLong, aes(x = EffectSizes, y = value, colour = Heterogeneity), size = 2) +
  geom_point() +
  theme(text = element_text(size=10)) +
  xlab('Effect Size (g)') +
  ylab('Power') +
  scale_fill_manual(values=cbbPalette) +
  scale_colour_manual(values=cbbPalette)
print(p)




